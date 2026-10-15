package services.facebook

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration.*

/** Facebook's X-App-Usage read, and acted on by holding the fleet's re-scrapes — the one rule the
 *  worker's re-scrapes and the web's Facebook sign-ins both report to. */
class FacebookUsageBrakeSpec extends AnyFlatSpec with Matchers {
  import FacebookUsageBrake.*

  private val Now = Instant.parse("2026-10-15T06:00:00Z")

  private final class Fixture {
    val held = collection.mutable.Buffer.empty[Instant]
    val brake = new FacebookUsageBrake(new FacebookSlots { def holdSlots(until: Instant): Unit = held += until }, Clock.fixed(Now, ZoneOffset.UTC))
  }

  // Facebook's documented shape (developers.facebook.com, "Rate Limiting": application-level). Not
  // recorded: a request without the app's token, which is all a spec run can make, carries no
  // usage header (checked 2026-10-14).
  "Facebook's X-App-Usage" should "be read as the highest of its percentages" in {
    AppUsage.parse("""{"call_count":28,"total_time":91,"total_cputime":25}""") shouldBe Some(AppUsage(91))
    AppUsage.parse("""{"call_count":100}""") shouldBe Some(AppUsage(100))
  }

  it should "be no usage at all when it names no percentage" in {
    AppUsage.parse("") shouldBe None
    AppUsage.parse("{}") shouldBe None
    AppUsage.parse("""["call_count"]""") shouldBe None
  }

  "A reading under the ceiling" should "hold nothing" in {
    val f = new Fixture
    f.brake.observe(AppUsage(UsageCeiling - 1)) shouldBe None
    f.held shouldBe empty
  }

  // 2026-10-15: each send spent ~1.5% of the allowance, and the first hour's calls had not aged out
  // yet, so every resume after a flat five-minute hold added one more: 91% → 96% → 99%.
  "A reading at the ceiling" should "hold the fleet's re-scrapes, twice as long for every point over, up to Facebook's hour" in {
    (0 to 6).map(over => usageHold(UsageCeiling + over)) shouldBe
      Seq(5.minutes, 10.minutes, 20.minutes, 40.minutes, 1.hour, 1.hour, 1.hour)
    val f = new Fixture
    f.brake.observe(AppUsage(UsageCeiling + 2)) shouldBe Some(Now.plusSeconds(20 * 60))
    f.held shouldBe Seq(Now.plusSeconds(20 * 60))
  }
}
