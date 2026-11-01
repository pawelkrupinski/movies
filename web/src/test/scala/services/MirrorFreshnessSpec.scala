package services

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

/**
 * The rule behind the `/debug` navbar's mirror-age badge. A stale mirror serves
 * a page that looks live, so the only thing standing between a wedged sync and
 * another day of chasing a phantom data bug is this number being on the page and
 * being right.
 */
class MirrorFreshnessSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val now = Instant.parse("2026-08-31T09:00:00Z")

  private def ageOf(secondsAgo: Long) =
    MirrorFreshness.describe(Some(now.minusSeconds(secondsAgo)), now)

  "mirror freshness" should "report nothing when nothing is mirrored" in {
    MirrorFreshness.describe(None, now) shouldBe None
    MirrorFreshness.notMirrored.newestUpdate() shouldBe None
  }

  it should "report a live mirror as fresh" in {
    val age = ageOf(12).value
    age.behind shouldBe 12.seconds
    age.stale shouldBe false
  }

  // THE case: the 2026-08-30 wedge froze the mirror a day back, and every page
  // still rendered as though it were live.
  it should "call a mirror that stopped syncing yesterday stale" in {
    val age = ageOf(26 * 3600).value
    age.label shouldBe "26h"
    age.stale shouldBe true
  }

  // The boundary is the re-seed gate's: a healthy sync re-seeds itself at 30
  // minutes of lag, so anything past that is a sync that is not working.
  it should "turn stale exactly where the re-seed gate does" in {
    ageOf(MirrorFreshness.StaleAfter.toSeconds - 1).value.stale shouldBe false
    ageOf(MirrorFreshness.StaleAfter.toSeconds).value.stale     shouldBe true
  }

  it should "stay legible from seconds to days" in {
    MirrorFreshness.label(0.seconds)   shouldBe "0s"
    MirrorFreshness.label(59.seconds)  shouldBe "59s"
    MirrorFreshness.label(90.seconds)  shouldBe "1m"
    MirrorFreshness.label(59.minutes)  shouldBe "59m"
    MirrorFreshness.label(90.minutes)  shouldBe "1h"
    MirrorFreshness.label(47.hours)    shouldBe "47h"
    MirrorFreshness.label(50.hours)    shouldBe "2d"
  }

  // A document stamped by a host whose clock runs fast reads as "-3s behind",
  // which is not evidence of anything — least of all of a mirror that is ahead
  // of prod.
  it should "not report a negative age" in {
    val age = MirrorFreshness.describe(Some(now.plusSeconds(30)), now).value
    age.behind shouldBe 0.seconds
    age.label  shouldBe "0s"
  }
}
