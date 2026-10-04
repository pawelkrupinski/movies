package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{IntegrationMongoSuite, IsolatedMongoDatabase, MongoFleetHostPace}

import java.time.Instant
import scala.concurrent.duration._

/** The fleet's host paces in Mongo: every worker's take moves the one next slot, and a slot past the horizon is handed
 *  back untaken — what `InMemoryFleetHostPace` does in one process, here across processes. */
class MongoFleetHostPaceIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite with BeforeAndAfterAll {
  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "fleet_pace")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  "the fleet's pace" should "hand each worker the next slot in turn, and hand a slot past the horizon back untaken" in {
    val (pl, uk) = (new MongoFleetHostPace(isolated.database), new MongoFleetHostPace(isolated.database))
    val at = Instant.parse("2026-10-04T21:00:00Z")
    pl.take("www.wikidata.org", 500.millis, 1.second, at) shouldBe Right(at)
    uk.take("www.wikidata.org", 500.millis, 1.second, at) shouldBe Right(at.plusMillis(500))
    pl.take("www.wikidata.org", 500.millis, 1.second, at) shouldBe Right(at.plusMillis(1000))
    uk.take("www.wikidata.org", 500.millis, 1.second, at) shouldBe Left(at.plusMillis(1500))   // past the horizon: not taken
    pl.take("www.wikidata.org", 500.millis, 1.second, at.plusMillis(1500)) shouldBe Right(at.plusMillis(1500))
    pl.take("other.example", 500.millis, 1.second, at) shouldBe Right(at)                       // a host's own pace
  }
}
