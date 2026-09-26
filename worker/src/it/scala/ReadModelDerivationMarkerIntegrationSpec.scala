package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.MongoReadModelDerivationMarker
import tools.IsolatedMongoDatabase

import org.mongodb.scala.SingleObservableFuture

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.Await
import scala.concurrent.duration.*

/** The derivation marker's round trip through a real Mongo: a fresh database has none recorded
 *  (which owes a pass), and a recorded version reads back — replaced, not added to, on the next. */
class ReadModelDerivationMarkerIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "the Mongo derivation marker" should "read nothing from a fresh database and read back what was last recorded" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "derivation-marker") { db =>
      val clock  = Clock.fixed(Instant.parse("2026-09-25T00:00:00Z"), ZoneOffset.UTC)
      val marker = new MongoReadModelDerivationMarker(Some(db), clock)
      marker.recorded().get shouldBe None
      marker.record("first")
      marker.record("second")
      marker.recorded().get shouldBe Some("second")
      new MongoReadModelDerivationMarker(Some(db), clock).recorded().get shouldBe Some("second")
      withClue("one marker document, replaced in place: ") {
        Await.result(db.getCollection(MongoReadModelDerivationMarker.Collection).countDocuments().toFuture(), 10.seconds) shouldBe 1L
      }
    }
  }
}
