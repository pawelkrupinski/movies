package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.{DerivationProgress, DerivationVersion, MongoReadModelDerivationMarker}
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
      marker.record(DerivationVersion("first"))
      marker.record(DerivationVersion("second"))
      marker.recorded().get shouldBe Some(DerivationVersion("second"))
      new MongoReadModelDerivationMarker(Some(db), clock).recorded().get shouldBe Some(DerivationVersion("second"))
      withClue("one marker document, replaced in place: ") {
        Await.result(db.getCollection(MongoReadModelDerivationMarker.Collection).countDocuments().toFuture(), 10.seconds) shouldBe 1L
      }
    }
  }

  // What lets a pass survive the restart a deploy brings: a new process reads how far the last got.
  it should "keep a pass's progress beside the version, readable by the next process, without touching the version" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "derivation-progress") { db =>
      val clock  = Clock.fixed(Instant.parse("2026-09-26T00:00:00Z"), ZoneOffset.UTC)
      val marker = new MongoReadModelDerivationMarker(Some(db), clock)
      marker.progress().get shouldBe None
      marker.record(DerivationVersion("old"))
      marker.recordProgress(DerivationProgress(DerivationVersion("new"), 17))
      marker.recordProgress(DerivationProgress(DerivationVersion("new"), 18))
      val next = new MongoReadModelDerivationMarker(Some(db), clock)
      next.progress().get shouldBe Some(DerivationProgress(DerivationVersion("new"), 18))
      next.recorded().get shouldBe Some(DerivationVersion("old"))
    }
  }
}
