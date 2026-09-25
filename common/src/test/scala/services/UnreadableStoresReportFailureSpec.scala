package services

import models.Multikino
import org.mongodb.scala.{Document, MongoClient}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.attempts.MongoEnrichmentAttemptReader
import services.cadence.MongoRatingCadenceReader
import services.scrapes.MongoScrapeGuardLedger

/**
 * Four stores that used to answer a failed Mongo read with the value an empty-but-healthy
 * store gives — the sites `NoSwallowedFailureSpec` carried as KNOWN. Each now says it could
 * not look. Port 1 refuses instantly and 200ms of server selection stands in for any driver
 * error, as `MongoTaskQueueUnreachableSpec` does.
 */
class UnreadableStoresReportFailureSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")
  private val db     = client.getDatabase("unreachable")

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  // "Fresh" was then written back over the stored rejection count (RewiredVenueGuardSpec).
  "MongoScrapeGuardLedger.get" should "answer None, not a fresh venue, when the ledger cannot be read" in {
    new MongoScrapeGuardLedger(Some(db)).get(Multikino) shouldBe None
  }

  "MongoEnrichmentAttemptReader.forKeys" should "throw, not report a film as never attempted" in {
    an[Exception] should be thrownBy new MongoEnrichmentAttemptReader(Some(db)).forKeys(Seq("imdb|tmdb:1"))
  }

  "MongoRatingCadenceReader.forKeys" should "throw, not report a film as having no cadence history" in {
    an[Exception] should be thrownBy new MongoRatingCadenceReader(Some(db)).forKeys(Seq("imdb|tmdb:1"))
  }

  // Taken as "no index", the reconcile went on to a create and RECORDED a mismatch — an alert
  // about an index it never saw.
  "MongoTtlIndex.reconcile" should "record no mismatch for an index it could not read" in {
    val collection = db.getCollection[Document]("unreadable_ttl")
    val mismatches = new TtlIndexMismatches
    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec", mismatches)
    mismatches.names should not contain collection.namespace.getFullName
  }
}
