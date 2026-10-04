package integration

import tools.SpecTimeouts

import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, MongoFreshnessStore}
import tools.{Eventually, IntegrationCorpusDatabase, IntegrationMongoSuite}

import java.time.Instant
import scala.concurrent.Await

/** A Mongo store's retention rows: the scan names only what was written before the cutoff, and a delete
 *  lands only on a row still carrying the stamp the scan read — in Mongo and in the store's mirror. */
class MongoStampedRowsIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite {
  private val client = MongoClient(MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
    .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).build())
  private val database: MongoDatabase = client.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "stamped-rows"))

  private val old   = Instant.parse("2026-08-01T00:00:00Z")
  private val fresh = Instant.parse("2026-10-01T00:00:00Z")
  private val cut   = Instant.parse("2026-09-01T00:00:00Z")

  "a Mongo freshness store's retention" should "scan by stamp and delete only what is unchanged since the scan" in {
    Await.result(database.getCollection("freshness").drop().toFuture(), SpecTimeouts.Io)
    val store = new MongoFreshnessStore(Some(database))
    try {
      Await.result(store.whenReady(FreshnessKind.ImdbRating), SpecTimeouts.Io)
      store.markFresh("imdb|tmdb:1", FreshnessKind.ImdbRating, old)
      store.markFresh("imdb|tmdb:2", FreshnessKind.ImdbRating, old)
      store.markFresh("imdb|tmdb:3", FreshnessKind.ImdbRating, fresh)
      Eventually.eventually(store.retention.stampedBefore(cut).map(_._1).toSet shouldBe Set("imdb|tmdb:1", "imdb|tmdb:2"))
      val scanned = store.retention.stampedBefore(cut)
      store.markFresh("imdb|tmdb:2", FreshnessKind.ImdbRating, fresh)            // written again after the scan
      Eventually.eventually(store.retention.stampedBefore(cut).map(_._1) shouldBe Seq("imdb|tmdb:1"))
      store.retention.deleteIfStill(scanned) shouldBe 1
      store.lastFetchedAt("imdb|tmdb:1") shouldBe None
      store.lastFetchedAt("imdb|tmdb:2") shouldBe Some(fresh)
      // `markFresh` writes without waiting: "imdb|tmdb:3", never scanned (it is fresh), may land after the delete
      Eventually.eventually(Await.result(database.getCollection("freshness").countDocuments().toFuture(), SpecTimeouts.Io) shouldBe 2L)
    } finally {
      Await.result(database.drop().toFuture(), SpecTimeouts.Io)
      client.close()
    }
  }
}
