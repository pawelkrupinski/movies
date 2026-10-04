package integration

import tools.SpecTimeouts

import org.mongodb.scala.{Document, ObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cadence.{MongoRatingCadenceReader, MongoRatingCadenceStore, RatingCadenceReader}

import java.time.Instant
import scala.concurrent.Await

/**
 * The cadence page's whole-collection read against real MongoDB: an incomplete keyset scan must
 * not come back as the collection. It did — the reader dropped the scan's outcome and answered
 * with the rows it had (here none), so the page showed every film past the failure as never
 * refreshed. A row whose `_id` is not a string stops the scan where it stands, as a page that
 * still fails after its retries does.
 *
 * Runs in a database of its own, dropped in `afterAll`.
 */
class RatingCadenceReaderIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "rating-cadence-reader")
  private val reader   = new MongoRatingCadenceReader(Some(isolated.database))
  private lazy val collection = isolated.database.getCollection[Document](RatingCadenceReader.Collection)

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, SpecTimeouts.Io)

  "MongoRatingCadenceReader.all" should "read every stored row" in {
    val store = new MongoRatingCadenceStore(Some(isolated.database))
    store.record("imdb|tmdb:1", Some("7.1"), Instant.parse("2026-10-01T00:00:00Z"))
    _root_.tools.Eventually.eventually(reader.all().map(_._1) shouldBe Seq("imdb|tmdb:1"))
    store.close()
  }

  it should "throw, not answer part of the collection, when its scan stops short" in {
    await(collection.insertOne(Document("_id" -> 42, "backoffLevel" -> 0)).toFuture())
    an[Exception] should be thrownBy reader.all()
  }
}
