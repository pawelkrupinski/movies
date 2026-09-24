package services.movies

import org.mongodb.scala.{Document, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.readmodel.DecodeFailureMetrics
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/** A `movies` document the codec cannot decode fails every read that meets it: the point read
 *  answers "unreadable", and the keyset corpus scan — every page retried, then the whole scan
 *  declared incomplete — fails for the whole corpus. Both used to leave only a WARN line, so the
 *  one bad document behind a cold mirror or a cursor held for good went unnamed. They count on
 *  the same `decode_failures` counter as the read model's skipped documents. */
class MovieRepositoryDecodeFailureIntegrationSpec extends AnyFlatSpec with Matchers {
  private val uri = Env.get("MONGODB_URI").get

  "the movies repository" should "count a document it cannot decode, on a point read and on the corpus scan" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "movies-decode-failure") { db =>
      val counted    = scala.collection.mutable.ListBuffer.empty[String]
      val metrics: DecodeFailureMetrics = (collection: String) => { counted += collection; () }
      val repository = new MongoMovieRepository(Some(db), normalizer = titleNormalizer,
        foreachRecordBatchAttempts = 1, decodeFailures = metrics)
      repository.enabled shouldBe true
      // `sourceData` must be a document; a string cannot be decoded into one.
      Await.result(db.getCollection[Document]("movies").insertOne(
        Document("_id" -> "__undecodable__", "key" -> "undecodable|2026", "title" -> "Undecodable",
          "sourceData" -> "not a document")).toFuture(), 10.seconds)

      repository.findByIdChecked(FilmId("__undecodable__")) shouldBe ((None, false))
      counted.toSeq shouldBe Seq("movies")
      repository.findAllChecked()._2 shouldBe false
      counted.toSeq shouldBe Seq("movies", "movies")
    }
}
