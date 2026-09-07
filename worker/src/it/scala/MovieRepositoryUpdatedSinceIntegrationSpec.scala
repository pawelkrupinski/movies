package services.movies

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.{Document, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/** The bounded catch-up read for a silent change stream: every `movies` row whose
 *  `updatedAt` is after an instant, through the same stitched path every other scan
 *  takes, served by an index rather than a collection scan. `updatedAt` has been bumped on
 *  every write since the collection existed and was indexed by nothing. */
class MovieRepositoryUpdatedSinceIntegrationSpec extends AnyFlatSpec with Matchers {
  private val uri  = Env.get("MONGODB_URI").get
  private val when = java.time.LocalDateTime.now().plusDays(2).withHour(18).withMinute(0).withSecond(0).withNano(0)

  private def row(title: String, tmdbId: Int): MovieRecord =
    MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some(title), releaseYear = Some(2026), showtimes = Seq(Showtime(when, None)))))

  "the movies collection" should "index updatedAt and scan the rows written after an instant, stitched" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "updated-since") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings), slots = Some(slots),
        normalizer = titleNormalizer)
      try {
        repository.enabled shouldBe true
        val indexes = Await.result(db.getCollection[Document]("movies").listIndexes().toFuture(), 10.seconds)
        withClue(s"indexes: ${indexes.map(_.toJson())}\n") {
          indexes.exists(i => i.get("key").exists(_.asDocument().containsKey("updatedAt")) &&
                              !i.get("unique").exists(_.asBoolean().getValue)) shouldBe true
        }

        repository.upsert("__updated-since-first__", Some(2026), row("__updated-since-first__", 7001))
        Thread.sleep(20)
        val since = Instant.now()
        Thread.sleep(20)
        repository.upsert("__updated-since-second__", Some(2026), row("__updated-since-second__", 7002))

        def updatedSince(at: Instant): Seq[StoredMovieRecord] = {
          val rows = Seq.newBuilder[StoredMovieRecord]
          repository.foreachRecordUpdatedSince(at)(rows += _) shouldBe true
          rows.result()
        }
        updatedSince(since).map(_.title) shouldBe Seq("__updated-since-second__")
        updatedSince(Instant.now())      shouldBe empty
        // Stitched: the showtimes come back from `screenings`, the slot from `movie_slots`.
        updatedSince(since).head.record.data.values.flatMap(_.showtimes).toSeq shouldBe Seq(Showtime(when, None))

        // A later write to the first row is what the catch-up exists to find.
        Thread.sleep(20)
        val later = Instant.now()
        Thread.sleep(20)
        repository.upsert("__updated-since-first__", Some(2026), row("__updated-since-first__", 7001).copy(imdbRating = Some(7.5)))
        updatedSince(later).map(_.title) shouldBe Seq("__updated-since-first__")
      } finally repository.close()
    }
}
