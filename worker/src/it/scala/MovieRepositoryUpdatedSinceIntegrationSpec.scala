package services.movies

import tools.SpecTimeouts

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.{Document, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.Instant
import scala.concurrent.Await

/** The bounded catch-up read for a silent change stream: every `movies` row whose
 *  `updatedAt` is after an instant, through the same stitched path every other scan
 *  takes, served by an index rather than a collection scan. `updatedAt` has been bumped on
 *  every write since the collection existed and was indexed by nothing.
 *
 *  The clock never moves here: the floor and the stamps come from one strictly increasing
 *  sequence, so a row written in the millisecond a catch-up began is still after its floor.
 *  Stamped with bare clock readings, floor and row were equal and `updatedAt > floor` skipped
 *  the row for good. */
class MovieRepositoryUpdatedSinceIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  // Never moved: every write below lands in the SAME millisecond, the case a bare clock reading
  // could not order (a catch-up floor equal to the stamp of a row written after it).
  private val clock = new tools.MutableClock(Instant.parse("2026-09-07T12:00:00Z"))
  private val when  = java.time.LocalDateTime.of(2026, 9, 9, 18, 0)

  private def row(title: String, tmdbId: Int): MovieRecord =
    MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some(title), releaseYear = Some(2026), showtimes = Seq(Showtime(when, None)))))

  "the movies collection" should "index updatedAt and scan the rows written after an instant, stitched" in
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "updated-since") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      val repository = new MongoMovieRepository(Some(db), clock, screenings = Some(screenings), slots = Some(slots),
        normalizer = titleNormalizer)
      try {
        repository.enabled shouldBe true
        val indexes = Await.result(db.getCollection[Document]("movies").listIndexes().toFuture(), SpecTimeouts.Io)
        withClue(s"indexes: ${indexes.map(_.toJson())}\n") {
          indexes.exists(i => i.get("key").exists(_.asDocument().containsKey("updatedAt")) &&
                              !i.get("unique").exists(_.asBoolean().getValue)) shouldBe true
        }

        // A catch-up's floor is the repository's own liveness stamp — what the read-model sweep reads from.
        val liveness = repository.changeStreamLiveness
        repository.upsert("__updated-since-first__", Some(2026), row("__updated-since-first__", 7001))
        val since = liveness.now()
        repository.upsert("__updated-since-second__", Some(2026), row("__updated-since-second__", 7002))

        def updatedSince(at: Instant): Seq[StoredMovieRecord] = {
          val rows = Seq.newBuilder[StoredMovieRecord]
          repository.foreachRecordUpdatedSince(at)(rows += _) shouldBe tools.ScanOutcome.Complete
          rows.result()
        }
        updatedSince(since).map(_.title) shouldBe Seq("__updated-since-second__")
        updatedSince(liveness.now())     shouldBe empty
        // Stitched: the showtimes come back from `screenings`, the slot from `movie_slots`.
        updatedSince(since).head.record.data.values.flatMap(_.showtimes).toSeq shouldBe Seq(Showtime(when, None))

        // A later write to the first row is what the catch-up exists to find.
        val later = liveness.now()
        repository.upsert("__updated-since-first__", Some(2026), row("__updated-since-first__", 7001).copy(imdbRating = Some(7.5)))
        updatedSince(later).map(_.title) shouldBe Seq("__updated-since-first__")
      } finally repository.close()
    }
}
