package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{KinoMuranow, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * An `upsert` that changes nothing about a film must not rewrite the film's `movies`
 * document.
 *
 * `upsert` is the whole-record path every scrape merge takes, so each of a film's venues
 * wrote this document once per tick regardless of whether anything had changed — and Mongo
 * does not collapse that. A byte-identical `replaceOne` reports `modifiedCount: 1` and
 * writes a full oplog entry, which the change stream delivers, which re-decodes the film
 * document and re-dispatches it downstream. Measured on the steady-state shape (one film,
 * every one of its venues re-scraping it, nothing moved): 16 oplog entries for a German
 * film, 53 for an American one, all of them no-ops.
 *
 * The sibling guards on the same method — slots, then screenings — already covered their
 * collections, which is why those measured zero while `movies` did not.
 *
 * WHY THE PROFILER. There is no seam to count on: the `movies` write is issued directly on
 * the collection inside the repository, unlike `screenings`/`slots` which arrive as
 * injected collaborators. `StagingSiblingProjectionIntegrationSpec` reads the profiler for
 * the same reason, and takes its own database because profiling is database-wide — with
 * suites running concurrently the counts otherwise belong to whoever else was querying.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class MoviesWriteSkippedWhenUnchangedIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val db = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "movies-write-skip-spec")

  override protected def afterAll(): Unit = {
    tools.IsolatedMongoDatabase.drop(db)
    super.afterAll()
  }

  private val repository = new MongoMovieRepository(Some(db),
    screenings = Some(new MongoScreeningsRepository(Some(db))),
    slots      = Some(new MongoSlotsRepository(Some(db))),
    normalizer = titleNormalizer)

  private val title = "__movies-write-skip-sentinel__"
  private val year  = Some(2026)
  private val when  =
    java.time.LocalDateTime.now().plusDays(3).withHour(20).withMinute(0).withSecond(0).withNano(0)

  private def recordShowing(at: java.time.LocalDateTime): MovieRecord =
    MovieRecord(tmdbId = Some(4242), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some(title), showtimes = Seq(Showtime(at, None))),
      KinoMuranow -> SourceData(title = Some(title), showtimes = Seq(Showtime(at.plusHours(2), None)))))

  /** `movies` writes issued while `body` runs. `replaceOne` reaches the profiler as an
   *  `update` op; the namespace filter keeps `screenings` / `movie_slots` out of it. */
  private def moviesWritesDuring(body: => Unit): Int = {
    Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)
    Await.result(db.getCollection[Document]("system.profile").drop().toFuture(), 30.seconds)
    Await.result(db.runCommand(Document("profile" -> 2)).toFuture(), 30.seconds)
    try body
    finally Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)
    Await.result(
      db.getCollection[Document]("system.profile")
        .find(Filters.and(Filters.eq("op", "update"), Filters.regex("ns", "movies$")))
        .toFuture(), 30.seconds).size
  }

  "an upsert that changes nothing" should "not rewrite the film's movies document" in {
    val id     = StoredMovieRecord.idFor(title, year, titleNormalizer)
    val record = recordShowing(when)
    try {
      // The film does not exist yet, so this one has to write it.
      withClue("the first write must actually create the film document: ") {
        moviesWritesDuring(repository.upsert(title, year, record)) should be > 0
      }

      // The SAME record again — what a scrape tick does to a film nothing has changed
      // about. Every venue of the film takes this path, once each, every tick.
      withClue("a re-merge that changes nothing must not rewrite the document — each " +
               "rewrite is an oplog entry, a change-stream delivery and a re-decode of " +
               "the film: ") {
        moviesWritesDuring(repository.upsert(title, year, record)) shouldBe 0
      }

      // A SHOWTIME change writes nothing here either — and that is correct, not the guard
      // over-skipping. Under the read-split the `movies` document carries no showtimes and,
      // once its slots have landed, no `sourceData`; the change belongs to `screenings`.
      // Asserted rather than assumed, because it is the reason nearly every `movies` write
      // the scrape path made was redundant: the collection holds none of what a scrape
      // usually changes.
      val moved = recordShowing(when.plusDays(1))
      withClue("a showtimes-only change belongs to `screenings`, not `movies`: ") {
        moviesWritesDuring(repository.upsert(title, year, moved)) shouldBe 0
      }
      repository.findByIdChecked(id)._1.map(_.record.data.values.flatMap(_.showtimes).map(_.dateTime).toSet)
        .getOrElse(Set.empty) shouldBe Set(when.plusDays(1), when.plusDays(1).plusHours(2))

      // …and the guard must not over-skip. A change to a field `movies` DOES own still
      // writes, and is stored — otherwise "skip when unchanged" would be indistinguishable
      // from "never write again".
      val rated = moved.copy(imdbRating = Some(7.4))
      withClue("a change to a field the movies document owns must still be written: ") {
        moviesWritesDuring(repository.upsert(title, year, rated)) should be > 0
      }
      repository.findByIdChecked(id)._1.flatMap(_.record.imdbRating) shouldBe Some(7.4)
    } finally repository.delete(title, year)
  }
}
