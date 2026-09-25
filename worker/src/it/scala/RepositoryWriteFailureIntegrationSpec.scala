package services.movies

import models.{CinemaMovie, Movie, Multikino, Showtime}
import org.mongodb.scala.{Document, SingleObservableFuture}
import org.mongodb.scala.model.{CreateCollectionOptions, Filters, ValidationOptions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.Env

import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The 2026-09-24 incident against REAL Mongo: a `movies` write that the server refuses must
 * be counted, rolled out of the cache, and retried by the next identical scrape — which then
 * lands it once the refusal clears.
 *
 * On the day, a codec bug made the upserts throw for ~6h; `MongoMovieRepository` logged a
 * WARN and returned `Unit`, the cache kept the unwritten row, and every later identical
 * scrape diffed as a no-op. Here the refusal is a collection validator no film document can
 * satisfy, toggled off with `collMod` — so the client, database and repository all stay
 * healthy and only the write fails, exactly as they did.
 *
 * In `services.movies` for the cache's package-private reads (`keyOf`, `get`).
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class RepositoryWriteFailureIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val isolatedDb = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "repository-write-failure-spec")

  private val db = isolatedDb.database
  private def await[T](f: scala.concurrent.Future[T]): T = Await.result(f, 30.seconds)

  await(db.createCollection(MovieRepository.Collection, CreateCollectionOptions().validationOptions(
    ValidationOptions().validator(Filters.exists("__no_movies_write_may_satisfy_this__")))).toFuture())

  private def acceptWrites(): Unit = {
    await(db.runCommand(Document("collMod" -> MovieRepository.Collection, "validator" -> Document())).toFuture()); ()
  }

  private class Recorder extends RepositoryWriteMetrics {
    @volatile var failures: Vector[(String, String, String)] = Vector.empty
    def recordWriteFailed(collection: String, op: String, exception: String): Unit =
      failures :+= ((collection, op, exception))
  }
  private val Recording = new Recorder

  private val screenings = new MongoScreeningsRepository(Some(db), writeMetrics = Recording)
  private val slots      = new MongoSlotsRepository(Some(db), writeMetrics = Recording)
  private val repository = new MongoMovieRepository(Some(db), normalizer = titleNormalizer,
    screenings = Some(screenings), slots = Some(slots), writeMetrics = Recording)

  override protected def afterAll(): Unit = {
    repository.close()
    isolatedDb.drop()
    super.afterAll()
  }

  private val title   = "__repository-write-failure-sentinel__"
  private val listing = CinemaMovie(movie = Movie(title, releaseYear = Some(2026)), cinema = Multikino,
    posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
    showtimes = Seq(Showtime(LocalDateTime.now().plusDays(1).withHour(20).withMinute(0).withSecond(0).withNano(0), None)))

  private def moviesDocuments: Long =
    await(db.getCollection(MovieRepository.Collection).countDocuments().toFuture())

  "a new film whose movies write Mongo refuses" should "be counted, leave the cache, and land on the next identical scrape" in {
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val key   = cache.keyOf(title, Some(2026))

    cache.recordCinemaScrape(Multikino, Seq(listing))

    moviesDocuments shouldBe 0L
    Recording.failures shouldBe Vector((MovieRepository.Collection, "upsert", "MongoWriteException"))
    withClue("the row Mongo refused must not stay resident — the next scrape would diff it as a no-op: ")(
      cache.get(key) shouldBe None)

    acceptWrites()
    cache.recordCinemaScrape(Multikino, Seq(listing))

    withClue("the identical re-scrape must retry the write: ")(moviesDocuments shouldBe 1L)
    repository.findAll().map(_.title) shouldBe Seq(title)
    Recording.failures should have size 1
  }

  // The side collections' bulk deletes (the stranded-row and retired-venue sweeps) answered a
  // failure with "0 rows" and a WARN, and nothing counted it. A VIEW under the collection's
  // name is a namespace Mongo refuses to delete from, with everything else healthy.
  "a side collection's bulk delete that Mongo refuses" should "be counted, and report no rows removed" in {
    val isolatedViewDb = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "repository-delete-failure-spec")
    val viewDb = isolatedViewDb.database
    try {
      await(viewDb.createCollection("backing").toFuture())
      await(viewDb.createView(SlotsRepository.Collection, "backing", Seq.empty).toFuture())
      await(viewDb.createView(ScreeningsRepository.Collection, "backing", Seq.empty).toFuture())
      val metrics = new Recorder
      val viewSlots      = new MongoSlotsRepository(Some(viewDb), writeMetrics = metrics)
      val viewScreenings = new MongoScreeningsRepository(Some(viewDb), writeMetrics = metrics)

      viewSlots.deleteRows(Set("__repository-delete-failure-sentinel__␟Multikino")) shouldBe 0L
      viewSlots.deleteFilms(Set("__repository-delete-failure-sentinel__")) shouldBe 0L
      viewScreenings.deleteRows(Set("__repository-delete-failure-sentinel__␟Multikino")) shouldBe 0L
      viewScreenings.deleteFilms(Set("__repository-delete-failure-sentinel__")) shouldBe 0L

      metrics.failures.map(f => f._1 -> f._2) shouldBe Vector(
        SlotsRepository.Collection -> "deleteRows", SlotsRepository.Collection -> "deleteFilms",
        ScreeningsRepository.Collection -> "deleteRows", ScreeningsRepository.Collection -> "deleteFilms")
    } finally isolatedViewDb.drop()
  }
}
