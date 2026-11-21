package services.movies

import models.{CacheKey, FilmId, Multikino, MovieRecord, Source, SourceData}
import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * [[WholeRecordUpsert]] when its duplicate-key pre-check cannot be read: `movies` unreachable (port 1 refuses at
 * once), `movie_slots` in memory and writable. The pre-check exists so a film never lands its slots and then has its
 * `movies` write refused (the 2026-09-16 strand); a pre-check that failed was read as "no collision" and the slots
 * went first all the same.
 */
class WholeRecordUpsertUnreachableSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private val client = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")

  override protected def afterAll(): Unit = try client.close() finally super.afterAll()

  "upsert" should "report the failure and write no slot when the collision pre-check cannot be read" in {
    val slots = new InMemorySlotsRepository(_root_.tools.SpecClock.Pinned)
    val repository = new MongoMovieRepository(
      sharedDb   = Some(client.getDatabase("unreachable")),
      clock      = _root_.tools.SpecClock.Pinned,
      stopwatch  = new _root_.tools.Stopwatch(() => 0L),
      slots      = Some(slots),
      normalizer = titleNormalizer)
    val record = MovieRecord(tmdbId = Some(42),
      data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Diuna"))))

    val outcome = repository.upsert(FilmId("diuna|2021"), CacheKey("Diuna", Some(2021), titleNormalizer), record)
    outcome should matchPattern { case WriteOutcome.Failed(MovieRepository.Collection, "upsert", _) => }
    slots.findForFilm("diuna|2021") shouldBe empty
  }
}
