package integration

import models.{CinemaShowing, KinoPionier, MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository}
import tools.{Env, ProdCoverage}

import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * That the production baseline counts a film whichever of the TWO homes its cinema
 * slots live in.
 *
 * `movies.sourceData` was split out into the `movie_slots` side collection, and the
 * split is still in progress: 259 of production's 940 rows carried their slots
 * embedded on the day this was written. The baseline read only the side collection,
 * so every one of those films was invisible to it — the guard was comparing the
 * replay's whole corpus against a fraction of production's.
 *
 * The cost was a leg that failed for a reason nobody could act on. Poland's sample
 * leg reported `films run=94 prod=73 — off by 28.8%`, which reads as the pipeline
 * minting 21 films production does not have. It was not: unioning the embedded slots
 * back in brought production's own count to 90 on the same repertoire, which is 4.4%
 * — the same figure the leg had reported while passing the day before. The size of
 * the error depends only on how many of the sampled films happen to be mid-migration,
 * so it moves with every re-record and cannot be read off the number.
 *
 * Matched on the SLOT KEY (`"<cinema>␟<titleKey>"`, `CinemaShowing.keyFor`) rather
 * than on the slot's stored title, because the key is the one identity both homes
 * share — the embedded map is KEYED by it — and because a stored title is not
 * comparable to the corpus's: production strips a listing's decoration before storing
 * it ("The Room [dubbing]" is stored as "The Room"), so exact-title matching missed
 * those too.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ProdCoverageIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val database = MongoClient(Env.get("MONGODB_URI").get).getDatabase(s"prod_coverage_spec_${System.nanoTime()}")

  private val movies     = new MongoMovieRepository(sharedDb = Some(database), normalizer = titleNormalizer)
  private val slots      = new MongoSlotsRepository(Some(database))
  private val screenings = new MongoScreeningsRepository(Some(database))

  /** Two films, identical in every way that matters except WHERE their cinema slot is
   *  stored: one migrated to `movie_slots`, one still embedded on the row. */
  private val sideKey     = CinemaShowing.keyFor(KinoPionier, "Ktoś całkiem obcy", titleNormalizer)
  private val embeddedKey = CinemaShowing.keyFor(KinoPionier, "Rambo: Pierwsza krew", titleNormalizer)

  override def beforeAll(): Unit = {
    val soon = LocalDateTime.ofInstant(Instant.now().plusSeconds(3600), ZoneOffset.UTC)

    // The migrated film: row carries no slots, they live in `movie_slots`.
    movies.upsert("side", Some(2024), MovieRecord(tmdbId = Some(1), data = Map.empty))
    slots.upsertSlot("side|2024", sideKey.displayName, SourceData(title = Some("Ktoś całkiem obcy")))

    // The un-migrated film: slots still embedded, nothing in `movie_slots`.
    movies.upsert("embedded", Some(1982), MovieRecord(
      tmdbId = Some(2),
      data   = Map[Source, SourceData](embeddedKey -> SourceData(title = Some("Rambo: Pierwsza krew")))))

    // Both are SCREENING — the baseline counts only films with a future showtime.
    Seq("side|2024" -> sideKey, "embedded|1982" -> embeddedKey).foreach { case (filmId, key) =>
      screenings.upsertSlot(filmId, key.displayName, Seq(Showtime(dateTime = soon, bookingUrl = None)))
    }
  }

  override def afterAll(): Unit = {
    Await.result(database.drop().toFuture(), 1.minute)
    super.afterAll()
  }

  it should "count a film whose slots are still embedded, not only the migrated ones" in {
    val keys = Set(sideKey.displayName, embeddedKey.displayName)

    val baseline = ProdCoverage.of(database, onlySlotKeys = Some(keys))

    withClue("a film mid-migration is still one of production's films: ") {
      baseline.films shouldBe 2
    }
    baseline.tmdbId shouldBe 2
  }
}
