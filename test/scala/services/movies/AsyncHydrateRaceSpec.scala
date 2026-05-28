package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus

import java.time.LocalDateTime
import java.util.concurrent.CountDownLatch
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Reproduces the prod duplicate-row bug introduced when MovieCache.rehydrate()
 * was moved off the constructor thread to a daemon (Wiring's `asyncHydrate =
 * true`). When the first cinema scrape lands before hydrate finishes:
 *
 *   - Mongo has the canonical row keyed `("Mandalorian i Grogu", Some(2026))`
 *   - Cache is empty (hydrate still in flight)
 *   - `redirectToExistingVariant` can't see the Mongo row, returns None
 *   - Scrape creates a fresh row at `("Mandalorian i Grogu", None)` — the
 *     cinema feed didn't carry releaseYear this tick
 *   - Hydrate completes and adds the Mongo row at its key
 *   - Cache now has **two rows** for the same film at different (title, year)
 *     keys — the page renders both, the user sees duplicate cards
 *
 * Fix: scrapes wait for hydration to complete before consulting the cache, so
 * the redirect lookup sees the full Mongo state.
 */
class AsyncHydrateRaceSpec extends AnyFlatSpec with Matchers {

  private def mkScrape(title: String, year: Option[Int]): CinemaMovie = CinemaMovie(
    movie     = Movie(title, releaseYear = year),
    cinema    = Multikino,
    posterUrl = None,
    filmUrl   = None,
    synopsis  = None,
    cast      = Seq.empty,
    director  = Seq.empty,
    showtimes = Seq(Showtime(LocalDateTime.now(), None))
  )

  // Subclass that blocks findAll until the test releases a latch — simulates
  // the slow Atlas round-trip that makes the prod race observable.
  private class GatedRepo(seed: Seq[(String, Option[Int], MovieRecord)], gate: CountDownLatch)
    extends InMemoryMovieRepo(seed) {
    override def findAll(): Seq[StoredMovieRecord] = {
      gate.await()
      super.findAll()
    }
  }

  "recordCinemaScrape" should "not create a duplicate key when it races an in-flight asyncHydrate" in {
    val gate = new CountDownLatch(1)
    // Mongo's canonical row: full (title, year) tuple resolved on a prior boot.
    val seed = Seq(("Mandalorian i Grogu", Some(2026), MovieRecord(imdbId = Some("tt1"))))
    val repo = new GatedRepo(seed, gate)

    val cache = new CaffeineMovieCache(repo, new InProcessEventBus(), asyncHydrate = true)

    // Scrape arrives before hydrate has loaded the Mongo row. The cinema feed
    // shipped the title without a release year (a real prod pattern — Helios
    // and Multikino both intermittently drop the year field on freshly added
    // films), so the naive cache key differs from the Mongo key by year alone.
    val scrapeFuture = Future {
      cache.recordCinemaScrape(Multikino, Seq(mkScrape("Mandalorian i Grogu", year = None)))
    }

    // Give the scrape a moment to start. With the fix it should be parked
    // waiting on the hydration latch; without the fix it races ahead and
    // writes a new row.
    Thread.sleep(200)

    // Release hydrate. The daemon thread finishes loading Mongo, then the
    // blocked scrape proceeds — now seeing the Mongo row and redirecting
    // its (no-year) key onto the canonical (year) key.
    gate.countDown()
    Await.result(scrapeFuture, 5.seconds)

    // Eventually-consistent — let hydrate's put + invalidate settle.
    Thread.sleep(100)

    val titles = cache.snapshot().map(r => (r.title, r.year))
    withClue(s"cache snapshot keys: $titles -- ") {
      titles should have size 1
    }
  }

  // ── Cleanup of duplicates that already shipped to Mongo ───────────────────

  private def mkCinemaSlot(cinema: Cinema): SourceData = SourceData(
    title       = Some("X"),
    showtimes   = Seq(Showtime(LocalDateTime.now(), None)),
    releaseYear = None
  )

  "rehydrate" should "merge year-less duplicates into the year-having canonical row" in {
    val repo = new InMemoryMovieRepo(Seq(
      // Canonical: TMDB-resolved row from a prior boot.
      ("Mandalorian i Grogu", Some(2026), MovieRecord(
        tmdbId = Some(123),
        imdbId = Some("tt9"),
        data   = Map(Multikino -> mkCinemaSlot(Multikino))
      )),
      // Buggy twin from the async-hydrate race: new cinema slot from a
      // post-bug scrape, no enrichment because it never went through TMDB.
      ("Mandalorian i Grogu", None, MovieRecord(
        data = Map(Helios -> mkCinemaSlot(Helios))
      ))
    ))

    val cache = new CaffeineMovieCache(repo, new InProcessEventBus())

    val snap = cache.snapshot()
    snap should have size 1
    val canonical = snap.head
    canonical.year   shouldBe Some(2026)
    canonical.record.tmdbId shouldBe Some(123)
    // Slots from BOTH rows survive — Multikino from canonical, Helios from
    // the merged year-less row.
    canonical.record.data.keySet shouldBe Set(Multikino, Helios)
    // Mongo side is also cleaned up.
    repo.deletes.map(_._2) should contain (None)
  }

  it should "leave a year-less row alone when it carries its own TMDB enrichment" in {
    // Two distinct rows that happen to normalise to the same title: one is
    // a genuinely year-less but TMDB-resolved indie (rare but real), the
    // other is the year-having sequel. Neither is buggy residue.
    val repo = new InMemoryMovieRepo(Seq(
      ("Mandalorian i Grogu", None,         MovieRecord(tmdbId = Some(999), imdbId = Some("tt8"))),
      ("Mandalorian i Grogu", Some(2026),   MovieRecord(tmdbId = Some(123)))
    ))

    val cache = new CaffeineMovieCache(repo, new InProcessEventBus())

    cache.snapshot() should have size 2
    repo.deletes shouldBe empty
  }

  it should "leave two year-having rows alone (legitimate sequels with same normalised title)" in {
    val repo = new InMemoryMovieRepo(Seq(
      ("Heat", Some(1995), MovieRecord(tmdbId = Some(1))),
      ("Heat", Some(2025), MovieRecord(tmdbId = Some(2)))
    ))

    val cache = new CaffeineMovieCache(repo, new InProcessEventBus())

    cache.snapshot() should have size 2
    repo.deletes shouldBe empty
  }
}
