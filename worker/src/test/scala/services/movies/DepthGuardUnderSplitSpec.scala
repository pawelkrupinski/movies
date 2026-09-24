package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The degraded-scrape DEPTH guard, exercised against PRODUCTION's storage shape — the
 * `screenings` + `movie_slots` split wired — rather than the inline-showtimes default the
 * rest of `MovieCacheSpec` uses.
 *
 * That distinction is the whole spec. `CaffeineMovieCache.forCache` strips a record's
 * showtime LISTS the moment `repository.hasScreenings` is true, so every resident slot in
 * production carries `Nil`. The guard measured what a cinema was holding with
 * `showtimes.size`, which is therefore 0 for every cinema on a real worker — its
 * `MinShowtimesForDepthGuard` floor never engaged and the guard was dead code from the day
 * the split turned on. Every existing depth spec wires a bare `InMemoryMovieRepository`,
 * the one shape that keeps the lists resident, so all of them passed throughout.
 */
class DepthGuardUnderSplitSpec extends AnyFlatSpec with Matchers {

  private def showtime(iso: String) = Showtime(LocalDateTime.parse(iso), None)

  /** `films` slots at Multikino, each carrying `showtimesEach` distinct screenings. */
  private def deepScrape(films: Int, showtimesEach: Int): Seq[CinemaMovie] =
    (1 to films).map { i =>
      val times = DepthGuardTime.showtimes(showtimesEach)
      CinemaMovie(movie = Movie(s"Film $i", releaseYear = Some(2026)), cinema = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
        showtimes = times)
    }

  private def splitRepository() = new InMemoryMovieRepository(
    screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository))

  /** What the film actually has STORED — the cache's own copy is stripped under the split,
   *  so asking it would measure the wrong thing. */
  private def storedShowtimes(repository: InMemoryMovieRepository, title: String): Int =
    repository.findAll().find(_.title.contains(title))
      .map(_.record.data.values.map(_.showtimes.size).sum).getOrElse(0)

  it should "discard a depth-degraded tick when showtimes and slots live in their own collections" in {
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    storedShowtimes(repository, "Film 1") shouldBe 12

    // Every film still listed — the film-count guard reads a full board — but each carries
    // one screening instead of twelve. A chunked scrape that lost most of its dates.
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1))
    storedShowtimes(repository, "Film 1") shouldBe 12
  }

  it should "still apply a plausible shrink under the split (a real schedule change)" in {
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 10))
    storedShowtimes(repository, "Film 1") shouldBe 10
  }

  it should "let a smaller consecutive-rejection cap accept a degraded tick sooner" in {
    // The shape behind `ScrapeHealth.maxRejectionsFor`: a slow-cadence country's flat
    // 3-tick grace can hold a venue's stale showtimes for far longer than its own
    // scrape interval implies (es/Multicines Zamora, 2026-09-07: 21h). Wiring a
    // smaller cap through `CaffeineMovieCache` accepts the degraded listing on the
    // SECOND thin tick instead of the fourth.
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock,
      maxConsecutiveGuardRejections = 1)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1)) // 1st reject: held
    storedShowtimes(repository, "Film 1") shouldBe 12

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1)) // 2nd reject: accepted
    storedShowtimes(repository, "Film 1") shouldBe 1
  }

  it should "keep the default cap's full grace when no override is given" in {
    // Same two thin ticks, default (unset) cap: still held after both, matching the
    // existing "discard a depth-degraded tick" spec above — the default threads
    // through unchanged.
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1))
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1))
    storedShowtimes(repository, "Film 1") shouldBe 12
  }

  it should "let a sustained film-count drop finally prune the slots it stopped listing" in {
    // The BREADTH guard's escape valve (`ScrapeHealth.breadth`, added 2026-09-13 — Kino
    // Aurum's shape: 11 currently-listed films against 57 accumulated slot-keys, a
    // ratio that could never clear the floor on its own because the guard's OWN
    // prune-skip was what stopped the stale 46 from ever being retired). Simulated
    // here as a straight film-count drop, 20 films down to 9, each remaining film
    // carrying MORE showtimes so total showtimes stay well above the DEPTH floor
    // throughout — this spec isolates the breadth guard alone.
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 20, showtimesEach = 12))
    storedShowtimes(repository, "Film 1")  shouldBe 12
    storedShowtimes(repository, "Film 20") shouldBe 12

    // Three thin ticks: only Film 1..9 are listed (270 showtimes vs a 240-showtime
    // known total — comfortably healthy on the DEPTH axis), but the film/slot COUNT
    // (9 vs 20) is well below the breadth floor. The prune is skipped each time, so
    // Film 20 (unlisted this tick) keeps its stale Multikino slot — the guard doing
    // its job. Film 1 (listed every tick) is written normally regardless: the
    // breadth guard gates the PRUNE, never the per-film write of what WAS listed.
    (1 to ScrapeHealth.MaxConsecutiveDepthRejections).foreach { _ =>
      cache.recordCinemaScrape(Multikino, deepScrape(films = 9, showtimesEach = 30))
      storedShowtimes(repository, "Film 1")  shouldBe 30
      storedShowtimes(repository, "Film 20") shouldBe 12
    }

    // The fourth thin tick exhausts the guard's grace: the prune finally runs, and
    // Multikino's slot on Film 20 — never listed again since the first scrape — is
    // dropped. Film 1 is unaffected, exactly as a real, sustained schedule cut
    // should behave once the guard stops treating it as a bad fetch.
    cache.recordCinemaScrape(Multikino, deepScrape(films = 9, showtimesEach = 30))
    storedShowtimes(repository, "Film 1")  shouldBe 30
    storedShowtimes(repository, "Film 20") shouldBe 0
  }

  it should "measure a venue against the showtimes it still has AHEAD, not the ones that have passed" in {
    // A venue stuck on a stale listing: everything it stored is now behind the clock,
    // so there is nothing left to protect. Counting those passed showtimes as the
    // baseline held Braniewo's Baszta rejecting against another town's programme long
    // after that programme had run out.
    val clock      = new tools.MutableClock(java.time.Instant.parse("2027-06-08T00:00:00Z"))
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = clock)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))   // all on 06-08
    storedShowtimes(repository, "Film 1") shouldBe 12

    clock.advance(java.time.Duration.ofHours(23))                                      // 06-08 23:00
    val nextDay = (1 to 10).map { i =>
      CinemaMovie(movie = Movie(s"Film $i", releaseYear = Some(2026)), cinema = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
        showtimes = Seq(showtime("2027-06-09T18:00"), showtime("2027-06-09T20:00")))
    }
    cache.recordCinemaScrape(Multikino, nextDay)
    storedShowtimes(repository, "Film 1") shouldBe 2
  }

  it should "keep a stripped slot's upcoming showtime count available to the guard" in {
    // The mechanism the specs above depend on, pinned directly: stripping for cache
    // residency drops the list but must not drop when each showtime starts.
    val record   = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(showtimes = Seq(showtime("2027-06-08T18:00"), showtime("2027-06-08T20:00")))))
    val stripped = ShowtimesDigest.stripForCache(record).data.values.head
    val full     = record.data.values.head

    stripped.showtimes                          shouldBe empty
    ShowtimesDigest.slotShowtimeCount(stripped) shouldBe 2

    Seq(stripped, full).foreach { slot =>
      ShowtimesDigest.upcomingShowtimeCount(slot, LocalDateTime.parse("2027-06-08T17:00")) shouldBe 2
      ShowtimesDigest.upcomingShowtimeCount(slot, LocalDateTime.parse("2027-06-08T19:00")) shouldBe 1
      ShowtimesDigest.upcomingShowtimeCount(slot, LocalDateTime.parse("2027-06-08T20:00")) shouldBe 0
    }
  }
}
