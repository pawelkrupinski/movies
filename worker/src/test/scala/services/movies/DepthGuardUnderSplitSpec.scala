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
      val times = (0 until showtimesEach).map(n => showtime(f"2027-06-${8 + n / 12}%02dT${8 + n % 12}%02d:00"))
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
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    storedShowtimes(repository, "Film 1") shouldBe 12

    // Every film still listed — the film-count guard reads a full board — but each carries
    // one screening instead of twelve. A chunked scrape that lost most of its dates.
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 1))
    storedShowtimes(repository, "Film 1") shouldBe 12
  }

  it should "still apply a plausible shrink under the split (a real schedule change)" in {
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)

    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 12))
    cache.recordCinemaScrape(Multikino, deepScrape(films = 10, showtimesEach = 10))
    storedShowtimes(repository, "Film 1") shouldBe 10
  }

  it should "keep a stripped slot's showtime count available to the guard" in {
    // The mechanism the two specs above depend on, pinned directly: stripping for cache
    // residency drops the list but must not drop how many there were.
    val record   = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(showtimes = Seq(showtime("2027-06-08T18:00"), showtime("2027-06-08T20:00")))))
    val stripped = ShowtimesDigest.stripForCache(record)

    stripped.data.values.head.showtimes                       shouldBe empty
    ShowtimesDigest.slotShowtimeCount(stripped.data.values.head) shouldBe 2
  }
}
