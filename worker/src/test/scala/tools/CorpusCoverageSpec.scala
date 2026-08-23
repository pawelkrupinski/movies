package tools

import models._
import services.movies.ShowtimesDigest
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * The RUN side of the convergence band — the counterpart to [[ProdCoverage]], which
 * counts production.
 *
 * Its one interesting property is the one the band was silently missing: production is
 * counted over the films SCREENING at the capture instant, so the replay has to be
 * too. `cinema_scrapes` keeps every venue's last content-bearing scrape for as long as
 * the venue stays white — that is what stops an outage erasing a cinema — so a corpus
 * carries venues whose newest showtime has already passed, and counting their films
 * put Poland's `films` axis 40 films from production while every enrichment axis
 * measured against it sat inside 3%.
 */
class CorpusCoverageSpec extends AnyFlatSpec with Matchers {

  private val Now = LocalDateTime.of(2026, 8, 22, 23, 45)

  private def film(title: String, showtimes: LocalDateTime*): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](
      CinemaShowing(KinoPalacowe, title) ->
        SourceData(title = Some(title), showtimes = showtimes.map(Showtime(_, None)).toList)))

  "isScreening" should "count a film a cinema still lists a showtime for" in {
    CorpusCoverage.isScreening(film("a", Now.plusDays(3)), Now) shouldBe true
  }

  // The tail this exists for: a venue that stopped answering in July is still in the
  // corpus in August, with its July showtimes. Production does not count its films.
  it should "not count a film whose every showtime has already passed" in {
    CorpusCoverage.isScreening(film("a", Now.minusDays(30), Now.minusDays(2)), Now) shouldBe false
  }

  it should "count a film that has one future showtime among many past ones" in {
    CorpusCoverage.isScreening(film("a", Now.minusDays(30), Now.plusHours(2)), Now) shouldBe true
  }

  // `Showtime.isUpcoming`, not a bare `isAfter` — the same grace-windowed rule the web
  // filters list views by, so a film is never dropped here for a reason that would
  // still keep it on the page.
  it should "count a screening that started within the listing grace window" in {
    CorpusCoverage.isScreening(film("a", Now.minusMinutes(10)), Now) shouldBe true
  }

  it should "not count a film no cinema holds a slot for at all" in {
    CorpusCoverage.isScreening(MovieRecord(data = Map[Source, SourceData](Tmdb -> SourceData())), Now) shouldBe false
  }

  /** A slot stripped for cache residency has no dates left to judge — only a digest and
   *  a count — so "unknown" must not be read as "empty". Convicting on a stripped slot
   *  is how a rule that walks one ends up dead in production while every spec on the
   *  embedded shape passes. */
  it should "count a slot whose showtimes were stripped for the cache" in {
    val stripped = ShowtimesDigest.stripForCache(film("a", Now.plusDays(1), Now.plusDays(2)))
    stripped.cinemaSlots.head._2.showtimes shouldBe empty      // the strip really happened
    CorpusCoverage.isScreening(stripped, Now) shouldBe true
  }

  it should "not count a stripped slot that held nothing" in {
    CorpusCoverage.isScreening(ShowtimesDigest.stripForCache(film("a")), Now) shouldBe false
  }

  "screening" should "leave the corpus's stale tail out of the coverage counted against production" in {
    val corpus = Seq(
      film("still-playing", Now.plusDays(1)),
      film("also-playing",  Now.plusDays(4)),
      film("july-only",     Now.minusDays(23)),      // a venue that went white in July
      film("june-only",     Now.minusDays(61)))

    CorpusCoverage.of(corpus).films                              shouldBe 4
    CorpusCoverage.of(CorpusCoverage.screening(corpus, Now)).films shouldBe 2
  }

  "of" should "count each enrichment field over exactly the records it is given" in {
    val screening = film("a", Now.plusDays(1)).copy(tmdbId = Some(1), imdbId = Some("tt1"), imdbRating = Some(7.1))
    val stale     = film("b", Now.minusDays(30)).copy(tmdbId = Some(2), imdbId = Some("tt2"), imdbRating = Some(6.0))

    val counted = CorpusCoverage.of(CorpusCoverage.screening(Seq(screening, stale), Now))
    counted.films      shouldBe 1
    counted.tmdbId     shouldBe 1
    counted.imdbId     shouldBe 1
    counted.imdbRating shouldBe 1
    counted.metascore  shouldBe 0
  }

  "zoneOf" should "read the capture instant on the country's own wall clock" in {
    // The corpus's showtimes are zone-less local times, so a UTC instant recorded just
    // before midnight is already the next day in Warsaw — and films screening in that
    // gap must not read as past.
    CorpusCoverage.localise(java.time.Instant.parse("2026-08-22T23:45:55Z"), Country.Poland) shouldBe
      LocalDateTime.of(2026, 8, 23, 1, 45, 55)
  }
}
