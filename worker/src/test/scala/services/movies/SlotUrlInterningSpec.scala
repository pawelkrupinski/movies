package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * A film's poster / film-page / trailer URL is the SAME string at every cinema
 * showing it, so the per-cinema slots must share ONE instance rather than each
 * holding a byte-identical copy — exactly what [[StringPool]] already does for
 * synopsis, cast, director, countries and genres.
 *
 * Measured from the 2026-07-27 UK OOM heap dump: 656,202 URL strings occupying
 * 53.1 MB collapsed to 216,850 distinct values (16.9 MB) — a 3x duplication
 * factor worth ~36 MB. It is concentrated in exactly these fields, because a
 * popular film's poster and film page repeat once per cinema slot:
 *   - poster URLs      136,064 occurrences ->  1,896 distinct (71.8x)
 *   - flicks /movie/   138,199 occurrences ->  2,004 distinct (69.0x)
 * whereas `Showtime.bookingUrl` is per-screening and only 1.6x duplicated
 * (182,719 -> 116,571), so it is deliberately NOT interned: it would evict the
 * whole low-cardinality pool for almost no saving.
 */
class SlotUrlInterningSpec extends AnyFlatSpec with Matchers {

  private val poster  = "https://d32qys9a6wm9no.cloudfront.net/images/movies/poster/zjZ3UhmU49oNd8WHNCH"
  private val film    = "https://www.flicks.co.uk/movie/spider-man-brand-new-day/"
  private val trailer = "https://www.youtube.com/watch?v=aBcDeFgHiJk"

  /** Fresh (non-interned) instances, as a scraper's parser would produce them —
   *  `new String` defeats the compile-time literal pool the way a real parse does. */
  private def showing(cinema: Cinema) = CinemaMovie(
    movie     = Movie(title = "Spider-Man: Brand New Day", releaseYear = Some(2026)),
    cinema    = cinema,
    posterUrl = Some(new String(poster)),
    filmUrl   = Some(new String(film)),
    synopsis  = None,
    cast      = Seq.empty,
    director  = Seq.empty,
    showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 27, 20, 0), Some("https://book.example/1"))),
    trailerUrl = Some(new String(trailer))
  )

  private def slotsForOneFilmAtTwoCinemas(): Seq[SourceData] = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq.empty), normalizer = titleNormalizer)
    val a = OdeonNorwich
    val b = BfiLondonSouthbank
    cache.recordCinemaScrape(a, Seq(showing(a)))
    cache.recordCinemaScrape(b, Seq(showing(b)))
    val record = cache.entries.map(_._2).find(_.data.size >= 2)
    withClue("expected ONE film row carrying a slot per cinema") { record.isDefined shouldBe true }
    record.get.data.values.toSeq
  }

  "the cinema slot builder" should "share one poster-URL instance across a film's cinema slots" in {
    val urls = slotsForOneFilmAtTwoCinemas().flatMap(_.posterUrl)
    urls should have size 2
    urls.head shouldBe poster
    (urls.head eq urls(1)) shouldBe true
  }

  it should "share one film-URL instance across a film's cinema slots" in {
    val urls = slotsForOneFilmAtTwoCinemas().flatMap(_.filmUrl)
    urls should have size 2
    urls.head shouldBe film
    (urls.head eq urls(1)) shouldBe true
  }

  it should "share one trailer-URL instance across a film's cinema slots" in {
    val urls = slotsForOneFilmAtTwoCinemas().flatMap(_.trailerUrl)
    urls should have size 2
    urls.head shouldBe trailer
    (urls.head eq urls(1)) shouldBe true
  }
}
