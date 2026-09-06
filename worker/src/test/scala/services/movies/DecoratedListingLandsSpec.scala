package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.staging.InMemoryStagingRepository

/**
 * Nine days of prod re-key logs (2026-08-29 → 09-06): every Cineworld venue's first
 * scrape of "gb Fallen Angels by Noël Coward." became a NEWCOMER — diverted to
 * staging, resolved to the very film already in `movies`, folded onto it, re-keyed —
 * 92 times for one film. The divert gate's three questions were all keyed on the
 * exact decorated title, which the corpus never holds a row under because the fold
 * always absorbs it. The settle knew the listing was a decoration; the gate did not.
 * Now they ask the same question, and the listing lands on the film's row.
 */
class DecoratedListingLandsSpec extends AnyFlatSpec with Matchers {

  private val film      = "Fallen Angels by Noël Coward"
  private val decorated = "gb Fallen Angels by Noël Coward."

  private def resolvedRow: MovieRecord =
    MovieRecord(tmdbId = Some(1702350),
      data = Map[Source, SourceData](
        Tmdb -> SourceData(title = Some(film), originalTitle = Some(film), releaseYear = Some(2026)),
        (KinoMuza: Source) -> SourceData(title = Some(film), releaseYear = Some(2026))))

  private def scrape(cinema: Cinema, title: String): CinemaMovie =
    CinemaMovie(Movie(title = title), cinema, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Nil)

  "recordCinemaScrape" should "land a decorated listing of a known film on that film's row, not in staging" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    val key     = CacheKey(film, Some(2026), titleNormalizer)
    cache.put(key, resolvedRow)

    // A DIFFERENT venue than the one already on the row — the flap was per venue.
    cache.recordCinemaScrape(Helios, Seq(scrape(Helios, decorated)))

    withClue(s"staging: ${staging.findAll().map(_.title)}\n") { staging.findAll() shouldBe empty }
    val row = cache.get(key).getOrElse(fail("the film's row is gone"))
    row.cinemaShowings.map(_._1).toSet shouldBe Set(KinoMuza, Helios)
    row.cinemaShowings.collectFirst { case (Helios, sd) => sd.title } shouldBe Some(Some(decorated))
    cache.entries.map(_._1.cleanTitle).filter(_.startsWith("gb")) shouldBe empty
  }

  it should "still incubate a sequel that merely carries the film's title" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    cache.put(CacheKey("The Hunger Games", Some(2012), titleNormalizer),
      MovieRecord(tmdbId = Some(70160), data = Map[Source, SourceData](
        Tmdb -> SourceData(title = Some("The Hunger Games"), releaseYear = Some(2012)),
        (KinoMuza: Source) -> SourceData(title = Some("The Hunger Games"), releaseYear = Some(2012)))))

    cache.recordCinemaScrape(Helios, Seq(scrape(Helios, "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)")))

    staging.findAll().map(_.title) should contain ("The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)")
  }
}
