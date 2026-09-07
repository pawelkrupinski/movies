package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CountryNames
import services.events.InProcessEventBus
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.staging.InMemoryStagingRepository

/**
 * [[ScrapeLanding]] on its own constructor: the scrape-time landing is a class the cache
 * delegates to, built on the cache as its [[LandingStore]], and this spec drives it
 * through that seam rather than through `MovieCache.recordCinemaScrape`. The case is
 * the one the seam exists for — a decorated listing of a known film lands on the film's
 * row instead of incubating as a newcomer (`DecoratedListingLandsSpec` pins the same
 * behaviour through the cache's delegation).
 */
class ScrapeLandingSpec extends AnyFlatSpec with Matchers {

  private val film      = "Fallen Angels by Noël Coward"
  private val decorated = "gb Fallen Angels by Noël Coward."

  private def scrape(cinema: Cinema, title: String): CinemaMovie =
    CinemaMovie(Movie(title = title), cinema, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Nil)

  "ScrapeLanding" should "land a decorated listing of a known film on that film's row, through the store seam" in {
    val repository = new InMemoryMovieRepository
    val staging    = new InMemoryStagingRepository
    val store      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val landing    = new ScrapeLanding(store, repository, Some(staging), new InProcessEventBus(),
      ScreeningTokens.Default, CountryNames.DefaultLanguage)
    val key        = CacheKey(film, Some(2026), titleNormalizer)
    store.put(key, MovieRecord(tmdbId = Some(1702350), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some(film), originalTitle = Some(film), releaseYear = Some(2026)),
      (KinoMuza: Source) -> SourceData(title = Some(film), releaseYear = Some(2026)))))

    val landed = landing.recordCinemaScrape(Helios, Seq(scrape(Helios, decorated)))

    landed.map { case (_, k, isNew) => (k, isNew) } shouldBe Seq(key -> true)
    withClue(s"staging: ${staging.findAll().map(_.title)}\n") { staging.findAll() shouldBe empty }
    val row = store.get(key).getOrElse(fail("the film's row is gone"))
    row.cinemaShowings.map(_._1).toSet shouldBe Set(KinoMuza, Helios)
    row.cinemaShowings.collectFirst { case (Helios, sd) => sd.title } shouldBe Some(Some(decorated))
    // Written through the store's funnel, so the repository holds it too.
    repository.findAll().map(_.record.cinemaShowings.map(_._1).toSet) shouldBe Seq(Set(KinoMuza, Helios))
  }
}
