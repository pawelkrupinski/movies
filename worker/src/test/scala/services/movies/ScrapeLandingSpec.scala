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
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val staging    = new InMemoryStagingRepository
    val store      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val landing    = new ScrapeLanding(store, repository, Some(staging), new InProcessEventBus(),
      ScreeningTokens.forDefaultCountry(), CountryNames.DefaultLanguage)
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

  // A DETAIL-ONLY SLOT is a venue's slot its listing never wrote: the detail handler merges
  // into `SourceData()` when the row holds no slot of that venue, which leaves no `title` —
  // every listing write sets one. One shared bilety24 detail group wrote every bilety24
  // film's detail onto Janosik's slot (7b225cab2 stopped new ones), so production rows carry
  // such phantoms. They are no evidence of what the venue lists: they must neither inflate
  // the breadth guard (which then skips the prune that would clear them) nor survive a tick
  // of that venue, whatever the guard says about its real slots.
  it should "drop the venue's detail-only slots on its next scrape, even while the breadth guard spares its real ones" in {
    def rig() = {
      val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
      val store      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
      val landing    = new ScrapeLanding(store, repository, None, new InProcessEventBus(),
        ScreeningTokens.forDefaultCountry(), CountryNames.DefaultLanguage)
      (store, landing)
    }
    def row(title: String, slots: (Source, SourceData)*) =
      MovieRecord(tmdbId = Some(title.hashCode.abs), data = Map[Source, SourceData](
        Tmdb -> SourceData(title = Some(title), releaseYear = Some(2026))) ++ slots)
    def listed(title: String)  = CinemaShowing.keyFor(Helios, title, titleNormalizer) -> SourceData(title = Some(title), releaseYear = Some(2026))
    def phantom(title: String) = CinemaShowing.keyFor(Helios, title, titleNormalizer) -> SourceData(synopsis = Some("Bałtyk's film, not ours"))
    def muza(title: String)    = CinemaShowing.keyFor(KinoMuza, title, titleNormalizer) -> SourceData(title = Some(title), releaseYear = Some(2026))
    def heliosSlots(store: CaffeineMovieCache, title: String) =
      store.get(CacheKey(title, Some(2026), titleNormalizer)).toSeq.flatMap(_.data.keys.filter(s => Source.cinemaOf(s).contains(Helios)))

    val shown    = Seq("Alpha", "Beta", "Gamma")
    val phantoms = (1 to 10).map(i => s"Other Film $i")

    // Healthy venue: it still lists everything it really showed.
    {
      val (store, landing) = rig()
      shown.foreach(t => store.put(CacheKey(t, Some(2026), titleNormalizer), row(t, listed(t))))
      phantoms.foreach(t => store.put(CacheKey(t, Some(2026), titleNormalizer), row(t, muza(t), phantom(t))))
      landing.recordCinemaScrape(Helios, shown.map(t => scrape(Helios, t).copy(movie = Movie(title = t, releaseYear = Some(2026)))))
      phantoms.foreach(t => withClue(s"$t: ")(heliosSlots(store, t) shouldBe empty))
      shown.foreach(t => withClue(s"$t: ")(heliosSlots(store, t) should not be empty))
    }
    // A thin tick the breadth guard holds: its real slots are spared, the phantoms still go.
    {
      val (store, landing) = rig()
      val formerlyShown = (1 to 10).map(i => s"Shown $i")
      (shown ++ formerlyShown).foreach(t => store.put(CacheKey(t, Some(2026), titleNormalizer), row(t, listed(t))))
      phantoms.foreach(t => store.put(CacheKey(t, Some(2026), titleNormalizer), row(t, muza(t), phantom(t))))
      landing.recordCinemaScrape(Helios, shown.map(t => scrape(Helios, t).copy(movie = Movie(title = t, releaseYear = Some(2026)))))
      formerlyShown.foreach(t => withClue(s"$t (guard-spared): ")(heliosSlots(store, t) should not be empty))
      phantoms.foreach(t => withClue(s"$t: ")(heliosSlots(store, t) shouldBe empty))
    }
  }
}
