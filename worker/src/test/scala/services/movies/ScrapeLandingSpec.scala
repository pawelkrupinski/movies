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

  private def scrape(cinema: Cinema, title: String, runtimeMinutes: Option[Int] = None): CinemaMovie =
    CinemaMovie(Movie(title = title, runtimeMinutes = runtimeMinutes), cinema, posterUrl = None, filmUrl = None,
      synopsis = None, cast = Nil, director = Nil, showtimes = Nil)

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

  // `StagingFold.resolveKeyCollisions` (services.staging) gives a losing cluster a
  // key suffixed with its OWN tmdbId when it collides with another film's plain key
  // (the 'Lalka' incident — tmdbId 1321666 vs an unrelated 1309396, both bare-titled
  // "Lalka", both TMDB year 2026). The REJECTED approach that would have broken this
  // check: suffixing the stored key while leaving `CacheKey.normalized` alone drops
  // the loser out of `CorpusIndex.rowsByNormalized`'s bucket, so a future listing of
  // EITHER film would see only the winner and land on it with no disambiguation. This
  // pins that both films stay independently reachable, and that `concludedKeyFor` /
  // `chooseConcluded`'s runtime corroboration — built for exactly this "two films
  // share a title" shape — still tells them apart by a NEW cinema's published minutes.
  "ScrapeLanding" should "keep BOTH films of a resolved key collision independently reachable and disambiguated by runtime" in {
    val repository = new InMemoryMovieRepository
    val staging    = new InMemoryStagingRepository
    val store      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val landing    = new ScrapeLanding(store, repository, Some(staging), new InProcessEventBus(),
      ScreeningTokens.Default, CountryNames.DefaultLanguage)

    val plainKey        = CacheKey("Lalka", Some(2026), titleNormalizer)
    val disambiguatedKey = CacheKey.disambiguated(plainKey, "tmdb1321666")
    // The winner: kept at the plain key, tmdbId 1309396, TMDB runtime 90.
    store.put(plainKey, MovieRecord(tmdbId = Some(1309396), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Lalka"), releaseYear = Some(2026), runtimeMinutes = Some(90)))))
    // The loser: disambiguated, tmdbId 1321666, TMDB runtime 180 — a real, unrelated
    // film sharing nothing but the bare title and year.
    store.put(disambiguatedKey, MovieRecord(tmdbId = Some(1321666), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Lalka"), releaseYear = Some(2026), runtimeMinutes = Some(180)))))

    // A THIRD cinema's bare "Lalka" listing, publishing minutes that match the
    // DISAMBIGUATED film's own runtime — must land there, not on the plain key.
    val landedOnLoser = landing.recordCinemaScrape(Multikino, Seq(scrape(Multikino, "Lalka", runtimeMinutes = Some(180))))
    landedOnLoser.map { case (_, k, _) => k } shouldBe Seq(disambiguatedKey)
    store.get(disambiguatedKey).map(_.cinemaShowings.map(_._1).toSet) shouldBe Some(Set(Multikino))
    withClue("the winner's own row must be untouched by a listing that belongs to the other film\n")(
      store.get(plainKey).map(_.cinemaShowings.map(_._1).toSet) shouldBe Some(Set.empty))

    // A FOURTH cinema's bare "Lalka" listing, publishing minutes that match the
    // PLAIN-key film's own runtime — must land on the plain key instead.
    val landedOnWinner = landing.recordCinemaScrape(KinoMuza, Seq(scrape(KinoMuza, "Lalka", runtimeMinutes = Some(90))))
    landedOnWinner.map { case (_, k, _) => k } shouldBe Seq(plainKey)
    store.get(plainKey).map(_.cinemaShowings.map(_._1).toSet) shouldBe Some(Set(KinoMuza))
    withClue("the disambiguated film's own row must be untouched by a listing that belongs to the other film\n")(
      store.get(disambiguatedKey).map(_.cinemaShowings.map(_._1).toSet) shouldBe Some(Set(Multikino)))
  }
}
