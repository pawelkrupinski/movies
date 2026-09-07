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

  // The settle's OTHER cross-title edge, asked at landing: rows with one search-title
  // key (the title with the decorations the search rules strip removed, romanised).
  it should "land a Cyrillic listing on the resolved Latin row it romanises to" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    val key     = CacheKey("Vaiana", Some(2026), titleNormalizer)
    cache.put(key, MovieRecord(tmdbId = Some(1241982), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Vaiana"), originalTitle = Some("Moana 2"), releaseYear = Some(2026)),
      (KinoMuza: Source) -> SourceData(title = Some("Vaiana"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape(Helios, "Ваяна")))

    withClue(s"staging: ${staging.findAll().map(_.title)}\n") { staging.findAll() shouldBe empty }
    val row = cache.get(key).getOrElse(fail("the film's row is gone"))
    row.cinemaShowings.collectFirst { case (Helios, sd) => sd.title } shouldBe Some(Some("Ваяна"))
  }

  it should "land an edition whose stripped search title is the film's on the film's row" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    val key     = CacheKey("Ojczyzna", Some(2026), titleNormalizer)
    cache.put(key, MovieRecord(tmdbId = Some(1300001), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Ojczyzna"), originalTitle = Some("Ojczyzna"), releaseYear = Some(2026)),
      (KinoMuza: Source) -> SourceData(title = Some("Ojczyzna"), releaseYear = Some(2026)))))
    // The search rules strip the preview marker, so the settle would fold this row a
    // tick later; a DIFFERENT film whose title merely shares the word (and does not
    // carry it as a whole edge run, which would be the containment case) does not
    // share the key and incubates as a newcomer.
    FilmCanonicalizer.searchKey("Ojczyzna - pokaz przedpremierowy", titleNormalizer) shouldBe FilmCanonicalizer.searchKey("Ojczyzna", titleNormalizer)
    FilmCanonicalizer.searchKey("Ojczyzny nie ma", titleNormalizer) should not be FilmCanonicalizer.searchKey("Ojczyzna", titleNormalizer)

    cache.recordCinemaScrape(Helios, Seq(scrape(Helios, "Ojczyzna - pokaz przedpremierowy"), scrape(Helios, "Ojczyzny nie ma")))

    staging.findAll().map(_.title) shouldBe Seq("Ojczyzny nie ma")
    val row = cache.get(key).getOrElse(fail("the film's row is gone"))
    row.cinemaShowings.collectFirst { case (Helios, sd) => sd.title } shouldBe Some(Some("Ojczyzna - pokaz przedpremierowy"))
  }

  // The settle's year window at landing: a venue's production year may sit two years
  // before TMDB's release year (rule 2 of `clusterByFilm` attaches within ±2).
  it should "land a listing two years off the resolved row's year on that row, as the settle would attach it" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    val key     = CacheKey("Zawieście czerwone latarnie", Some(1991), titleNormalizer)
    cache.put(key, MovieRecord(tmdbId = Some(10412), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Zawieście czerwone latarnie"), originalTitle = Some("Da hong deng long gao gao gua"), releaseYear = Some(1991)),
      (KinoMuza: Source) -> SourceData(title = Some("Zawieście czerwone latarnie"), releaseYear = Some(1991)))))

    cache.recordCinemaScrape(Helios, Seq(CinemaMovie(Movie(title = "Zawieście czerwone latarnie", releaseYear = Some(1989)), Helios,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Nil)))

    withClue(s"staging: ${staging.findAll().map(_.title)}\n") { staging.findAll() shouldBe empty }
    cache.get(CacheKey("Zawieście czerwone latarnie", Some(1989), titleNormalizer)) shouldBe empty
    cache.get(key).getOrElse(fail("row gone")).cinemaShowings.map(_._1).toSet shouldBe Set(KinoMuza, Helios)
  }

  // A one-word film title runs along the edge of many unrelated titles. Without
  // evidence the listing must resolve on its own; with evidence the veto decides.
  it should "not treat a listing as a decoration of a ONE-word film unless it carries evidence" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    cache.put(CacheKey("It", Some(2017), titleNormalizer), MovieRecord(tmdbId = Some(346364), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("It"), originalTitle = Some("It"), releaseYear = Some(2017), runtimeMinutes = Some(135)),
      (KinoMuza: Source) -> SourceData(title = Some("It"), releaseYear = Some(2017)))))

    cache.recordCinemaScrape(Helios, Seq(
      scrape(Helios, "It Ends With Us"),                                                             // no evidence: incubates
      CinemaMovie(Movie(title = "It Ends With Us", releaseYear = Some(2024)), Helios, posterUrl = None, filmUrl = None,
        synopsis = None, cast = Nil, director = Nil, showtimes = Nil)))                            // evidence contradicts: incubates

    staging.findAll().map(_.title).distinct shouldBe Seq("It Ends With Us")
    cache.get(CacheKey("It", Some(2017), titleNormalizer)).map(_.cinemaShowings.map(_._1).toSet) shouldBe Some(Set(KinoMuza))
  }

  it should "still land a two-word film's banner variant without evidence" in {
    val staging = new InMemoryStagingRepository
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, staging = Some(staging), normalizer = titleNormalizer)
    val key     = CacheKey("Toy Story", Some(2026), titleNormalizer)
    cache.put(key, MovieRecord(tmdbId = Some(862), data = Map[Source, SourceData](
      Tmdb -> SourceData(title = Some("Toy Story"), originalTitle = Some("Toy Story"), releaseYear = Some(2026)),
      (KinoMuza: Source) -> SourceData(title = Some("Toy Story"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape(Helios, "Toddler Club: Toy Story")))

    staging.findAll() shouldBe empty
    cache.get(key).map(_.cinemaShowings.map(_._1).toSet) shouldBe Some(Set(KinoMuza, Helios))
  }
}
