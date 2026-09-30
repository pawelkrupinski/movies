package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, KinoMuza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RoutingHttpFetch
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Regression: "Mi Amor" (Kino Muza, reports director "Guillaume Nicloux",
 * release year 2025) went UNRESOLVED against TMDB.
 *
 * Captured from the live data:
 *   - A title search for "Mi amor" returns four same-title films; the closest-to-
 *     2025 hit is tmdb 1302640 — a DIFFERENT film (directors Christian Luna /
 *     Diego Alejandro Sierra), so `verifyByDirector` correctly rejects it.
 *   - The film the cinema actually shows is Nicloux's "Mi Amor" = tmdb 1432817,
 *     which TMDB dates 2026-05-06 (year 2026). The cinema reports 2025 (the
 *     production year). Filmography years routinely drift ±1 from a cinema's
 *     reported year (production vs first-release).
 *
 * `directorWalk` used to require an EXACT year match against the director's
 * filmography, so it found no 2025 Nicloux film and abstained. The fix lets the
 * walk pick the director's credit whose title matches the cinema's title even
 * when the year is off by one — landing on 1432817.
 */
class DirectorWalkYearDriftSpec extends AnyFlatSpec with Matchers {

  private val Title    = "Mi Amor"
  private val Year     = Some(2025)            // production year the cinema reports
  private val Decoy    = 1302640               // a different "Mi amor", 2025, not Nicloux
  private val Correct  = 1432817               // Nicloux's "Mi Amor", TMDB-dated 2026
  private val Director = "Guillaume Nicloux"
  private val PersonId = 17623

  private def miAmorTmdb(): TmdbClient = new TmdbClient(
    http = RoutingHttpFetch.getOnly(Seq(
      // The title search lands on the wrong same-year "Mi amor" (1302640).
      "/search/movie" -> s"""{"results":[
        |{"id":$Decoy,"title":"Mi amor","original_title":"Mi amor","release_date":"2025-01-02","popularity":0.097}
        |]}""".stripMargin,
      // Decoy credits: not Nicloux → verifyByDirector rejects it.
      s"/movie/$Decoy/credits" -> """{"crew":[{"id":1,"name":"Christian Luna","job":"Director"}]}""",
      // Director-walk recovery for "Guillaume Nicloux": his "Mi Amor" is dated
      // 2026, plus two 2024 films — so an exact-2025 match finds nothing and a
      // naive ±1 window is ambiguous; only the title disambiguates.
      "/search/person" -> s"""{"results":[{"id":$PersonId,"name":"Guillaume Nicloux","known_for_department":"Directing"}]}""",
      s"/person/$PersonId/movie_credits" -> s"""{"crew":[
        |{"id":2,"title":"Sarah Bernhardt","release_date":"2024-12-18","department":"Directing"},
        |{"id":$Correct,"title":"Mi Amor","release_date":"2026-05-06","department":"Directing"}
        |]}""".stripMargin,
      s"/movie/$Correct/external_ids" -> s"""{"id":$Correct,"imdb_id":""}"""
    )),
    apiKey = Some(settings.TmdbApiKey("stub"))
  )

  "a film whose TMDB release year drifts from the cinema's production year" should
    "still resolve via the director's filmography by title" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      KinoMuza -> SourceData(title = Some(Title), director = Seq(Director), releaseYear = Some(2025))))
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val bus   = new services.events.InProcessEventBus()
    val service = new MovieService(cache, bus, miAmorTmdb())

    service.reEnrichSync(Title, Year)

    cache.get(cache.keyOf(Title, Year)).flatMap(_.tmdbId) shouldBe Some(Correct)
  }
  // Cinema City's "Lalka (ale to horror)" credits Rod Blackhurst, 2025 and 82 minutes: his
  // horror "Dolly", which TMDB dates 2026 and times at 83. The title shares nothing with it, and
  // no Blackhurst credit sits at 2025 — two sit at 2026 and two at 2024 — so only the runtime
  // picks the one a year off. Unresolved, the settle folded it onto Kawalski's "Lalka".
  private val Dolly      = 1309083
  private val Blackhurst = 1043983
  private def dollyTmdb(): TmdbClient = new TmdbClient(
    http = RoutingHttpFetch.getOnly(Seq(
      "/search/movie" -> """{"results":[
        |{"id":1321666,"title":"Lalka","original_title":"Lalka","release_date":"2026-09-25","popularity":3.0}
        |]}""".stripMargin,
      "/search/person" -> s"""{"results":[{"id":$Blackhurst,"name":"Rod Blackhurst","known_for_department":"Directing"}]}""",
      s"/person/$Blackhurst/movie_credits" -> s"""{"crew":[
        |{"id":1025596,"title":"Blood for Dust","release_date":"2024-04-19","department":"Directing","job":"Director"},
        |{"id":1362471,"title":"The Tennessee 11","release_date":"2024-09-21","department":"Directing","job":"Director"},
        |{"id":$Dolly,"title":"Dolly","release_date":"2026-03-06","department":"Directing","job":"Director"},
        |{"id":1743661,"title":"Horror Anthology Volume 2","release_date":"2026-08-01","department":"Directing","job":"Director"}
        |]}""".stripMargin,
      s"/movie/$Dolly/external_ids" -> s"""{"id":$Dolly,"imdb_id":""}""",
      s"/movie/$Dolly?"   -> s"""{"id":$Dolly,"title":"Dolly","original_title":"Dolly","release_date":"2026-03-06","runtime":83}""",
      "/movie/1025596?"   -> """{"id":1025596,"title":"Blood for Dust","release_date":"2024-04-19","runtime":97}""",
      "/movie/1362471?"   -> """{"id":1362471,"title":"The Tennessee 11","release_date":"2024-09-21","runtime":64}""",
      "/movie/1743661?"   -> """{"id":1743661,"title":"Horror Anthology Volume 2","release_date":"2026-08-01","runtime":95}"""
    )),
    apiKey = Some(settings.TmdbApiKey("stub"))
  )

  "a credit a year off the cinema's, whose title shares nothing with the listing's" should
    "resolve when it is the one credit in the window the cinema's runtime agrees with" in {
    val title = "Lalka (ale to horror)"
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some(title), director = Seq("Rod Blackhurst"), releaseYear = Some(2025),
        runtimeMinutes = Some(82))))
    val repository = new InMemoryMovieRepository(Seq((title, Some(2025), seed)), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val service = new MovieService(cache, new services.events.InProcessEventBus(), dollyTmdb())

    service.reEnrichSync(title, Some(2025))

    cache.get(cache.keyOf(title, Some(2025))).flatMap(_.tmdbId) shouldBe Some(Dolly)
  }
}
