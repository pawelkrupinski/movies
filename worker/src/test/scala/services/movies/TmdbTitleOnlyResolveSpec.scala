package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

/**
 * When the SEARCH TITLE is the only signal (no year, no director, no
 * original-title hint), a title search that returns several same-title films
 * would resolve by the popularity tie-break — a guess. The TMDB stage refuses
 * unless the search is unambiguous (exactly one result).
 *
 * The one widening: when a YEAR is present, a year-scoped search whose TOP hit
 * is an exact-title match resolves even if it returned several same-year films
 * (`searchYearExactTop`) — the year + verbatim top is confidence the yearless
 * case can't have. A non-exact top still refuses, and yearless rows are
 * unaffected.
 */
class TmdbTitleOnlyResolveSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  private def result(id: Int, title: String, date: String, pop: Double): String =
    s"""{"id":$id,"title":"$title","original_title":"$title","release_date":"$date","popularity":$pop}"""

  private def tmdb(routes: (String, String)*): TmdbClient =
    new TmdbClient(http = new StubFetch(routes), apiKey = Some("stub"))

  // A bare-title row: one cinema slot, no year, no director, no original title.
  private def bareRow(title: String): CaffeineMovieCache = {
    val seed = MovieRecord(data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(title))))
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq((title, None, seed))))
  }

  "the TMDB stage" should "NOT resolve a title-only row when the search returns several same-title films" in {
    val cache = bareRow("Zaproszenie")
    val search = s"""{"results":[${result(950028, "Zaproszenie", "2026-06-25", 4.7)},${result(830788, "Zaproszenie", "2022-08-24", 3.6)}]}"""
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb("/search/movie" -> search)) // no /external_ids stub — a resolve would throw on the unstubbed call

    service.reEnrichSync("Zaproszenie", None)
    cache.get(cache.keyOf("Zaproszenie", None)).flatMap(_.tmdbId) shouldBe None
  }

  it should "resolve a title-only row when the search is unambiguous (exactly one result)" in {
    val cache = bareRow("Unikat")
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"            -> s"""{"results":[${result(555, "Unikat", "2025-01-01", 2.0)}]}""",
        "/movie/555/external_ids"  -> """{"id":555,"imdb_id":"tt5550000"}"""
      ))

    service.reEnrichSync("Unikat", None)
    // The hit's release year re-keys the row off (Unikat, None), so assert on the
    // snapshot rather than the original key.
    cache.snapshot().flatMap(_.record.tmdbId) should contain (555)
  }

  it should "persist the wikidata_id from TMDB's /external_ids onto the resolved row" in {
    val cache = bareRow("Osobliwość")
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"            -> s"""{"results":[${result(888, "Osobliwość", "2023-01-01", 2.0)}]}""",
        // /external_ids carries wikidata_id alongside imdb_id — capture both.
        "/movie/888/external_ids"  -> """{"id":888,"imdb_id":"tt8880000","wikidata_id":"Q888"}"""
      ))

    service.reEnrichSync("Osobliwość", None)
    val record = cache.snapshot().find(_.record.tmdbId.contains(888)).map(_.record)
    record.flatMap(_.wikidataId) shouldBe Some("Q888")
  }

  it should "capture TMDB's English release title into the Tmdb slot (the cross-title merge alias)" in {
    // A non-Latin-original film: the search hit carries the Polish title and the
    // Chinese original; the English release title is only on the en-US `details`
    // call. The resolve must fold that en-US title into `englishTitle` so the
    // row a cinema lists as "Left-Handed Girl" later merges onto this one.
    val cache = bareRow("Left-Handed Girl")
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie" ->
          """{"results":[{"id":999075,"title":"Left-Handed Girl. To była ręka… diabła!",
            |"original_title":"左撇子女孩","release_date":"2025-10-01","popularity":3.0}]}""".stripMargin,
        "/movie/999075/external_ids" -> """{"id":999075,"imdb_id":"tt27722618"}""",
        // en-US `details` call — its `title` is the English release title.
        "language=en-US" -> """{"id":999075,"title":"Left-Handed Girl","release_date":"2025-10-01"}"""
      ))

    service.reEnrichSync("Left-Handed Girl", None)
    cache.snapshot().flatMap(_.record.data.get(Tmdb)).flatMap(_.englishTitle) should
      contain ("Left-Handed Girl")
  }

  it should "still resolve via the popularity pick when a YEAR disambiguates (guard is title-only)" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some("Zaproszenie"), releaseYear = Some(2026))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Zaproszenie", Some(2026), seed))))
    val search = s"""{"results":[${result(950028, "Zaproszenie", "2026-06-25", 4.7)}]}"""
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"              -> search,
        "/movie/950028/external_ids" -> """{"id":950028,"imdb_id":"tt14173636"}"""
      ))

    service.reEnrichSync("Zaproszenie", Some(2026))
    cache.get(cache.keyOf("Zaproszenie", Some(2026))).flatMap(_.tmdbId) shouldBe Some(950028)
  }

  it should "resolve a year-bearing row when the year-scoped search's TOP hit is an exact-title match (several same-year films)" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some("Sundown"), releaseYear = Some(2021))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Sundown", Some(2021), seed))))
    // Three 2021 films; the exact-title "Sundown" is the most popular, so after the
    // popularity sort it's the top hit. searchUnique refuses (count != 1); the new
    // year + exact-top rule resolves to it rather than leaving the row unenriched.
    val search = s"""{"results":[${result(701, "Sundown", "2021-01-01", 30.0)},${result(702, "Sundown Town", "2021-01-01", 5.0)},${result(703, "DJ at Sundown", "2021-01-01", 2.0)}]}"""
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"           -> search,
        "/movie/701/external_ids" -> """{"id":701,"imdb_id":"tt7010000"}"""
      ))

    service.reEnrichSync("Sundown", Some(2021))
    cache.snapshot().flatMap(_.record.tmdbId) should contain (701)
  }

  it should "NOT resolve a year-bearing row when the year-scoped search's TOP hit is NOT exact (no popularity guess)" in {
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some("Labirynt"), releaseYear = Some(2024))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Labirynt", Some(2024), seed))))
    // The most-popular 2024 hit is a SUBSTRING match, not exact; the exact title is
    // absent from the top → refuse rather than guess. (No /external_ids stub — a
    // resolve would throw on the unstubbed call, so a silent guess fails loudly.)
    val search = s"""{"results":[${result(801, "Labirynt strachu", "2024-01-01", 40.0)},${result(802, "W labiryncie", "2024-01-01", 10.0)}]}"""
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb("/search/movie" -> search))

    service.reEnrichSync("Labirynt", Some(2024))
    cache.get(cache.keyOf("Labirynt", Some(2024))).flatMap(_.tmdbId) shouldBe None
  }

  it should "resolve a year-bearing row to the most-popular exact match when multiple results are exact (same-year same-original-title)" in {
    // Two 2022 "The Visitor" films: the popular one (881487) matches via
    // original_title; the less-popular one (1026057) matches via title.
    // Both are exact matches — searchYearExactTop picks the most-popular one.
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some("The Visitor"), releaseYear = Some(2022))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("The Visitor", Some(2022), seed))))
    val search = """{"results":[
      |{"id":881487,"title":"Gość","original_title":"The Visitor","release_date":"2022-10-07","popularity":1.413},
      |{"id":1026057,"title":"The Visitor","original_title":"The Visitor","release_date":"2022-06-01","popularity":0.145}
      |]}""".stripMargin
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"              -> search,
        "/movie/881487/external_ids" -> """{"id":881487,"imdb_id":"tt15558152"}"""
      ))

    service.reEnrichSync("The Visitor", Some(2022))
    cache.get(cache.keyOf("The Visitor", Some(2022))).flatMap(_.tmdbId) shouldBe Some(881487)
  }

  // ── Embedded "(YYYY)" year hint — a year-less retrospective screening whose
  //    title carries the film year in parens ("Generał (1926) 4K") reads it as the
  //    lookup year, so it takes the safe year-scoped exact-title path instead of
  //    stalling at the singleton guard. Only when the row has NO scraped year. ──

  it should "resolve a year-LESS row via a parenthesised (YYYY) in its title (several same-title films)" in {
    val cache = bareRow("Generał (1926)")
    // Two same-title films → searchUnique refuses; the (1926) hint makes it a
    // year-scoped search whose exact-title top (961) resolves.
    val search = s"""{"results":[${result(961, "Generał", "1926-12-25", 12.0)},${result(999, "Generał brygady", "1926-01-01", 3.0)}]}"""
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"           -> search,
        "/movie/961/external_ids" -> """{"id":961,"imdb_id":"tt0017925"}"""
      ))

    service.reEnrichSync("Generał (1926)", None)
    cache.snapshot().flatMap(_.record.tmdbId) should contain (961)
  }

  it should "NOT read an embedded (YYYY) when the row already carries a scraped year (scraped year wins)" in {
    // Row scraped as 2015; title also carries a decorative "(1926)". The scraped
    // year must win — a 2015-scoped exact-title top resolves, the (1926) is ignored.
    val seed = MovieRecord(data = Map[Source, SourceData](
      CinemaCityPoznanPlaza -> SourceData(title = Some("Generał (1926)"), releaseYear = Some(2015))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Generał (1926)", Some(2015), seed))))
    val search = s"""{"results":[${result(700, "Generał", "2015-05-01", 8.0)},${result(701, "Generał brygady", "2015-01-01", 2.0)}]}"""
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdb(
        "/search/movie"           -> search,
        "/movie/700/external_ids" -> """{"id":700,"imdb_id":"tt7000000"}"""
      ))

    service.reEnrichSync("Generał (1926)", Some(2015))
    cache.get(cache.keyOf("Generał (1926)", Some(2015))).flatMap(_.tmdbId) shouldBe Some(700)
  }

  // The pure extraction is unit-tested in common's `EmbeddedYearSpec`; these
  // resolve-path specs cover how a scraped-yearless row USES the embedded year.
}
