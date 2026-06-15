package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

/**
 * When the SEARCH TITLE is the only signal (no year, no director, no
 * original-title hint), a title search that returns several same-title films
 * would resolve by the popularity tie-break — a guess. The TMDB stage now
 * refuses unless the search is unambiguous (exactly one result).
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
}
