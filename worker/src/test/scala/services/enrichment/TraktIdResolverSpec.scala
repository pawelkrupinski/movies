package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.TraktIdResolver.TraktResolution
import tools.HttpFetch

/**
 * TraktIdResolver drives the real TraktClient over a url-routed fake fetch
 * (fixtures on disk) — so the resolution rules are exercised end-to-end through
 * the actual parser, not a re-implemented fake. Fixtures:
 *   /search/imdb/tt0111161 → The Shawshank Redemption (tmdb 278)
 *   /search/movie?query=Dune → Dune 2021 (tmdb 438631) + Dune 1984 (tmdb 841) decoy
 */
class TraktIdResolverSpec extends AnyFlatSpec with Matchers {

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  /** Real TraktClient over a fake fetch routing by url substring → body. */
  private def resolver(responses: Map[String, String]): TraktIdResolver =
    new TraktIdResolver(new TraktClient(new HttpFetch {
      def get(url: String): String = responses
        .collectFirst { case (key, body) if url.contains(key) => body }
        .getOrElse("[]")
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("TraktClient should not POST")
    }, apiKey = Some("cid")))

  // ── id bridge: known imdbId → tmdbId (exact, no corroboration) ────────────────

  "resolve" should "convert a known imdb id into a tmdb id via the exact id bridge" in {
    val r = resolver(Map("search/imdb/tt0111161" -> loadFixture("/fixtures/trakt/search_imdb_tt0111161.json")))
    r.resolve(imdbId = Some("tt0111161"), titles = Seq("The Shawshank Redemption"), year = Some(1994)) shouldBe
      TraktResolution(tmdbId = Some(278), imdbId = Some("tt0111161"))
  }

  it should "prefer the id bridge over the title search when an imdb id is present" in {
    // Title search fixture is present but must not be consulted — the id bridge wins.
    val r = resolver(Map(
      "search/imdb/tt0111161" -> loadFixture("/fixtures/trakt/search_imdb_tt0111161.json"),
      "search/movie"          -> """[{"movie":{"title":"Wrong","year":1900,"ids":{"tmdb":999}}}]""",
    ))
    r.resolve(Some("tt0111161"), Seq("The Shawshank Redemption"), Some(1994)).tmdbId shouldBe Some(278)
  }

  // ── title search fallback: corroborated LONE match ───────────────────────────

  it should "resolve tmdb id by corroborated title+year, filtering the wrong-year decoy" in {
    val r = resolver(Map("search/movie" -> loadFixture("/fixtures/trakt/search_movie_dune.json")))
    // Only the 2021 Dune survives the year gate; the 1984 decoy is discarded.
    r.resolve(imdbId = None, titles = Seq("Dune"), year = Some(2021)) shouldBe
      TraktResolution(tmdbId = Some(438631), imdbId = Some("tt1160419"))
  }

  it should "REFUSE to guess when two same-title films remain and no year disambiguates" in {
    // Both Dune entries have the exact title; without a year both survive → ambiguous.
    val r = resolver(Map("search/movie" -> loadFixture("/fixtures/trakt/search_movie_dune.json")))
    r.resolve(imdbId = None, titles = Seq("Dune"), year = None) shouldBe TraktResolution(None, None)
  }

  it should "return empty when the title does not match exactly" in {
    val r = resolver(Map("search/movie" -> loadFixture("/fixtures/trakt/search_movie_dune.json")))
    r.resolve(imdbId = None, titles = Seq("Duna: Part Three"), year = Some(2021)) shouldBe TraktResolution(None, None)
  }

  it should "return empty when Trakt has nothing" in {
    resolver(Map.empty).resolve(Some("tt9999999"), Seq("Nonexistent"), Some(2030)) shouldBe TraktResolution(None, None)
  }
}
