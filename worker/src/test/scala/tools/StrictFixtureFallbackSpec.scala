package tools

import clients.tools.{FakeHttpFetch, RecordingHttpFetch}
import org.scalatest.flatspec.AnyFlatSpec
import tools.HttpStatusException
import org.scalatest.matchers.should.Matchers

/**
 * The two properties a fixture-FIRST enrichment fetch depends on. Both are
 * load-bearing and neither is obvious.
 */
class StrictFixtureFallbackSpec extends AnyFlatSpec with Matchers {

  private val searchUrl = "https://api.themoviedb.org/3/search/movie?query=Nieistniejacy+film&api_key=k"

  // The default behaviour: a TMDB search miss is SYNTHESISED as an empty result.
  // Correct for the recorded corpus, and fatal behind a fallback — a wrapper sees
  // success and never reaches the live leg, so every unrecorded query resolves to
  // nothing. That is the "everything is tmdbNoMatch" failure, via the replay layer.
  "the default fixture fetch" should "synthesise an empty TMDB search rather than fail" in {
    val lenient = new FakeHttpFetch("does-not-exist-anywhere")
    lenient.get(searchUrl) should include ("\"total_results\":0")
  }

  it should "still throw for a non-search, non-probed endpoint, so real gaps are loud" in {
    val lenient = new FakeHttpFetch("does-not-exist-anywhere")
    // A cinema listing page: one URL, one recording, no probing. A missing
    // fixture here is unambiguously a recording gap.
    a [java.io.FileNotFoundException] should be thrownBy lenient.get("https://www.kinopodbaranami.pl/repertuar")
  }

  // The rating resolvers find a page by PROBING candidate slugs — ~20 per title
  // for Metacritic/RT, ~55 candidate films for Filmweb — of which at most one
  // exists, so the recorder can only ever capture that one. Every losing
  // candidate is unrecorded BY CONSTRUCTION and answers 404 in production. The
  // clients now tell a 404 ("no such page, try the next candidate") apart from a
  // failed read ("upstream is down, stop") — see tools.EnrichmentRead — so the
  // fake has to draw the same line or the first losing probe aborts the ladder.
  it should "replay a 404 for an unrecorded page on a probed rating host" in {
    val lenient = new FakeHttpFetch("does-not-exist-anywhere")
    Seq("https://www.metacritic.com/movie/x",
        "https://www.rottentomatoes.com/m/x",
        "https://www.filmweb.pl/api/v1/film/1/info").foreach { url =>
      withClue(s"$url: ") {
        val thrown = the [HttpStatusException] thrownBy lenient.get(url)
        thrown.code shouldBe 404
      }
    }
  }

  // IMDb answers an unknown title id with HTTP 200 and a null title, and an
  // unknown suggestion query with an empty candidate list — never an error. An
  // unrecorded id/query is simply one never asked during recording, so replay
  // the real "nothing here" body rather than failing the whole enrichment chain.
  it should "replay IMDb's empty answers rather than throwing" in {
    val lenient = new FakeHttpFetch("does-not-exist-anywhere")
    lenient.post("https://caching.graphql.imdb.com/", """{"query":"x"}""", "application/json") should
      include ("\"title\":null")
    lenient.get("https://v3.sg.media-imdb.com/suggestion/x/unknown.json") should include ("\"d\":[]")
  }

  // Strict mode is what makes a fallback possible at all.
  "a strict fixture fetch" should "throw on a TMDB search miss so a fallback can fire" in {
    val strict = new FakeHttpFetch("does-not-exist-anywhere", strict = true)
    a [java.io.FileNotFoundException] should be thrownBy strict.get(searchUrl)
  }

  // Enrichment fixtures must keep the year: the year-scoped and yearless searches
  // return materially different bodies and TmdbClient depends on the difference.
  "the fixture key" should "fold the year by default, matching the cinema corpus" in {
    RecordingHttpFetch.stableQueryFingerprint("query=Dune&year=2021") shouldBe
      RecordingHttpFetch.stableQueryFingerprint("query=Dune")
  }

  it should "keep the year when folding is disabled, for enrichment fixtures" in {
    RecordingHttpFetch.stableQueryFingerprint("query=Dune&year=2021", foldYear = false) should not be
      RecordingHttpFetch.stableQueryFingerprint("query=Dune", foldYear = false)
  }

  it should "still ignore credentials whether or not the year is folded" in {
    RecordingHttpFetch.stableQueryFingerprint("query=Dune&api_key=aaa", foldYear = false) shouldBe
      RecordingHttpFetch.stableQueryFingerprint("query=Dune&api_key=bbb", foldYear = false)
  }
}
