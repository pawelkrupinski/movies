package tools

import clients.tools.{FakeHttpFetch, RecordingHttpFetch}
import org.scalatest.flatspec.AnyFlatSpec
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

  it should "still throw for a non-search endpoint, so real gaps are loud" in {
    val lenient = new FakeHttpFetch("does-not-exist-anywhere")
    a [java.io.FileNotFoundException] should be thrownBy lenient.get("https://www.metacritic.com/movie/x")
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
