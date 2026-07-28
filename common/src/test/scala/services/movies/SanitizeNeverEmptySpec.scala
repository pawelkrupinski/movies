package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A film's identity is `sanitize(title)|year`, so a title `sanitize` reduces to the EMPTY
 * string has no identity at all — it keys as `|1957`, and EVERY such film sharing that
 * year collides onto the one `_id` and merges into one row.
 *
 * Reachable because the Canonical tier is a set of `^`-anchored banner rules, and a
 * cinema can list a film whose title is nothing BUT the banner: Kino Muza's
 * "Federico Fellini: ciao a tutti!" is a programme name that happens to be the whole
 * listing. Found live in the fixture corpus as `"_id" : "|1957"` (with its screening row
 * keyed `"|1957|krakow|Kino Agrafka"` behind it). Prod carried none at the time this was
 * written — that programme was not running — which is exactly why the guard is cheap now:
 * there is nothing to re-key.
 *
 * The guard falls back to the RAW title's key rather than inventing one, so the identity
 * stays a pure function of what the cinemas reported.
 */
class SanitizeNeverEmptySpec extends AnyFlatSpec with Matchers {
  import TitleNormalizer.sanitize

  // The whole string is a Canonical-tier banner. Kept as a `val` so the premise assertion
  // below fails loudly if the rules ever stop consuming it, rather than the guard quietly
  // going untested.
  private val whollyBanner = "Federico Fellini: ciao a tutti!"

  "sanitize" should "never reduce a non-empty title to an empty key" in {
    withClue("premise — the Canonical tier no longer consumes this title, so it no longer " +
             "exercises the guard; pick another wholly-banner title: ") {
      sanitize(s"$whollyBanner - Wałkonie") should not be empty // sanity: a decorated sibling keys fine
    }
    sanitize(whollyBanner) should not be empty
  }

  it should "give such a film a real `_id`, not a bare year" in {
    StoredMovieRecord.idFor(whollyBanner, Some(1957)) should not be "|1957"
    StoredMovieRecord.idFor(whollyBanner, Some(1957)) shouldBe s"${sanitize(whollyBanner)}|1957"
  }

  // The fallback is the RAW title's key — deburred, lower-cased, punctuation stripped —
  // not an invented one, so the identity stays a pure function of what the cinema
  // reported and the usual case/punctuation folding still applies to it.
  it should "fall back to the raw title's key, not invent one" in {
    sanitize(whollyBanner)                         shouldBe "federicofelliniciaoatutti"
    sanitize("  Federico Fellini: Ciao a Tutti!  ") shouldBe "federicofelliniciaoatutti"
  }

  // The guard neither merges nor splits: both spellings are wholly consumed, so before it
  // they shared the empty key and after it they share the raw one. Measured both ways
  // rather than assumed, because a change to the identity function that quietly re-keyed
  // rows would be the expensive kind of mistake.
  it should "keep two spellings of one wholly-banner title on the SAME key, as before" in {
    sanitize("FEDERICO FELLINI: CIAO A TUTTI!") shouldBe sanitize(whollyBanner)
  }

  // The reason the empty key matters: without it these two share an `_id` and the second
  // silently replaces the first.
  it should "keep two different wholly-banner titles on different ids" in {
    sanitize("DKF Rozpięci:") should not be sanitize(whollyBanner)
  }
}
