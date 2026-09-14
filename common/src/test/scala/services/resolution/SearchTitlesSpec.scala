package services.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `SearchTitles.candidates` is pure candidate-string generation — no normaliser,
 *  no lookups — so these assert directly against the returned strings. */
class SearchTitlesSpec extends AnyFlatSpec with Matchers {

  "candidates" should "split a colon-banner-prefixed programme title, keeping the film's own title" in {
    val found = SearchTitles.candidates("Akademia Kina Polskiego: Człowiek z żelaza (1981) 4K", None)
    found should contain("Człowiek z żelaza (1981) 4K")
    // undivided form stays a candidate too
    found should contain("Akademia Kina Polskiego: Człowiek z żelaza (1981) 4K")
  }

  it should "split a double-decorated colon-AND-period programme banner down to the film title" in {
    val found = SearchTitles.candidates("3 wieczory: kieślowski. Blizna", None)
    found should contain("Blizna")
  }

  it should "not split on a colon with nothing meaningful after it" in {
    val found = SearchTitles.candidates("Trailing banner:", None)
    found should not contain ""
    found should contain only "Trailing banner:"
  }

  it should "not split on a colon with nothing meaningful before it" in {
    val found = SearchTitles.candidates(": Film", None)
    found should not contain ""
    // the banner-split would have produced an empty banner, so no extra
    // "Film"-only candidate is added — only the undivided form survives.
    found should contain only ": Film"
  }

  it should "keep the undivided title as a candidate when a colon appears mid-title legitimately" in {
    val found = SearchTitles.candidates("Kill Bill: Vol. 2: Redux", None)
    found should contain("Kill Bill: Vol. 2: Redux")
  }
}
