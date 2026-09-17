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

  /** Regression for the 2026-09-15 `PolandConvergenceSpec` settle-determinism
   *  failure: a period-split was tried alongside the colon-split, and
   *  "Vincent. Legenda oceanu- seans" (Kino Stary Młyn's own listing, not a
   *  programme banner) split into a spurious "Legenda oceanu- seans"
   *  candidate. That raced the ALREADY-AMBIGUOUS bare title "Vincent. Legenda
   *  oceanu" (TMDB itself returns different films for it depending on which
   *  year happens to be attached at resolution time) and made a
   *  settled-corpus re-settle fold a row that hadn't folded on the previous
   *  pass. A period is common, legitimate mid-title punctuation in this
   *  corpus (unlike a colon), so it's not split on. */
  it should "NOT split on a period — only a colon marks a programme banner" in {
    val found = SearchTitles.candidates("Vincent. Legenda oceanu- seans", None)
    found should not contain "Legenda oceanu- seans"
    found should contain("Vincent. Legenda oceanu- seans")
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

  "wholeCandidates" should "keep only the reported titles, dropping every de-decorated split" in {
    // UK convergence, 2026-09-15→17: `candidates`' banner-split off "The Hunger
    // Games: Catching Fire" ("Catching Fire" alone) is a fragment, not one of the
    // row's own reported titles, and `TmdbCandidateSearch.searchUnique`'s
    // director-less branch needs to tell the two apart.
    val found = SearchTitles.wholeCandidates("The Hunger Games: Catching Fire", None)
    found should contain only "The Hunger Games: Catching Fire"

    val dashed = SearchTitles.wholeCandidates("The Hunger Games: Mockingjay - Part 2", None)
    dashed should contain only "The Hunger Games: Mockingjay - Part 2"
  }

  it should "still include the original title and extra titles as reported, unsplit" in {
    val found = SearchTitles.wholeCandidates(
      "Akademia Kina Polskiego: Człowiek z żelaza", Some("Man of Iron"), Seq("Ktoś inny: Wariant"))
    found should contain theSameElementsAs Seq(
      "Akademia Kina Polskiego: Człowiek z żelaza", "Man of Iron", "Ktoś inny: Wariant")
  }
}
