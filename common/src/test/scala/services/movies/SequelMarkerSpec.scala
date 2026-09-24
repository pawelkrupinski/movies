package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pairs come from nine days of prod re-key logs (2026-08-29 → 09-06): the ones
 *  that folded wrongly and the ones that folded rightly and must keep doing so. */
class SequelMarkerSpec extends AnyFlatSpec with Matchers {

  private def toks(s: String): Seq[String] =
    tools.TextNormalization.deburr(s).toLowerCase.split("[^\\p{L}\\p{N}]+").filter(_.nonEmpty).toSeq

  private def anotherEntry(base: String, whole: String) = SequelMarker.namesAnotherEntry(toks(base), toks(whole))

  "SequelMarker" should "refuse a sequel that carries the base title as a prefix" in {
    anotherEntry("The Hunger Games", "The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)") shouldBe true
    anotherEntry("The Hunger Games", "The Hunger Games: Mockingjay Part 1")            shouldBe true
    anotherEntry("Toy Story",        "Toy Story 5")                                     shouldBe true
    anotherEntry("Rocky",            "Rocky II")                                        shouldBe true
    anotherEntry("Blade Runner",     "Blade Runner 2049")                               shouldBe true
    anotherEntry("Star Wars",        "Star Wars: Episode IV")                           shouldBe true
    anotherEntry("Szybcy i wściekli", "Szybcy i wściekli: część 8")                     shouldBe true
    anotherEntry("Dune",             "Dune: Part Two")                                  shouldBe true
    anotherEntry("Diuna",            "Diuna: Część druga")                               shouldBe true
    anotherEntry("Wicked",           "Wicked: Part Three")                               shouldBe true
  }

  // UK convergence, 2026-09-16: "The Hunger Games: Catching Fire" carries no
  // ordinal or part-marker at all — the SUBTITLE changes instead of a number —
  // so neither existing check above caught it, and the containment edge
  // (`TitleContainment.decorates`) folded it onto the resolved "The Hunger
  // Games" (2012) row exactly like a genuine decoration. Which processing
  // order discovered the row first then decided whether Catching Fire's
  // screenings folded onto the original or stayed their own — the same
  // order-dependence shape as the already-fixed Mockingjay case above, just
  // for a franchise entry that renames itself instead of numbering itself.
  it should "refuse a same-franchise entry whose subtitle changes instead of numbering itself" in {
    anotherEntry("The Hunger Games", "The Hunger Games: Catching Fire") shouldBe true
    anotherEntry("The Hunger Games", "The Hunger Games: The Ballad of Songbirds and Snakes") shouldBe true
  }

  // UK convergence, 2026-09-15→17: a newly-trending, not-yet-released franchise
  // entry ("Sunrise on the Reaping", also Francis Lawrence) shares "the hunger
  // games" with every other entry but names none of them — a bare, rerelease-
  // year-stamped listing for "Catching Fire"/"Mockingjay - Part 1"/"Part 2" let
  // `TmdbCandidateSearch.directorWalk`'s year-pinned tier resolve straight to
  // it, because nothing here told `isDifferentInstalment` these were DIFFERENT
  // entries rather than the same one under an unfamiliar subtitle.
  it should "refuse a same-franchise entry that hasn't released yet, sharing only the franchise prefix" in {
    anotherEntry("The Hunger Games", "The Hunger Games: Sunrise on the Reaping") shouldBe true
  }

  // US prod, 2026-09-16: "Bring It On: All or Nothing" (2006, dir. Steve Rash)
  // folded onto the resolved "Bring It On" (2000, dir. Peyton Reed) the same
  // way as Catching Fire above — caught by `CinemaCorroboration`'s director
  // contradiction rather than a re-key log.
  it should "refuse Bring It On's own subtitled sequel" in {
    anotherEntry("Bring It On", "Bring It On: All or Nothing") shouldBe true
  }

  it should "let a decorated screening of the same film fold" in {
    anotherEntry("Toy Story 5",              "Toddler Club: Toy Story 5")               shouldBe false
    anotherEntry("Fallen Angels by Noel Coward", "GB: Fallen Angels by Noel Coward")    shouldBe false
    anotherEntry("The Matrix",               "Cineworld 30: The Matrix")                shouldBe false
    anotherEntry("Casablanca",               "Casablanca 1942")                          shouldBe false
    anotherEntry("Ojczyzna",                 "Ojczyzna - pokaz przedpremierowy 2026")    shouldBe false
    anotherEntry("Top Gun",                  "Top Gun - Re-Release")                     shouldBe false
    anotherEntry("Cars 20th Anniversary",    "Toddler Club Cars 20th Anniversary")       shouldBe false
  }

  private def different(a: String, b: String) =
    SequelMarker.differentInstalments(TitleContainment.tokens(a), TitleContainment.tokens(b))

  it should "tell same-length sibling instalments apart, whichever side is asked first" in {
    // The UK convergence flip (2026-09-10): "Mockingjay - Part 1" and "Part 2" are one
    // character apart once sanitized, well inside `TitleMatch.close`'s edit-distance
    // bound — this is the guard that keeps them from tying under it.
    different("The Hunger Games: Mockingjay - Part 1", "The Hunger Games: Mockingjay - Part 2") shouldBe true
    different("The Hunger Games: Mockingjay - Part 2", "The Hunger Games: Mockingjay - Part 1") shouldBe true
    different("Rocky II", "Rocky III")                                                          shouldBe true
    different("Kingsman 2", "Kingsman 3")                                                        shouldBe true
  }

  it should "still let genuine spelling drift of the SAME film through" in {
    different("Guru", "Gourou")                                             shouldBe false
    different("The Hunger Games: Mockingjay - Part 2", "The Hunger Games: Mockingjay - Part 2") shouldBe false
  }

  it should "fall back to the containment check when the two run different lengths" in {
    different("Toy Story", "Toy Story 5")                     shouldBe true
    different("Toy Story 5", "Toddler Club: Toy Story 5")     shouldBe false
  }

  it should "still catch the divergent ordinal despite a typo earlier in the title" in {
    // A stray typo elsewhere ("Mockinjay") does not consume the ≤2-edit budget
    // `titleClose` already spent judging the titles close — the trailing
    // ordinal alone has to carry the signal, so this must NOT require every
    // other token to match exactly.
    different("The Hunger Games: Mockinjay - Part 1", "The Hunger Games: Mockingjay - Part 2") shouldBe true
  }

  it should "not mistake the SAME instalment numbered two different ways for two films" in {
    // `MortalKombatDisappearanceSpec`, broken by the first cut of this guard
    // (2026-09-10): Multikino reports "Mortal Kombat 2", TMDB's own credit reads
    // "Mortal Kombat II" — one film, arabic vs roman for the same number 2.
    different("Mortal Kombat 2", "Mortal Kombat II")   shouldBe false
    different("Mortal Kombat II", "Mortal Kombat 2")   shouldBe false
    different("Dune: Part 2", "Dune: Part Two")         shouldBe false
  }

  // UK convergence, 2026-09-16, round two: closing the containment edge (above) let a
  // DIFFERENT gap in the SAME franchise surface — "Catching Fire" and "Mockingjay -
  // Part 2" don't contain each other at all (neither's tokens run along the other's),
  // so `namesAnotherEntry`'s prefix/suffix check never even engages; they only share
  // the franchise's common prefix. That's exactly the SIBLING shape `differentInstalments`
  // exists to catch, and its own fallback — `namesAnotherEntry` on whichever side is
  // shorter — needs one of the two to be a token-run of the other too, so it missed this
  // pair the same way. `directorWalk`'s fuzzy match ties them once titleClose overall,
  // pinning Catching Fire's screenings onto whichever Mockingjay part resolved first —
  // observed in prod as the resolved row's YEAR flipping between the two films' real
  // years depending on processing order.
  it should "tell apart two same-franchise siblings that share only their common prefix" in {
    different("The Hunger Games: Catching Fire", "The Hunger Games: Mockingjay - Part 2") shouldBe true
    different("The Hunger Games: Mockingjay - Part 2", "The Hunger Games: Catching Fire") shouldBe true
    different("The Hunger Games: Catching Fire", "The Hunger Games: Mockingjay - Part 1") shouldBe true
    different("The Hunger Games: The Ballad of Songbirds and Snakes",
              "The Hunger Games: Mockingjay - Part 1") shouldBe true
  }

  // UK convergence, 2026-09-15→17: same shape as the pair above, but against the
  // franchise's newest, not-yet-released entry — the one `directorWalk`'s year-
  // pinned tier actually resolved a bare rerelease listing to (its 2026 release
  // year, the only signal a rerelease-stamped listing carries, uniquely pins his
  // filmography to this one credit).
  it should "tell the not-yet-released franchise entry apart from its siblings too" in {
    different("The Hunger Games: Catching Fire", "The Hunger Games: Sunrise on the Reaping") shouldBe true
    different("The Hunger Games: Sunrise on the Reaping", "The Hunger Games: Catching Fire") shouldBe true
    different("The Hunger Games: Mockingjay - Part 1", "The Hunger Games: Sunrise on the Reaping") shouldBe true
    different("The Hunger Games: Mockingjay - Part 2", "The Hunger Games: Sunrise on the Reaping") shouldBe true
  }

  // UK convergence run 35948292875 (2026-09-24): the Flicks listings name ONE film
  // "The Hunger Games: Mockingjay - Part 1 (2026)" at 64 venues and plain "... - Part 1"
  // at 14. Both qualify as an entry against the curated base and their extras differ —
  // only by the rerelease year — which the sibling check read as two DIFFERENT entries,
  // so MixedFilmDetector split the 14 off every settle. A decoration on the same entry is
  // not a sibling: the entry each title names (subtitle words + instalment number) must differ.
  it should "not read one curated entry beside its own decorated listing as two siblings" in {
    def siblings(a: String, b: String) = SequelMarker.curatedSiblingTitles(Seq(a), Seq(b))
    siblings("The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 1") shouldBe false
    siblings("The Hunger Games: Mockingjay - Part 2 (2015)", "The Hunger Games: Mockingjay - Part 2") shouldBe false
    siblings("The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)", "The Hunger Games: Mockingjay - Part 2") shouldBe false
    different("The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 1") shouldBe false
    siblings("The Hunger Games: The Ballad of Songbirds & Snakes",
             "The Hunger Games: The Ballad of Songbirds and Snakes") shouldBe false
    // ...while the entries that really differ still do, decorated or not.
    siblings("The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Mockingjay - Part 2") shouldBe true
    siblings("The Hunger Games: Mockingjay - Part 1 (2026)", "The Hunger Games: Catching Fire") shouldBe true
  }
}
