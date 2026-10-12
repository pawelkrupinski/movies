package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** A festival or film club bills its screenings of an ordinary film with its ACRONYM — a numbered
 *  edition before the film ("11. UFF - Demony", "19. FGA: Szepty lasu") or a pipe tag after it
 *  ("Atlantyda | UFF", "Frances Ha | DKF"). The tag is the screening's, not the film's: the card shows
 *  the film's own title, the screening keys with the film's other listings, and the film is looked up
 *  bare. Every title here is a venue's own, from kinowo.net's Polish pages on 2026-10-07 (the 11th
 *  Ukrainian Film Festival at Kino Amondo / Kinoteka). */
class FestivalTagSpec extends AnyFlatSpec with Matchers {

  private val n = SingleCountryNormalizer.titleNormalizer

  private val tagged = Seq(
    "11. UFF - Demony"                -> "Demony",
    "11. UFF - Za zwycięstwo"         -> "Za zwycięstwo",
    "11. UFF – Rzadko budzę się śniąc" -> "Rzadko budzę się śniąc",
    "19. FGA: Szepty lasu"            -> "Szepty lasu",
    "Atlantyda | UFF"                 -> "Atlantyda",
    "Odblask | UFF"                   -> "Odblask",
    "Frances Ha | DKF"                -> "Frances Ha",
    "Róża |DKF"                       -> "Róża",
    "Camino dla opornych | CHKF"      -> "Camino dla opornych",
    "Egon Schiele. Poza tabu | FKS"   -> "Egon Schiele. Poza tabu",
    // A year-numbered edition: ACME Screening Room's Lighthouse Horror Film Festival (US), 2026-10.
    "2026 LHFF Night of the Living Dead" -> "Night of the Living Dead",
    "2026 LHFF The Wicker Man"          -> "The Wicker Man")

  "A festival-tagged listing" should "show the film's own title, even when no other venue lists the film" in {
    tagged.foreach { case (listing, film) =>
      withClue(s"preferredDisplay('$listing'): ")(n.preferredDisplay(Seq(listing)) shouldBe Some(film))
      withClue(s"chooseDisplay('$listing'): ")(n.chooseDisplay(Seq(listing), listing) shouldBe film)
    }
  }

  it should "key with the film's other listings" in {
    tagged.foreach { case (listing, film) =>
      withClue(s"sanitize('$listing'): ")(n.sanitize(listing) shouldBe n.sanitize(film))
    }
  }

  it should "be looked up as the film" in {
    tagged.foreach { case (listing, film) =>
      withClue(s"apiQuery('$listing'): ")(n.apiQuery(listing) shouldBe film)
    }
  }

  it should "be looked up as the film when the edition is a year and the film carries its own" in {
    n.apiQuery("2026 LHFF Scream (1996)") shouldBe n.apiQuery("Scream (1996)")
    n.sanitize("2026 LHFF Scream (1996)") shouldBe n.sanitize("Scream (1996)")
  }

  it should "keep what follows the tag that is the screening's own" in {
    // A nested strand, an accessibility tag and an event stay — only the festival's tag goes.
    n.preferredDisplay(Seq("11. UFF - BKF: Łabędzi śpiew Fiodora Ozerowa")) shouldBe Some("BKF: Łabędzi śpiew Fiodora Ozerowa")
    n.preferredDisplay(Seq("11. UFF - Ślady - AD/SDH")) shouldBe Some("Ślady - AD/SDH")
    n.preferredDisplay(Seq("11. UFF - Gala otwarcia + Demony")) shouldBe Some("Gala otwarcia + Demony")
  }

  "A title that only looks tagged" should "keep its whole title" in {
    Seq(
      "1. FC Köln - Der Film",            // an acronym that runs on into the name — no separator after it
      "32. MFSP A PART - Trixie",         // a festival named past its acronym: not this shape, left alone
      "100 LAT Z MARILYN. ŚWIATŁO I CIEŃ | Słomiany wdowiec | prelekcja + pokaz",
      "KINO SENIORA | LALKA",             // a banner BEFORE the pipe: the capitals after it are the film
      "Lalka | PJM",                      // a signed screening is its own audience's card
      "Lalka | SDH",
      "Lalka | ENG",                      // an English-subtitled screening likewise
      "Kino seniora | Renoir",
      "UFF",
      "11. UFF - ",
      "2001 A Space Odyssey",             // a year then a one-letter word: the film's own title
      "2001 Maniacs",                     // a year then an ordinary word
      "2001 MANIACS",                     // seven capitals: a word, not an acronym
      "1776 THE MUSICAL",                 // an all-capitals listing: nothing lower-case says a title follows
      "1917",
      "2046",
      "2026 LHFF"                         // a tag and nothing after it
    ).foreach { t =>
      withClue(s"canonical('$t'): ")(n.preferredDisplay(Seq(t)) shouldBe Some(t.trim))
    }
  }
}
