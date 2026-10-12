package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Format words venues write into a title beyond Poland's own (2026-10 corpus survey): "Resident evil 2d sub"
 *  (Cinema N 3D), "Spirited Away (Dubbed)" and "Yuri!!! on Ice (Subtitled)" (US), "Collateral (35mm)" and
 *  "The Odyssey | 70MM IMAX" (UK), "Klasyka na TOPie: Absolwent (4K)". Each comes off the title and becomes
 *  its screening's token — the version one in the COUNTRY's own spelling. */
class TitleFormatWordsSpec extends AnyFlatSpec with Matchers {

  "A title's format words" should "come off it as tokens" in {
    FormatTags.extractFormatTags("Resident evil 2d sub") shouldBe ("Resident evil", List("2D", "SUB"))
    FormatTags.extractFormatTags("Spirited Away (Dubbed)") shouldBe ("Spirited Away", List("DUB"))
    FormatTags.extractFormatTags("Yuri!!! on Ice (Subtitled)") shouldBe ("Yuri!!! on Ice", List("SUB"))
    FormatTags.extractFormatTags("Collateral (35mm)") shouldBe ("Collateral", List("35MM"))
    FormatTags.extractFormatTags("The Odyssey | 70MM IMAX") shouldBe ("The Odyssey", List("70MM", "IMAX"))
    FormatTags.extractFormatTags("Klasyka na TOPie: Absolwent (4K)")._2 shouldBe List("4K")
  }

  they should "take the preposition a print format is billed with" in {
    // US Alamo "TERROR TUESDAY: THE EXORCIST - ON 35MM": "35MM" alone left "… - ON", searched as "The exorcist - on".
    FormatTags.extractFormatTags("TERROR TUESDAY: THE EXORCIST - ON 35MM") shouldBe ("TERROR TUESDAY: THE EXORCIST", List("35MM"))
    FormatTags.extractFormatTags("Lawrence of Arabia in 70mm") shouldBe ("Lawrence of Arabia", List("70MM"))
  }

  they should "take the preposition a screen format is billed with" in {
    // The corpora of 2026-10-12: "3D" alone left "… The Tour Live In", "Carmen in", "… Premieres in".
    FormatTags.extractFormatTags("Billie Eilish - Hit Me Hard And Soft: The Tour Live In 3D") shouldBe
      ("Billie Eilish - Hit Me Hard And Soft: The Tour Live", List("3D"))
    FormatTags.extractFormatTags("Carmen in 3D") shouldBe ("Carmen", List("3D"))
    FormatTags.extractFormatTags("Dune: Part Two Fan First Premieres in IMAX") shouldBe ("Dune: Part Two Fan First Premieres", List("IMAX"))
    FormatTags.extractFormatTags("Avatar w 3D") shouldBe ("Avatar", List("3D"))
  }

  // Balagueró's "[REC]" (US corpus 2026-10-01) came out as no title at all: served as an empty card.
  they should "never take a tag that is the whole title" in {
    FormatTags.extractFormatTags("[REC]") shouldBe ("[REC]", Nil)
    FormatTags.extractFormatTags("[REC] 3D") shouldBe ("[REC]", List("3D"))
    FormatTags.extractFormatTags("(IMAX 3D)")._1 shouldBe "(IMAX 3D)"
    FormatTags.extractFormatTags("Diuna [3D]") shouldBe ("Diuna", List("3D"))
  }

  they should "leave a word inside the title its own" in {
    // "4K Restoration" names the edition, not the screen: the trailing word is no format.
    FormatTags.extractFormatTags("Horror Season 2026 Dracula 4K Restoration") shouldBe ("Horror Season 2026 Dracula 4K Restoration", Nil)
    FormatTags.extractFormatTags("Sub Rosa")._1 shouldBe "Sub Rosa"
    // Bracketed too: Everyman's "Dracula (4K Restoration)" is the restoration's title, and the UK's ×81
    // "Horror Season 2026 Dracula 4K Restoration" merges with it only while it keeps it.
    FormatTags.extractFormatTags("Dracula (4K Restoration)") shouldBe ("Dracula (4K Restoration)", Nil)
  }

  // German venues (Cineamo / kinoheld / Filmstarts, prod corpus 2026-10-12) write the language version into the
  // title in front, behind or in brackets, and every spelling became its own card of the one film: "Digger" showed
  // as 8 cards, "Verity" as 15. The version is the screening's, so it comes off as its token.
  "A German language-version word" should "come off a title as its token, wherever the venue writes it" in {
    FormatTags.extractFormatTags("Digger (OV)") shouldBe ("Digger", List("OV"))
    FormatTags.extractFormatTags("Digger (OmU)") shouldBe ("Digger", List("OmU"))
    FormatTags.extractFormatTags("Digger - OmU") shouldBe ("Digger", List("OmU"))
    FormatTags.extractFormatTags("DIGGER -OmU") shouldBe ("DIGGER", List("OmU"))
    FormatTags.extractFormatTags("Digger OmU") shouldBe ("Digger", List("OmU"))
    FormatTags.extractFormatTags("VATERLAND (OmeU)") shouldBe ("VATERLAND", List("OmeU"))
    FormatTags.extractFormatTags("Shaun das schaf - spuk im kürbisfeld df") shouldBe ("Shaun das schaf - spuk im kürbisfeld", List("DF"))
    FormatTags.extractFormatTags("The Odyssey OV") shouldBe ("The Odyssey", List("OV"))
    FormatTags.extractFormatTags("Die Odyssee - engl. OV") shouldBe ("Die Odyssee", List("OV"))
    FormatTags.extractFormatTags("Digger, OV english") shouldBe ("Digger", List("OV"))
    FormatTags.extractFormatTags("Verity (Original English Version)") shouldBe ("Verity", List("OV"))
    FormatTags.extractFormatTags("The Rocky Horror Picture Show (1975/Original English Version)") shouldBe ("The Rocky Horror Picture Show", List("OV"))
  }

  // The DE recording of 2026-10-12: a version glued to the title by a bare hyphen (Kino Utopia), or spelled as one word
  // (Das Lumen Solingen, CineStar Cubix's "(Hebr. Originalversion mit engl. UT)").
  it should "come off glued to the title, or spelled as one word" in {
    FormatTags.extractFormatTags("Glaube-OmU") shouldBe ("Glaube", List("OmU"))
    FormatTags.extractFormatTags("Dune: Part Three Originalversion") shouldBe ("Dune: Part Three", List("OV"))
    FormatTags.extractFormatTags("Checkout (Hebr. Originalversion mit engl. UT)") shouldBe ("Checkout", List("OV"))
    // Only a version word glued at the very end: a hyphenated title keeps its hyphen.
    FormatTags.extractFormatTags("Spider-Man")._1 shouldBe "Spider-Man"
    FormatTags.extractFormatTags("X-OVerdrive")._1 shouldBe "X-OVerdrive"
  }

  it should "come off in FRONT of the title too, alone or beside a screen format" in {
    FormatTags.extractFormatTags("OV: Digger") shouldBe ("Digger", List("OV"))
    FormatTags.extractFormatTags("OmU: Rascal Does Not Dream of a Dear Friend") shouldBe ("Rascal Does Not Dream of a Dear Friend", List("OmU"))
    FormatTags.extractFormatTags("(OmU) Verity - Dunkle Geheimnisse") shouldBe ("Verity - Dunkle Geheimnisse", List("OmU"))
    FormatTags.extractFormatTags("ATMOS/OV: Verity - Dunkle Geheimnisse") shouldBe ("Verity - Dunkle Geheimnisse", List("ATMOS", "OV"))
    FormatTags.extractFormatTags("Dolby Cinema/OV: Verity - Dunkle Geheimnisse") shouldBe ("Verity - Dunkle Geheimnisse", List("OV"))
    FormatTags.extractFormatTags("Dolby Cinema: Die Odyssee") shouldBe ("Die Odyssee", Nil)
    FormatTags.extractFormatTags("3D: Forgotten Island") shouldBe ("Forgotten Island", List("3D"))
    FormatTags.extractFormatTags("(2D) Forgotten Island") shouldBe ("Forgotten Island", List("2D"))
    FormatTags.extractFormatTags("Forgotten Island (3-D)") shouldBe ("Forgotten Island", List("3D"))
  }

  // The DE re-record of 2026-10-12 (run 38251516862): Astor Grand Cinema's language-qualified version, Bali-Kino's
  // and Mathäser's version closed by a spaced dash, Mathäser's bracketed D-BOX seat note.
  it should "come off in front closed by a dash, with its language qualifier, and behind a D-BOX note" in {
    FormatTags.extractFormatTags("engl. OV: Der Herr der Ringe: Die Gefährten-Extended Version") shouldBe
      ("Der Herr der Ringe: Die Gefährten-Extended Version", List("OV"))
    FormatTags.extractFormatTags("OMU - SEARCHING FOR SUGAR MAN") shouldBe ("SEARCHING FOR SUGAR MAN", List("OmU"))
    FormatTags.extractFormatTags("DF: OV - Dune Part I + II - Atmos (auch in D-BOX)") shouldBe ("Dune Part I + II", List("DF", "OV", "ATMOS"))
    FormatTags.extractFormatTags("DF: Dune: Teil 1 + 2 - Atmos (auch in D-BOX)") shouldBe ("Dune: Teil 1 + 2", List("DF", "ATMOS"))
    // A dash after a film's own first word stays: only a run of version words closes there.
    FormatTags.extractFormatTags("Ovid - The Movie")._1 shouldBe "Ovid - The Movie"
    FormatTags.extractFormatTags("English - Vinglish")._1 shouldBe "English - Vinglish"
  }

  it should "leave a title that only contains such letters, or is nothing but the tag, whole" in {
    FormatTags.extractFormatTags("Johnny English")._1 shouldBe "Johnny English"
    FormatTags.extractFormatTags("The English Patient")._1 shouldBe "The English Patient"
    FormatTags.extractFormatTags("Ivanov")._1 shouldBe "Ivanov"
    FormatTags.extractFormatTags("Original")._1 shouldBe "Original"
    FormatTags.extractFormatTags("OV: ")._1 shouldBe "OV"
    FormatTags.extractFormatTags("(OmU)")._1 shouldBe "(OmU)"
    // A colon after a word that is not a version stays the title's own.
    FormatTags.extractFormatTags("Dune: Part Two")._1 shouldBe "Dune: Part Two"
    // "3-D" outside brackets is a film's own title ("Jaws 3-D"), only a bracketed one is the screen's.
    FormatTags.extractFormatTags("Jaws 3-D")._1 shouldBe "Jaws 3-D"
  }

  "A generic version token" should "be the country's own spelling" in {
    ScreeningTokens.of(models.Country.Poland).normalize(Seq("2D", "SUB")) shouldBe List("2D", "NAP")
    ScreeningTokens.of(models.Country.UnitedKingdom).normalize(Seq("SUB", "DUB")) shouldBe List("SUB", "DUB")
    ScreeningTokens.of(models.Country.Spain).normalize(Seq("SUB")) shouldBe List("VOSE")
  }
}
