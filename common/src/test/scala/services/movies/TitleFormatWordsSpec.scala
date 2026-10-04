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

  "A generic version token" should "be the country's own spelling" in {
    ScreeningTokens.of(models.Country.Poland).normalize(Seq("2D", "SUB")) shouldBe List("2D", "NAP")
    ScreeningTokens.of(models.Country.UnitedKingdom).normalize(Seq("SUB", "DUB")) shouldBe List("SUB", "DUB")
    ScreeningTokens.of(models.Country.Spain).normalize(Seq("SUB")) shouldBe List("VOSE")
  }
}
