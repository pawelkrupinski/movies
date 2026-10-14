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
    FormatTags.extractFormatTags("ATMOS: Michael") shouldBe ("Michael", List("ATMOS"))
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

  // The UK box offices of 2026-10-13 bill the BBFC certificate in the title — Lanternhouse Cinema's "Digger (15)",
  // "Verity (15)", "Heart of the Beast (12A)" each showed as a card of its own beside the film, IMDb id and all; Act
  // One's "Digger - Cert 15", Electric Palace's "Digger (15) 129 mins", Hippodrome Bo'ness's "(15) 26" likewise. The
  // certificate is the showing's, never the film's, so it comes off with the running time or year a box office runs
  // on after it — yielding no token: the certificate is a field of its own.
  "A box office's certificate" should "come off the title, with the running time or short year billed after it" in {
    FormatTags.extractFormatTags("Digger (15)") shouldBe ("Digger", Nil)
    FormatTags.extractFormatTags("Verity (15)") shouldBe ("Verity", Nil)
    FormatTags.extractFormatTags("Heart of the Beast (12A)") shouldBe ("Heart of the Beast", Nil)
    FormatTags.extractFormatTags("Festive Fun with Peppa Pig (U)") shouldBe ("Festive Fun with Peppa Pig", Nil)
    FormatTags.extractFormatTags("Andre Rieu: Let it Snow (TBC)") shouldBe ("Andre Rieu: Let it Snow", Nil)
    FormatTags.extractFormatTags("Pressure (15 TBC)") shouldBe ("Pressure", Nil)
    FormatTags.extractFormatTags("Digger - Cert 15") shouldBe ("Digger", Nil)
    FormatTags.extractFormatTags("EOS - Monet (Cert. TBC)") shouldBe ("EOS - Monet", Nil)
    FormatTags.extractFormatTags("Digger (15) 129 mins") shouldBe ("Digger", Nil)
    FormatTags.extractFormatTags("De Gaulle Liberté (15) 26") shouldBe ("De Gaulle Liberté", Nil)
    // A release year after it stays, for the year reading and the canonical year strip.
    FormatTags.extractFormatTags("Bad Apples (15) (2026) 100 mins") shouldBe ("Bad Apples (2026)", Nil)
    FormatTags.extractFormatTags("Elf (2003) (PG) 26") shouldBe ("Elf (2003)", Nil)
    FormatTags.extractFormatTags("Its A Wonderful Life (U) (1946) 130 mins") shouldBe ("Its A Wonderful Life (1946)", Nil)
    // Stacked with a format tag, in either order.
    FormatTags.extractFormatTags("Digger (15) (Subtitled)") shouldBe ("Digger", List("SUB"))
    FormatTags.extractFormatTags("Digger (Subtitled) (15)") shouldBe ("Digger", List("SUB"))
  }

  // Electric Palace Bridport bills a showing's note between asterisks after the rest, recording 38363428661: "Screening:
  // NTL - Hay Fever (12A TBC) (160 mins TBC) *MATINEE*", "Wicker (15) 105 mins *MATINEE*", "Pressure (15 TBC) (2026) 100
  // mins *SUBTITLED MATINEE*" — each a card beside the evening showing, its certificate left on behind the note.
  "A showing's note between asterisks" should "come off the title's end, and the certificate it hid with it" in {
    FormatTags.extractFormatTags("Wicker (15) 105 mins *MATINEE*") shouldBe ("Wicker", Nil)
    FormatTags.extractFormatTags("Sense and Sensibility (Cert TBC) (2026) 131 mins *MATINEE*") shouldBe ("Sense and Sensibility (2026)", Nil)
    FormatTags.extractFormatTags("Pressure (15 TBC) (2026) 100 mins *SUBTITLED MATINEE*") shouldBe ("Pressure (2026)", List("SUB"))
    FormatTags.extractFormatTags("Screening: NTL - Hay Fever (12A TBC) (160 mins TBC) *MATINEE*")._1 shouldBe "Screening: NTL - Hay Fever"
    // a running time it brackets is the showing's note too, the certificate before it with it
    FormatTags.extractFormatTags("Screening: NTL - Hay Fever (12A TBC) (160 mins TBC)") shouldBe ("Screening: NTL - Hay Fever", Nil)
    FormatTags.extractFormatTags("Wicker (15) (105 mins)") shouldBe ("Wicker", Nil)
    FormatTags.extractFormatTags("Apollo 13 (140 min.)")._1 shouldBe "Apollo 13"
    // an asterisk inside the title, or one glued to its last word, is the title's own
    FormatTags.extractFormatTags("Smile You Son Of a B****")._1 shouldBe "Smile You Son Of a B****"
    FormatTags.extractFormatTags("Exhibition On Screen: Monet*")._1 shouldBe "Exhibition On Screen: Monet*"
    FormatTags.extractFormatTags("M*A*S*H")._1 shouldBe "M*A*S*H"
    FormatTags.extractFormatTags("*batteries not included")._1 shouldBe "*batteries not included"
  }

  "A box office's certificate" should "key the listing with the film's other listings" in {
    val titles = TitleNormalizer.forCountry(models.Country.UnitedKingdom)
    Seq("Digger (15)", "Digger - Cert 15", "Digger (15) 129 mins").foreach { raw =>
      withClue(s"slot key of '$raw': ")(
        ScrapeListing.slotKey(models.LanternhouseCinema, raw, titles) shouldBe ScrapeListing.slotKey(models.LanternhouseCinema, "Digger", titles))
    }
  }

  it should "leave a bracketed number that is no certificate, and a title that is nothing but one, alone" in {
    FormatTags.extractFormatTags("Sweet Sixteen (16)")._1 shouldBe "Sweet Sixteen (16)"
    FormatTags.extractFormatTags("Apollo 18")._1 shouldBe "Apollo 18"
    FormatTags.extractFormatTags("(15)")._1 shouldBe "(15)"
    // A certificate before a strand stays for the strand's own rule: it is not the title's end.
    FormatTags.extractFormatTags("Calle Malaga (12A) - Film Africa")._1 shouldBe "Calle Malaga (12A) - Film Africa"
  }

  // German re-releases (DE prod 2026-10-13): "Tribute von Panem - Catching Fire (Ov) (Wa)", "… (WA)", "… (WA: 2026)"
  // made five cards of one resolved film. The re-release mark is the showing's, like the version beside it, so both
  // come off in any order and case; "(WA:" is a box office's truncation of it.
  "A re-release mark" should "come off the title with the version tags stacked around it" in {
    FormatTags.extractFormatTags("Tribute von Panem - Catching Fire (Ov) (Wa)") shouldBe ("Tribute von Panem - Catching Fire", List("OV"))
    FormatTags.extractFormatTags("Tribute von Panem - Catching Fire (WA)") shouldBe ("Tribute von Panem - Catching Fire", Nil)
    FormatTags.extractFormatTags("Tribute von Panem - Catching Fire (WA: 2026)") shouldBe ("Tribute von Panem - Catching Fire", Nil)
    FormatTags.extractFormatTags("DON CAMILLO UND PEPPONE (WA:2026)") shouldBe ("DON CAMILLO UND PEPPONE", Nil)
    FormatTags.extractFormatTags("Tribute von Panem - The Ballad of Songbirds and Snakes (WA:") shouldBe
      ("Tribute von Panem - The Ballad of Songbirds and Snakes", Nil)
    FormatTags.extractFormatTags("Tribute von Panem - Mockingjay: Teil 1 (OV) (WA)") shouldBe ("Tribute von Panem - Mockingjay: Teil 1", List("OV"))
    FormatTags.extractFormatTags("Tribute von Panem - Songbirds & Snakes (OV) (WA)") shouldBe ("Tribute von Panem - Songbirds & Snakes", List("OV"))
    FormatTags.extractFormatTags("Tribute von Panem - Catching Fire (WA) (OmU)") shouldBe ("Tribute von Panem - Catching Fire", List("OmU"))
    FormatTags.extractFormatTags("OV: Tribute von Panem - Catching Fire (WA: 2026)") shouldBe ("Tribute von Panem - Catching Fire", List("OV"))
    FormatTags.extractFormatTags("Die Vögel (Wiederaufführung)") shouldBe ("Die Vögel", Nil)
    val titles = TitleNormalizer.forCountry(models.Country.Germany)
    Seq("Tribute von Panem - Catching Fire (Ov) (Wa)", "Tribute von Panem - Catching Fire (WA)", "Tribute von Panem - Catching Fire (WA: 2026)").foreach { raw =>
      withClue(s"slot key of '$raw': ")(ScrapeListing.slotKey(models.LanternhouseCinema, raw, titles) shouldBe
        ScrapeListing.slotKey(models.LanternhouseCinema, "Tribute von Panem - Catching Fire", titles))
    }
  }

  // Traumpalast Leonberg (DE recording 2026-10-13) files a re-release library-style, "Rocky Horror Picture Show, The
  // (WA)": the tidy-up's article fronting saw the mark still at the end, so the peel left "…, The" — a phrase cut off.
  it should "front the article a peel leaves filed at the title's end" in {
    FormatTags.extractFormatTags("Rocky Horror Picture Show, The (WA)") shouldBe ("The Rocky Horror Picture Show", Nil)
    FormatTags.extractFormatTags("Perfekte Urlaub, Der (OV) (WA: 2026)") shouldBe ("Der Perfekte Urlaub", List("OV"))
    FormatTags.extractFormatTags("Piano Tuner, The (15)") shouldBe ("The Piano Tuner", Nil)
    val titles = TitleNormalizer.forCountry(models.Country.Germany)
    ScrapeListing.slotKey(models.LanternhouseCinema, "Rocky Horror Picture Show, The (WA)", titles) shouldBe
      ScrapeListing.slotKey(models.LanternhouseCinema, "The Rocky Horror Picture Show", titles)
  }

  it should "leave a title's own 'Wa' alone" in {
    FormatTags.extractFormatTags("Wa (2019)")._1 shouldBe "Wa (2019)"
    FormatTags.extractFormatTags("Wasabi")._1 shouldBe "Wasabi"
  }

  // Corso Stuttgart bills the original version as a phrase, "Forgotten Island in (Original English Version)" and "… in
  // 2D (Original English Version)": the version came off and left "Forgotten Island in", a card of its own.
  "A version billed with its preposition" should "take the preposition with it, a screen format between them too" in {
    FormatTags.extractFormatTags("Forgotten Island in (Original English Version)") shouldBe ("Forgotten Island", List("OV"))
    FormatTags.extractFormatTags("Forgotten Island in 2D (Original English Version)") shouldBe ("Forgotten Island", List("2D", "OV"))
    FormatTags.extractFormatTags("Avengers: Doomsday in (Original English Version)") shouldBe ("Avengers: Doomsday", List("OV"))
    // A title's own capitalised last word is no preposition.
    FormatTags.extractFormatTags("Let Me In (Original English Version)") shouldBe ("Let Me In", List("OV"))
  }

  "A generic version token" should "be the country's own spelling" in {
    ScreeningTokens.of(models.Country.Poland).normalize(Seq("2D", "SUB")) shouldBe List("2D", "NAP")
    ScreeningTokens.of(models.Country.UnitedKingdom).normalize(Seq("SUB", "DUB")) shouldBe List("SUB", "DUB")
    ScreeningTokens.of(models.Country.Spain).normalize(Seq("SUB")) shouldBe List("VOSE")
  }
}
