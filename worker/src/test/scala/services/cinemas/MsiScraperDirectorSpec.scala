package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit coverage for the director line mined out of an MSI `RepertoireEvents`
 *  Description. The portals emit several label shapes — these are the real ones
 *  seen across Zamek/Cinema1/Kijów fixtures. Lives in `services.cinemas` so it
 *  can reach the package-private `MsiScraper`. */
class MsiScraperDirectorSpec extends AnyFlatSpec with Matchers {

  "MsiScraper.parseDescriptionDirector" should "read the labelled 'REŻYSERIA:' form" in {
    MsiScraper.parseDescriptionDirector(
      "REŻYSERIA: Luigi Antonini<br><br> WYSTĘPUJĄ: Andrea Bocelli, José Carreras"
    ) shouldBe Seq("Luigi Antonini")
  }

  it should "read the colon-less 'REŻYSERIA …' form and drop the trailing period" in {
    MsiScraper.parseDescriptionDirector(
      "(Viridiana, Hiszpania/Meksyk 1961, 91’) <br><br><br>REŻYSERIA Luis Buñuel. <br>OBSADA: Silvia Pinal"
    ) shouldBe Seq("Luis Buñuel")
  }

  it should "read the 'Reżysera' genitive-typo form mid-Description" in {
    MsiScraper.parseDescriptionDirector(
      "dokumentalny, Włochy 2021 (94 min) <br><br><br>Reżysera  Marco Pianigiani<br><br>Salvador Dali"
    ) shouldBe Seq("Marco Pianigiani")
  }

  it should "split a comma-separated co-director list" in {
    MsiScraper.parseDescriptionDirector(
      "(Caravaggio, Wielka Brytania 2025, 100’) – FILM O SZTUCE<br>REŻYSERIA: David Bickerstaff, Phil Grabsky<br>"
    ) shouldBe Seq("David Bickerstaff", "Phil Grabsky")
  }

  it should "return empty for a Description with no director line (kids-film synopsis)" in {
    MsiScraper.parseDescriptionDirector(
      "Witajcie moi drodzy! Nazywam się Alicja i muszę się Wam poskarżyć...<br>"
    ) shouldBe empty
  }

  it should "not match a mid-sentence 'w reżyserii X' prose mention" in {
    // The genitive "reżyserii" ends past the `(?:ia|a)` the label form requires,
    // and the mention is not at a segment start, so no false director.
    MsiScraper.parseDescriptionDirector(
      "Film nakręcony w reżyserii Stevena Spielberga zachwyca.<br>"
    ) shouldBe empty
  }

  // The same Description blob carries the release year (and, for foreign films,
  // the original title) inside a `(OriginalTitle, Country YEAR, runtime')`
  // production-line parenthetical — there is no dedicated year field. Extracting
  // it lets these films resolve against same-title TMDB collisions, which the
  // director alone can't disambiguate.
  "MsiScraper.parseDescriptionProduction" should "read year + original title for a foreign film" in {
    MsiScraper.parseDescriptionProduction(
      "(Casablanca, USA 1942, 102’)<br>REŻYSERIA: Michael Curtiz"
    ) shouldBe ((Some(1942), Some("Casablanca")))
  }

  it should "read the year but no original title for a Polish film (paren opens on the country)" in {
    MsiScraper.parseDescriptionProduction(
      "(Polska 1976, 153’) <br><br>REŻYSERIA Andrzej Wajda."
    ) shouldBe ((Some(1976), None))
  }

  it should "read the year but no original title for a slash-separated multi-country co-production" in {
    MsiScraper.parseDescriptionProduction(
      "(Kanada/USA/Wielka Brytania 2001, 117’)<br>REŻYSERIA: Richard Kelly"
    ) shouldBe ((Some(2001), None))
  }

  it should "handle an original title that contains spaces" in {
    MsiScraper.parseDescriptionProduction(
      "(What Ever Happened to Baby Jane?, USA 1962, 134’)"
    ) shouldBe ((Some(1962), Some("What Ever Happened to Baby Jane?")))
  }

  it should "not leak a leading country as an original title for a comma-separated two-country film" in {
    // Regression: `(USA, Japonia 1989, …)` — Jarmusch's "Mystery Train" (the
    // original title sits OUTSIDE the paren). "USA" is the first of two countries,
    // not a title, so it must be detected and dropped.
    MsiScraper.parseDescriptionProduction(
      "Mystery Train (USA, Japonia 1989, 110 min.) komediodramat | reż. Jim Jarmusch"
    ) shouldBe ((Some(1989), None))
  }

  it should "keep a real title while stripping trailing countries that follow it" in {
    // A title followed by two countries inside the paren: the title survives, the
    // countries are stripped.
    MsiScraper.parseDescriptionProduction(
      "(Mystery Train, USA, Japonia 1989, 110’)"
    ) shouldBe ((Some(1989), Some("Mystery Train")))
  }

  it should "not mistake a bare biographical-dates paren for a production line" in {
    // Van Gogh's lifespan, not a film year — no country word precedes it.
    MsiScraper.parseDescriptionProduction(
      "Vincent van Gogh (1869-1939) malował pola zbóż.<br>"
    ) shouldBe ((None, None))
  }

  it should "return None for a Description with no production line" in {
    MsiScraper.parseDescriptionProduction(
      "Witajcie moi drodzy! Nazywam się Alicja...<br>"
    ) shouldBe ((None, None))
  }
}
