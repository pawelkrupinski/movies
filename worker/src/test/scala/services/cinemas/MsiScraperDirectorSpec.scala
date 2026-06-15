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
}
