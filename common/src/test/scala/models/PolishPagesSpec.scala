package models

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Poland's pages after the re-cluster: a major city lists only its own venues,
 *  and every venue outside one is on a town page or a cluster of nearby small
 *  towns — see `data/pl/scripts/build_pages.py` and [[PolishPages]]. */
class PolishPagesSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def page(slug: String): City = City.bySlug(slug).value

  "a major city" should "list only the venues inside it" in {
    Poznan.cinemas should contain (Multikino)
    Poznan.cinemas should not contain KinoHalszka          // Szamotuły
    Warszawa.cinemas should not contain MultikinoPruszkow   // Pruszków
    Trojmiasto.cinemas should not contain MultikinoRumia    // Rumia
    Konin.cinemas should not contain KinoTur                // Turek
  }

  "a venue outside every major city" should "be on its town's page or its cluster's" in {
    City.forCinema(MultikinoPruszkow).value.slug shouldBe "pruszkow"
    City.forCinema(KinoTur).value.slug shouldBe "turek"
    City.forCinema(KinoNadWarta).value.slug shouldBe "turek"   // Koło, 10 km from nothing bigger
  }

  it should "leave no Polish venue off a page, or on two" in {
    val onPages = Country.Poland.cities.flatMap(_.cinemas)
    onPages.distinct should have size onPages.size
    onPages.toSet shouldBe Cinema.polishAndUk.flatMap(_._2).filter(c => City.forCinema(c).exists(_.country == Country.Poland)).toSet
  }

  "a cluster page" should "be named after its biggest town, and speak of it 'i okolicach'" in {
    val turek = page("turek")
    turek.labels.nominative shouldBe "Turek i okolice"
    turek.locativePhrase shouldBe "w Turku i okolicach"
  }

  "a one-town page" should "just name its town, with the 'we' a consonant cluster needs" in {
    page("slubice").labels.nominative shouldBe "Słubice"
    page("slubice").locativePhrase shouldBe "w Słubicach"
    page("wlodawa").locativePhrase shouldBe "we Włodawie"
  }

  "a generated page's description" should "say where its cinemas are rather than decline an adjective" in {
    page("turek").genitivePluralLabel shouldBe ""
    Poznan.genitivePluralLabel shouldBe "poznańskich"
  }

  "Poland's picker" should "group every page by voivodeship, major cities beside their clusters" in {
    val groups = Country.Poland.cityGroups
    groups should have size 16
    groups.flatMap(_.allCities) should contain theSameElementsAs Country.Poland.cities
    val wielkopolskie = groups.find(_.label == "Wielkopolskie").value.cities
    wielkopolskie should contain allOf (Poznan, page("turek"))
  }

  "a page that stopped existing" should "redirect to the page now holding its town" in {
    City.renamedSlugs.get("ketrzyn").value shouldBe "gizycko"
    City.bySlug("ketrzyn") shouldBe None
  }
}
