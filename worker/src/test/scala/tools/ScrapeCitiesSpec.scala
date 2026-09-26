package tools

import models.City
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.{ProcessConfiguration, ScrapeCitySlugs}

class ScrapeCitiesSpec extends AnyFlatSpec with Matchers {

  private def scrapeCities(vars: (String, String)*): Option[ScrapeCitySlugs] =
    new ProcessConfiguration(Env.of(vars*)).scrapeCitySlugs

  "ScrapeCities.allCities (the production default)" should "be every modelled city, not a gated subset" in {
    // Guards the city limit staying removed: every City.all slug — including the
    // ones the old KINOWO_SCRAPE_CITIES gate excluded (łódź, katowice, rzeszów…)
    // — must be in the default scrape set.
    ScrapeCities.allCities shouldBe City.all.map(_.slug).toSet
    ScrapeCities.allCities should contain allOf ("lodz", "katowice", "rzeszow", "torun")
    ScrapeCities.allCities.size shouldBe City.all.size
  }

  "The KINOWO_SCRAPE_CITIES override" should "be absent when unset, blank or naming nothing, so the default stands" in {
    scrapeCities() shouldBe None
    scrapeCities("KINOWO_SCRAPE_CITIES" -> "") shouldBe None
    scrapeCities("KINOWO_SCRAPE_CITIES" -> "  , ") shouldBe None
  }

  it should "parse a comma-separated list, trimmed and lowercased" in {
    scrapeCities("KINOWO_SCRAPE_CITIES" -> " Poznan , wroclaw ,") shouldBe Some(ScrapeCitySlugs(Set("poznan", "wroclaw")))
  }

  it should "name exactly the listed cities — it REPLACES the default rather than merging" in {
    scrapeCities("KINOWO_SCRAPE_CITIES" -> "poznan,wroclaw,warszawa") shouldBe Some(ScrapeCitySlugs(Set("poznan", "wroclaw", "warszawa")))
    scrapeCities("KINOWO_SCRAPE_CITIES" -> "wroclaw") shouldBe Some(ScrapeCitySlugs(Set("wroclaw")))
  }
}
