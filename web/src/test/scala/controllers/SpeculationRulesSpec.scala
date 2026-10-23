package controllers

import models.{City, Country, Poznan}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.JsValue

class SpeculationRulesSpec extends AnyFlatSpec with Matchers {

  private def rule(city: City): JsValue = (SpeculationRules.forCity(city) \ "prefetch" \ 0).get

  "SpeculationRules" should "prefetch only the page's own city's film pages, on hover or pointerdown" in {
    val poznan = rule(Poznan)
    (poznan \ "where" \ "and" \ 0 \ "href_matches").as[String] shouldBe "/poznan/movie/*"
    (poznan \ "eagerness").as[String] shouldBe "moderate"
  }

  it should "keep a mounted country's prefix in the pattern" in {
    val ukCity = Country.UnitedKingdom.cities.head
    (rule(ukCity) \ "where" \ "and" \ 0 \ "href_matches").as[String] shouldBe s"/uk/${ukCity.slug}/movie/*"
  }

  it should "leave the share-card image route under the same prefix out" in {
    (rule(Poznan) \ "where" \ "and" \ 1 \ "not" \ "href_matches").as[String] shouldBe "/poznan/movie/og-image"
  }

  it should "prefetch, never prerender" in {
    (SpeculationRules.forCity(Poznan) \ "prerender").toOption shouldBe None
  }
}
