package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TitleSeasonSpec extends AnyFlatSpec with Matchers {

  "a listing's season" should "be one a cinema could relay when it screens: of the relay era, its own or the next" in {
    TitleSeason.of(Seq("Royal Ballet 2026/7: Romeo & Juliet"), Some(2026)) shouldBe Some(2026)
    TitleSeason.of(Seq("Met Opera 2027/28: Tosca"), Some(2026)) shouldBe Some(2027)
    // an encore of a past season's capture
    TitleSeason.of(Seq("Royal Opera House 2017/18: Alice im Wunderland"), Some(2026)) shouldBe Some(2017)
    TitleSeason.of(Seq("Met Opera 2029/30: Tosca"), Some(2026)) shouldBe None
  }

  it should "leave a span of years before the relay era to the title" in {
    // DE "Warschau 1944/45", screening in 2026: read as the 1944 season, it denied every film not dated 1944-45
    TitleSeason.of(Seq("Warschau 1944/45"), Some(2026)) shouldBe None
    TitleSeason.of(Seq("Warschau 1944/45", "Royal Ballet 2026/27: Carmen"), Some(2026)) shouldBe Some(2026)
  }

  "a date" should "be no season, its month running on into a day" in {
    // the one-digit season end ("2026/7") also matched an unpadded date
    TitleSeason.of(Seq("Gala 2026-7-15: Carmen")) shouldBe None
    TitleSeason.of(Seq("Royal Ballet 2026/7: Romeo & Juliet")) shouldBe Some(2026)
  }

  "a record's season" should "be any season its titles name" in {
    TitleSeason.of(Seq("Warschau 1944/45")) shouldBe Some(1944)
  }
}
