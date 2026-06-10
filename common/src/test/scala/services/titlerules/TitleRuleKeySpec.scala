package services.titlerules

import models.{CinemaCityKinepolis, Helios, HeliosMagnolia, KinoApollo, Multikino, MultikinoMlociny}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TitleRuleKeySpec extends AnyFlatSpec with Matchers {

  "TitleRuleKey" should "collapse all venues of a chain onto one key" in {
    TitleRuleKey.of(CinemaCityKinepolis) shouldBe "cinema-city"
    TitleRuleKey.of(Helios)              shouldBe "helios"
    TitleRuleKey.of(HeliosMagnolia)      shouldBe "helios"
    TitleRuleKey.of(Multikino)           shouldBe "multikino"
    TitleRuleKey.of(MultikinoMlociny)    shouldBe "multikino"
  }

  it should "slug a standalone cinema's display name, deburring Polish letters" in {
    TitleRuleKey.of(KinoApollo)               shouldBe "kino-apollo"
    TitleRuleKey.of("Kino Głębocka 66")       shouldBe "kino-glebocka-66"
    TitleRuleKey.of("Kino Pałacowe")          shouldBe "kino-palacowe"
  }
}
