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
    TitleRuleKey.of("Kino Pałacowe")          shouldBe "kino-palacowe"
  }

  it should "collapse both BoK venues onto one shared key" in {
    TitleRuleKey.of("Kino na Boku")     shouldBe "bok"
    TitleRuleKey.of("Kino Głębocka 66") shouldBe "bok"
  }

  it should "key ß-carrying German venues on the frozen fold, not the URL one" in {
    // Rule keys are a FROZEN key space — rules are persisted against them. The
    // shared slugger's URL policy maps ß→ss (so film permalinks read
    // "grosse-freiheit" not "groe-freiheit"), which would silently re-key the
    // 15 German venues whose display name carries one and orphan their rules.
    // These assertions pin the pre-extraction bytes.
    TitleRuleKey.of("Kino Weißhaus")     shouldBe "kino-wei-haus"
    TitleRuleKey.of("Filmpalast Meißen") shouldBe "filmpalast-mei-en"
    tools.Slugify("Kino Weißhaus")       shouldBe "kino-weisshaus"   // the URL policy, for contrast
  }
}
