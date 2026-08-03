package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `Country.soleFrom` answers None for two situations that must not be treated
 * alike: nothing configured (a dev box or a spec — defaulting to Poland is
 * right) and SEVERAL configured (a multi-country worker — defaulting to Poland
 * is the 2026 incident, where CinemaxX Würzburg's "Minions & Monster" was stored
 * and served as "Minions i Monster"). `ambiguousFrom` separates them, so
 * `TitleNormalizer.deployment` can refuse the second while still serving the
 * first.
 */
class AmbiguousCountryConfigSpec extends AnyFlatSpec with Matchers {

  "ambiguousFrom" should "report nothing when no country is configured at all" in {
    // The dev/spec shape: Poland is a fine default, not an ambiguity.
    Country.ambiguousFrom(None, None) shouldBe Nil
  }

  it should "report nothing for each single-country deploy we actually run" in {
    Seq("pl", "de", "uk").foreach { code =>
      withClue(s"KINOWO_COUNTRIES=$code: ")(Country.ambiguousFrom(None, Some(code)) shouldBe Nil)
    }
  }

  it should "report nothing when KINOWO_COUNTRY names one, whatever the list says" in {
    // The web tier's shape: the singular wins, so there is no ambiguity to report.
    Country.ambiguousFrom(Some("de"), Some("pl,de,uk")) shouldBe Nil
  }

  it should "name every country when the list holds several and nothing disambiguates" in {
    // The exact worker shape that silently fell back to Poland.
    Country.ambiguousFrom(None, Some("pl,de,uk")) should contain theSameElementsAs
      Seq(Country.Poland, Country.Germany, Country.UnitedKingdom)
  }

  it should "report even a two-country pairing" in {
    Country.ambiguousFrom(None, Some("pl,de")) should have size 2
  }

  it should "ignore blanks and duplicates rather than calling them several countries" in {
    Country.ambiguousFrom(None, Some("pl, ,pl")) shouldBe Nil
  }
}

