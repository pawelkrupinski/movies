package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.TitleOverrides

class TitleOverridesSpec extends AnyFlatSpec with Matchers {

  // Regression: TMDB has no Polish translation for tt36437006 (Girl Climber),
  // so a Polish search for "Wspinaczka" returns unrelated films. The override
  // pins the right IMDb id so the enrichment pipeline can /find it directly.
  "TitleOverrides" should "resolve Wspinaczka (2025) to tt36437006" in {
    TitleOverrides.lookup("Wspinaczka", Some(2025)) shouldBe Some("tt36437006")
  }

  it should "be case- and diacritic-insensitive on the title key" in {
    TitleOverrides.lookup("wspinaczka", Some(2025)) shouldBe Some("tt36437006")
    TitleOverrides.lookup("WSPINACZKA", Some(2025)) shouldBe Some("tt36437006")
  }

  it should "return None when the year doesn't match an override row" in {
    TitleOverrides.lookup("Wspinaczka", Some(2017)) shouldBe None
    TitleOverrides.lookup("Wspinaczka", None)       shouldBe None
  }

  it should "return None for titles not in the override table" in {
    TitleOverrides.lookup("Mortal Kombat II", Some(2026)) shouldBe None
  }
}
