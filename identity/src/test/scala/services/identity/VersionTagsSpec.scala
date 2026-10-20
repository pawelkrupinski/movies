package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VersionTagsSpec extends AnyFlatSpec with Matchers {

  "A title closing on a version tag" should "be read without it, whatever the delimiter and language" in {
    VersionTags.versionless("The Lord of the Rings: The Two Towers – Extended") shouldBe Some("The Lord of the Rings: The Two Towers")
    VersionTags.versionless("Herr der Ringe - Rückkehr des Königs (ext.)") shouldBe Some("Herr der Ringe - Rückkehr des Königs")
    VersionTags.versionless("Herr der Ringe - Rückkehr des Königs (ext.) (OV)") shouldBe Some("Herr der Ringe - Rückkehr des Königs")
    VersionTags.versionless("DER HERR DER RINGE - DIE ZWEI TÜRME - EXTENDED VERSION") shouldBe Some("DER HERR DER RINGE - DIE ZWEI TÜRME")
    VersionTags.versionless("Der Herr der Ringe: Die zwei Türme EXTENDED VERSION") shouldBe Some("Der Herr der Ringe: Die zwei Türme")
    VersionTags.versionless("Herr der Ringe 3 - Rückkehr des Königs - ext. Version") shouldBe Some("Herr der Ringe 3 - Rückkehr des Königs")
    VersionTags.versionless("Der Herr der Ringe 2 (Extended Version)") shouldBe Some("Der Herr der Ringe 2")
    VersionTags.versionless("Aliens: Director's Cut") shouldBe Some("Aliens")
    VersionTags.versionless("Das Boot (Kinofassung)") shouldBe Some("Das Boot")
    VersionTags.versionless("Terminator 2: Judgment Day – Remastered 4K") shouldBe Some("Terminator 2: Judgment Day")
    VersionTags.versionless("Your Name (re-release)") shouldBe Some("Your Name")
    VersionTags.versionless("Diabły – wersja reżyserska") shouldBe Some("Diabły")
    VersionTags.versionless("Blade Runner (versión del director)") shouldBe Some("Blade Runner")
    VersionTags.versionless("Amadeus | Special Edition") shouldBe Some("Amadeus")
  }

  it should "be left alone when no whole closing tag is billed, or nothing of a title is left" in {
    VersionTags.versionless("The Lord of the Rings: The Two Towers") shouldBe None
    VersionTags.versionless("Uncut Gems") shouldBe None
    VersionTags.versionless("Extended Version") shouldBe None
    VersionTags.versionless("The Final Cut") shouldBe None
    VersionTags.versionless("Pink Floyd: The Final Cut") shouldBe None
    VersionTags.versionless("Return of the King (Ext.Vers.) Intermission") shouldBe None
    VersionTags.versionless("Collateral (35mm)") shouldBe None
    VersionTags.versionless("Alien: Romulus") shouldBe None
    VersionTags.versionless("Der Herr der Ringe Triple") shouldBe None
  }
}
