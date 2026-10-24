package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SearchTitlesSpec extends AnyFlatSpec with Matchers {

  "SearchTitles.of" should "offer the curated spellings: the sources' original titles and the English title" in {
    SearchTitles.of("O dziewczynie skaczącej przez czas", Some("時をかける少女"),
      Seq("時をかける少女", "Toki o kakeru shôjo", "Toki o Kakeru Shōjo", "The Girl Who Leapt Through Time")) shouldBe
      Seq("時をかける少女", "Toki o kakeru shôjo", "Toki o kakeru shojo", "Toki o Kakeru Shōjo", "The Girl Who Leapt Through Time")
  }

  // Japanese, Chinese read in Cantonese, Persian: no transliteration knows the words, the sources do.
  it should "not add a machine romanisation where a curated one exists" in {
    SearchTitles.of("Parasite", Some("기생충"), Seq("기생충", "Gisaengchung", "Parasite")) shouldBe Seq("기생충", "Gisaengchung")
    SearchTitles.of("Chungking Express", Some("重慶森林"), Seq("重慶森林", "Chung Hing sam lam")) shouldBe Seq("重慶森林", "Chung Hing sam lam")
    SearchTitles.of("Kokuho", Some("国宝"), Seq("国宝", "Kokuhō")) shouldBe Seq("国宝", "Kokuhō")
  }

  it should "add an accent-free form of a curated spelling, so a search typed without accents finds it" in {
    SearchTitles.of("Smak wiśni", Some("طعم گيلاس"), Seq("طعم گيلاس", "Ta'm-e gīlās", "Taste of Cherry")) shouldBe
      Seq("طعم گيلاس", "Ta'm-e gīlās", "Ta'm-e gilas", "Taste of Cherry")
  }

  it should "romanise a non-Latin original by machine only when no source spells it in Latin letters" in {
    SearchTitles.of("Moskwa nie wierzy łzom", Some("Москва слезам не верит"), Nil) shouldBe
      Seq("Москва слезам не верит", "Moskva slezam ne verit")
    SearchTitles.of("Pies", Some("Σκυλί"), Seq("Σκυλί")) shouldBe Seq("Σκυλί", "Skyli")
  }

  // An abjad writes no short vowels, so its machine romanisation ("khrs nyst" for خرس نیست,
  // "Khers nist") is no spelling anybody types.
  it should "not romanise an Arabic or Hebrew script title by machine" in {
    SearchTitles.of("Dreamland", Some("رویاشهر"), Seq("رویاشهر")) shouldBe Seq("رویاشهر")
    SearchTitles.of("Pies", Some("כלב"), Nil) shouldBe Seq("כלב")
  }

  // A document projected before the curated spellings existed still offers its original title.
  it should "fall back to the original title when the read model has no curated spellings yet" in {
    SearchTitles.of("Zimna wojna", Some("Cold War"), Nil) shouldBe Seq("Cold War")
  }

  it should "offer nothing that is the title again, or blank" in {
    SearchTitles.of("Dune", None, Nil) shouldBe Nil
    SearchTitles.of("Dune", Some("Dune"), Seq("  DUNE ")) shouldBe Nil
    SearchTitles.of("Dzień", Some("Dzień"), Seq("   ")) shouldBe Nil
  }
}
