package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SearchTitlesSpec extends AnyFlatSpec with Matchers {

  "SearchTitles.of" should "offer an original title in Latin letters as it is" in {
    SearchTitles.of("Zimna wojna", Some("Cold War")) shouldBe Seq("Cold War")
  }

  it should "add the original's romanisation when it is written in another script" in {
    SearchTitles.of("Moskwa nie wierzy łzom", Some("Москва слезам не верит")) shouldBe
      Seq("Москва слезам не верит", "Moskva slezam ne verit")
    SearchTitles.of("Pies", Some("Σκυλί")) shouldBe Seq("Σκυλί", "Skyli")
    // ICU reads kanji as Mandarin, so a Japanese original gets SOME Latin spelling, not its reading:
    // still one a search can reach, which is all this promises.
    val japanese = SearchTitles.of("Spirited Away", Some("千と千尋の神隠し"))
    japanese should have size 2
    japanese(1).forall(_ < 0x80) shouldBe true
  }

  it should "romanise to ASCII, so a search typed without accents finds it" in {
    SearchTitles.of("Dzień", Some("Dzień")) shouldBe Nil
    SearchTitles.of("Sny", Some("Ёжик в тумане")).last shouldBe "Ezik v tumane"
  }

  it should "offer nothing when there is no original title or it is the title again" in {
    SearchTitles.of("Dune", None) shouldBe Nil
    SearchTitles.of("Dune", Some("Dune")) shouldBe Nil
    SearchTitles.of("Dune", Some("  DUNE ")) shouldBe Nil
    SearchTitles.of("Dune", Some("   ")) shouldBe Nil
  }
}
