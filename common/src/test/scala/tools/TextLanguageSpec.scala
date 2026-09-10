package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `TextLanguage.detect` — the stopword-ratio guess `MovieRecord.bestSynopsis`
 * uses to keep a wrong-language cinema blurb from beating a correctly-
 * localized one on length alone (see `MovieRecordSynopsisSpec`'s Cinema
 * City / Marsupilami cases for the production scenario).
 */
class TextLanguageSpec extends AnyFlatSpec with Matchers {

  "detect" should "recognise a Polish paragraph" in {
    TextLanguage.detect(
      "Aby uratować pracę, David zgadza się na szalony plan, który wciąga go i jego rodzinę " +
        "w podróż przez dżunglę, gdzie musi zmierzyć się z niebezpieczeństwem."
    ) shouldBe Some("pl")
  }

  it should "recognise an English paragraph" in {
    TextLanguage.detect(
      "To save his job, David agrees to a wild plan that pulls him and his family into a journey " +
        "through the jungle, where they must face danger after danger before the end."
    ) shouldBe Some("en")
  }

  it should "recognise a German paragraph" in {
    TextLanguage.detect(
      "Um seinen Job zu retten, willigt David in einen verrückten Plan ein, der ihn und seine Familie " +
        "auf eine Reise durch den Dschungel mitnimmt, wo sie sich der Gefahr stellen müssen."
    ) shouldBe Some("de")
  }

  it should "recognise a Spanish paragraph" in {
    TextLanguage.detect(
      "Para salvar su trabajo, David acepta un plan alocado que lo lleva a él y a su familia en un " +
        "viaje por la selva, donde deben enfrentarse al peligro para llegar a su destino."
    ) shouldBe Some("es")
  }

  it should "return None for text too short to classify confidently" in {
    TextLanguage.detect("Opis z sieci Cinema City.") shouldBe None
  }

  it should "return None for an empty string" in {
    TextLanguage.detect("") shouldBe None
  }

  it should "not be thrown off by diacritics (deburr before matching)" in {
    // "się"/"żeby" etc. only match the ASCII "sie" stopword after deburring.
    TextLanguage.detect(
      "On się boi, że nie zdąży, więc biegnie ile sił, aby dotrzeć tam, gdzie go już czekają."
    ) shouldBe Some("pl")
  }
}
