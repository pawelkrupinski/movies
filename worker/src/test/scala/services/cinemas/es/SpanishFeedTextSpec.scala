package services.cinemas.es

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class SpanishFeedTextSpec extends AnyFlatSpec with Matchers with OptionValues {

  "certificate" should "spell Taquilla's and Artesiete's certificates the SensaCine way" in {
    SpanishFeedText.certificate("M-12").value shouldBe "+12"
    SpanishFeedText.certificate("NO RECOMENDADA PARA MENORES DE 16 AÑOS").value shouldBe "+16"
    SpanishFeedText.certificate("No Recomendada para Menores de 16 años").value shouldBe "+16"
    SpanishFeedText.certificate("Mayores de 7 años").value shouldBe "+7"
    SpanishFeedText.certificate("+18").value shouldBe "+18"
    SpanishFeedText.certificate("TP").value shouldBe "APTA"
    SpanishFeedText.certificate("APTA PARA TODOS LOS PÚBLICOS").value shouldBe "APTA"
    SpanishFeedText.certificate("Todos los Públicos. Especialmente recomendada para la infancia").value shouldBe "APTA"
    SpanishFeedText.certificate("X").value shouldBe "X"
    Seq("PDC", "EDAD PENDIENTE DE CALIFICACIÓN", "Pendiente de Calificacion", "-")
      .foreach(SpanishFeedText.certificate(_) shouldBe None)
  }

  "field" should "read a string trimmed, a number as its digits, and a blank as none" in {
    val js = Json.obj("a" -> " Clayface ", "n" -> 120, "blank" -> " ")
    SpanishFeedText.field(js, "a").value shouldBe "Clayface"
    SpanishFeedText.field(js, "n").value shouldBe "120"
    SpanishFeedText.field(js, "blank") shouldBe None
    SpanishFeedText.field(js, "missing") shouldBe None
  }

  "fold" should "drop case and accents" in {
    SpanishFeedText.fold("CATALÁN Subtítulos") shouldBe "catalan subtitulos"
  }
}
