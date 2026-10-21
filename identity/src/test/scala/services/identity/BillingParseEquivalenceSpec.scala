package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, JsString, Json}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/**
 * Proposal #2's equivalence phase (docs/proposals/2026-10-21-matching-fundamentals.md §4.2): every distinct billing of
 * the identity recordings (Forgejo 38713274617, 528, 573) with what today's billing readers make of it. Each reader moved
 * onto the one structured billing parse must reproduce every row, so the move changes no take.
 *
 * Regenerate (only when a reader's behaviour is MEANT to change, never to make a move pass):
 * `scripts/sbt-warm "identity/Test/runMain services.identity.BillingParseEquivalenceSpec [<texts>]"` rewrites
 * [[BillingParseEquivalenceSpec.Golden]] from `<texts>` (a file of JSON strings, one a line) or else from its own rows.
 */
class BillingParseEquivalenceSpec extends AnyFlatSpec with Matchers {
  import BillingParseEquivalenceSpec.*

  "every billing of the recordings" should "read as the golden file records it" in {
    val rows     = golden
    rows.size should be > 10000
    val mismatch = rows.iterator.filter(row => row != reading((row \ "text").as[String])).take(20).toSeq
    withClue(mismatch.map(row => s"\n  golden ${Json.stringify(row)}\n  now    ${Json.stringify(reading((row \ "text").as[String]))}").mkString) {
      mismatch shouldBe empty
    }
  }
}

object BillingParseEquivalenceSpec {
  val Golden = Paths.get("test/resources/fixtures/billing-parse/golden.jsonl")

  /** What the billing readers make of `text`: a field is left out where it says nothing (equal to the text, empty or
   *  none), so the golden file stays small and a row reads as what the parse found. */
  def reading(text: String): JsObject = {
    val parse  = services.billing.BillingParse(text)
    val peeled = parse.peeled
    val fields = Seq(
      "formatless"  -> Option.when(peeled.title != text)(JsString(peeled.title)),
      "tokens"      -> Option.when(peeled.tokens.nonEmpty)(Json.toJson(peeled.tokens)),
      "certificate" -> peeled.certificate.map(JsString(_)),
      "runtime"     -> peeled.runtimeMinutes.map(Json.toJson(_)),
      "uncertified" -> Some(parse.uncertified).filter(_ != text).map(JsString(_)),
      "versionless" -> parse.versionless.map(JsString(_))
    )
    JsObject(("text" -> JsString(text)) +: fields.collect { case (k, Some(v)) => k -> v })
  }

  private def golden: Seq[JsObject] = Files.readAllLines(Golden, UTF_8).asScala.toSeq.filter(_.nonEmpty).map(Json.parse(_).as[JsObject])

  def main(args: Array[String]): Unit = {
    val texts = args.headOption match {
      case Some(file) => Files.readAllLines(Paths.get(file), UTF_8).asScala.toSeq.filter(_.nonEmpty).map(Json.parse(_).as[String])
      case None       => golden.map(row => (row \ "text").as[String])
    }
    Files.createDirectories(Golden.getParent)
    Files.write(Golden, texts.distinct.sorted.map(t => Json.stringify(reading(t))).asJava, UTF_8)
    println(s"wrote ${texts.distinct.size} billings to $Golden")
  }
}
