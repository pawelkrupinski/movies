package services.titlerules

import play.api.libs.json.Json
import services.billing.BillingParse

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

/**
 * Each title rule on the title-rules corpus ([[TitleRulesGoldenSpec.Golden]]'s texts), alone: how many texts it changes,
 * and on how many of those the one billing parse ([[services.billing.BillingParse]]) already reads the same title out
 * of the text, and through which part (the format peel, the certificate, the version tag, the years, a delimited
 * piece, a bill's piece, a slash piece, the one billed title). What docs/investigations/2026-10-22-rule-consolidation.md
 * is built from.
 *
 * `scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRuleInventory <out.tsv>"`
 */
object TitleRuleInventory {

  private def norm(s: String): String = s.trim.replaceAll("\\s+", " ").toLowerCase(java.util.Locale.ROOT)

  /** The titles the billing parse reads out of `text`, by the part that reads each. */
  private def parsed(text: String): Seq[(String, String)] = {
    val parse = BillingParse(text)
    Seq("format" -> parse.formatless, "certificate" -> parse.uncertified, "year" -> parse.yearless) ++
      parse.versionless.map("version" -> _) ++ parse.billedOne.map("billedOne" -> _) ++
      parse.pieces.map("piece" -> _) ++ parse.billPieces.map("bill" -> _) ++ parse.slashPieces.map("slash" -> _)
  }

  def main(args: Array[String]): Unit = {
    val texts = TitleRulesGoldenSpec.golden.map(row => (row \ "text").as[String])
    val rules = TitleRuleSet(TitleRules.all ++ ExtraTitleRules.all).effectiveRules
    val rows  = rules.map { rule =>
      val fired   = texts.filter(text => rule(text) != text)
      val covered = fired.flatMap { text =>
        val out = norm(rule(text))
        parsed(text).find { case (_, title) => norm(title) == out }.map(_._1)
      }
      val parts = covered.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).map { case (p, n) => s"$p:$n" }
      Seq(rule.id, fired.size.toString, covered.size.toString, parts.mkString(","),
        fired.take(3).map(t => Json.stringify(Json.toJson(t))).mkString(" ")).mkString("\t")
    }
    Files.write(Paths.get(args.head), (Seq("id\tcorpusFires\tbillingReads\tparts\texamples") ++ rows).asJava, UTF_8)
    println(s"wrote ${rows.size} rules to ${args.head}")
  }
}
