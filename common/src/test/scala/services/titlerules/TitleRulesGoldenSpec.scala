package services.titlerules

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, JsString, Json}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*

/**
 * Every title rule's effect pinned on a corpus: what each country's rule set makes of every distinct billing of the
 * identity recordings and every prod title snapshot — the lookup query, the merge key's fold, the display spelling, the
 * proper-name spelling, the programme banner, the banner boundary and each venue's cleanup. A change to the rules'
 * DATA (moving them into one table, merging rows that say the same thing) must reproduce every row; only a change
 * MEANT to normalise differently regenerates it.
 *
 * Regenerate: `scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRulesGoldenSpec"` rewrites
 * [[TitleRulesGoldenSpec.Golden]] over its own texts plus the billing and prod-title fixtures.
 */
class TitleRulesGoldenSpec extends AnyFlatSpec with Matchers {
  import TitleRulesGoldenSpec.*

  "every title of the corpus" should "fold under each country's rules as the golden file records" in {
    val rows = golden
    rows.size should be > 15000
    val sets     = ruleSets
    val mismatch = rows.iterator.filter(row => row != reading((row \ "text").as[String], sets)).take(20).toSeq
    withClue(mismatch.map(row => s"\n  golden ${Json.stringify(row)}\n  now    ${Json.stringify(reading((row \ "text").as[String], sets))}").mkString) {
      mismatch shouldBe empty
    }
  }
}

object TitleRulesGoldenSpec {
  val Golden: Path = Paths.get("test/resources/fixtures/title-rules/golden.jsonl")
  private val Billings: Path   = Paths.get("test/resources/fixtures/billing-parse/golden.jsonl")
  private val ProdTitles: Path = Paths.get("common/src/test/resources/fixtures/prod-movies/titles.txt")

  /** Each country's rule set, as a process serving it builds it, and the venues any rule is scoped to. */
  final case class Sets(byCountry: Seq[(Country, TitleRuleSet)], venues: Seq[String])

  def ruleSets: Sets = Sets(
    Country.all.map(country => country -> TitleRuleSet.forCountry(country)),
    (TitleRules.all ++ ExtraTitleRules.all).flatMap(_.cinemaId).distinct.sorted)

  /** What one rule set makes of `text`: a field is left out where it says nothing (the text itself, or none). */
  private def folds(text: String, rules: TitleRuleSet, venues: Seq[String]): JsObject = {
    def changed(key: String, value: String) = Option.when(value != text)(key -> JsString(value))
    val venue = venues.flatMap(v => Option.when(rules.perCinema(v, text) != text)(v -> JsString(rules.perCinema(v, text))))
    JsObject(Seq(
      changed("query", rules.structural(text)),
      changed("key", rules.canonical(text)),
      changed("display", rules.spellingUnified(text)),
      changed("properName", rules.properNamed(text)),
      rules.programmePrefix(text).map("programme" -> JsString(_)),
      rules.leadingBannerBoundary(text).map("bannerEnd" -> Json.toJson(_)),
      Option.when(venue.nonEmpty)("venue" -> JsObject(venue))
    ).flatten)
  }

  /** `text` with each country's folds, the countries that fold it alike under one comma-joined key. */
  def reading(text: String, sets: Sets): JsObject = {
    val byCountry = sets.byCountry.map { case (country, rules) => country.code -> folds(text, rules, sets.venues) }
    val grouped   = byCountry.groupBy(_._2).toSeq.filter(_._1.value.nonEmpty)
      .map { case (fold, countries) => countries.map(_._1).mkString(",") -> fold }
      .sortBy { case (codes, _) => byCountry.indexWhere(_._1 == codes.takeWhile(_ != ',')) }
    JsObject(("text" -> JsString(text)) +: grouped)
  }

  def golden: Seq[JsObject] =
    Files.readAllLines(Golden, UTF_8).asScala.toSeq.filter(_.nonEmpty).map(Json.parse(_).as[JsObject])

  private def lines(path: Path): Seq[String] =
    if (Files.exists(path)) Files.readAllLines(path, UTF_8).asScala.toSeq.filter(_.nonEmpty) else Seq.empty

  /** Every rule as it compiles, one JSON object a line, in fold order within its tier: the rule-level record a move of
   *  the rules' data is checked against before the corpus is. */
  def ruleDump: Seq[String] =
    (TitleRules.all ++ ExtraTitleRules.all).zip(TitleRuleSet(TitleRules.all ++ ExtraTitleRules.all).effectiveRules)
      .sortBy((_, r) => (r.scope.name, r.cinemaId.getOrElse(""), r.last, r.order, r.id)).map { (raw, r) =>
        Json.stringify(Json.obj("id" -> r.id, "scope" -> r.scope.name, "cinema" -> r.cinemaId, "order" -> r.order,
          "last" -> r.last, "applyAll" -> r.applyAll, "enabled" -> r.enabled, "tag" -> r.tag,
          "countries" -> r.countries.map(_.toSeq.map(_.code).sorted), "properName" -> r.writesProperName,
          "pattern" -> r.pattern, "raw" -> raw.pattern, "replacement" -> r.replacement, "note" -> r.note))
      }

  def main(args: Array[String]): Unit = args.toSeq match {
    case Seq("--rules", out) =>
      Files.write(Paths.get(out), ruleDump.asJava, UTF_8)
      println(s"wrote ${ruleDump.size} rules to $out")
    case _ => writeGolden()
  }

  private def writeGolden(): Unit = {
    val texts = (lines(Golden).map(Json.parse(_) \ "text").map(_.as[String]) ++
      lines(Billings).map(Json.parse(_) \ "text").map(_.as[String]) ++ lines(ProdTitles)).distinct.sorted
    val sets  = ruleSets
    Files.createDirectories(Golden.getParent)
    Files.write(Golden, texts.map(t => Json.stringify(reading(t, sets))).asJava, UTF_8)
    println(s"wrote ${texts.size} titles to $Golden")
  }
}
