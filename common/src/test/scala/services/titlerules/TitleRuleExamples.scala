package services.titlerules

import models.Country
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

/**
 * Each title-rule row's EXAMPLES (`title-rules.tsv`'s `examples` column): a billing and what the row's tier makes of it
 * under the rules of the first country holding the row — the lookup query of a `banner` or `query` row
 * ([[TitleRuleSet.query]]), the merge-key fold of a `key` row ([[TitleRuleSet.canonical]]), the proper name of a `name`
 * row ([[TitleRuleSet.properNamed]]), the venue's cleanup of a `venue` row ([[TitleRuleSet.perCinema]]).
 * [[TitleRuleExamplesSpec]] holds every row to its examples and [[TitleRuleMutationSpec]] removes each row in turn.
 *
 * `scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRuleExamples <strings.txt> <out.tsv>"` mines them: the
 * golden corpus's billings, then the strings of `strings.txt` (the fixtures' and specs' strings), on which the row fires
 * in its tier's fold, those its removal changes first.
 */
object TitleRuleExamples {

  /** The first country whose rule set holds `rule`. */
  def countryOf(rule: TitleRule): Country = Country.all.find(rule.appliesTo).getOrElse(Country.all.head)

  /** What `rules` make of `input` in `rule`'s tier. */
  def reading(rule: TitleRule, rules: TitleRuleSet, input: String): String = rule.scope match {
    case RuleScope.GlobalStructural                      => rules.query(input)
    case RuleScope.Canonical if rule.writesProperName    => rules.properNamed(input)
    case RuleScope.Canonical                             => rules.canonical(input)
    case _                                               => rules.perCinema(rule.cinemaId.getOrElse(""), input)
  }

  /** The table's rule set for `country`, less the rows `drop` names. */
  def ruleSet(country: Country, drop: Set[String] = Set.empty, table: Seq[TitleRule] = TitleRuleTable.all): TitleRuleSet =
    TitleRuleSet(table.filter(r => r.appliesTo(country) && !drop(r.id)))

  /** Does `rule` fire in its tier's fold of `input`? */
  private def fires(rule: TitleRule, rules: TitleRuleSet, input: String): Boolean = rule.scope match {
    case RuleScope.GlobalStructural => rules.firedSearch(input).contains(rule.id)
    case RuleScope.Canonical        => rules.firedCanonical(input).contains(rule.id)
    case _                          => rules.firedPerCinema(rule.cinemaId.getOrElse(""), input).contains(rule.id)
  }

  private def clean(s: String): Boolean =
    s.length <= 140 && s.trim == s && s.exists(_.isLetter) && !s.exists(c => c == '\t' || c == '{' || c == '}' || c == '\\') &&
      !s.contains("\":") && !s.contains("&#") && !s.contains("&quot;")

  def main(args: Array[String]): Unit = if (args.headOption.contains("--inputs")) read(args(1), args(2)) else mine(args(0), args(1))

  /** `id<TAB>input` lines read as each row's tier reads them: the examples written by hand for the rows whose mined ones
   *  are no real billing. Prints `id country bearing [[input, reading]]`. */
  private def read(inputs: String, out: String): Unit = {
    val table = TitleRuleTable.all
    val lines = Files.readAllLines(Paths.get(inputs), UTF_8).asScala.filter(_.contains("\t")).groupBy(_.split("\t")(0)).toSeq.map {
      case (id, ls) =>
        val rule    = table.find(_.id == id).getOrElse(sys.error(s"no row $id"))
        val country = countryOf(rule)
        val (full, without) = (ruleSet(country), ruleSet(country, Set(id)))
        val ins     = ls.map(_.split("\t", 2)(1)).toSeq
        val bearing = ins.exists(i => reading(rule, full, i) != reading(rule, without, i))
        Seq(id, country.code, "hand", bearing.toString, Json.stringify(Json.toJson(ins.map(i => Seq(i, reading(rule, full, i)))))).mkString("\t")
    }
    Files.write(Paths.get(out), lines.asJava, UTF_8)
    println(s"wrote ${lines.size}")
  }

  private def mine(stringsFile: String, out: String): Unit = {
    val golden  = TitleRulesGoldenSpec.golden.map(row => (row \ "text").as[String]).filter(clean)
    val strings = Files.readAllLines(Paths.get(stringsFile), UTF_8).asScala.iterator.filter(clean).toSeq.distinct
    val table   = TitleRuleTable.all
    val rows    = TitleRuleSet(table).effectiveRules
    val lines   = rows.map { expanded =>
      val rule    = table.find(_.id == expanded.id).get
      val country = countryOf(rule)
      val full    = ruleSet(country)
      val without = ruleSet(country, Set(rule.id))
      val loose   = expanded.copy(countries = None)
      def candidates(pool: Seq[String]) = pool.iterator.filter(s => loose(s) != s && fires(expanded, full, s))
      def mine(pool: Seq[String]) = {
        val firing  = candidates(pool).take(400).toSeq.sortBy(s => (s.length, s))
        val bearing = firing.filter(s => reading(rule, full, s) != reading(rule, without, s))
        (bearing.take(2), firing.take(1))
      }
      val (gb, gf) = mine(golden)
      val (sb, sf) = if (gb.nonEmpty) (Nil, Nil) else mine(strings)
      val (chosen, source, bearing) =
        if (gb.nonEmpty) (gb, "golden", true) else if (sb.nonEmpty) (sb, "strings", true)
        else if (gf.nonEmpty) (gf, "golden", false) else if (sf.nonEmpty) (sf, "strings", false) else (Nil, "none", false)
      val examples = Json.stringify(Json.toJson(chosen.map(s => Seq(s, reading(rule, full, s)))))
      Seq(rule.id, country.code, source, bearing.toString, examples).mkString("\t")
    }
    Files.write(Paths.get(out), lines.asJava, UTF_8)
    println(s"wrote ${lines.size}: " + lines.groupBy(l => l.split("\t").slice(2, 4).mkString("/")).view.mapValues(_.size).toMap)
  }
}
