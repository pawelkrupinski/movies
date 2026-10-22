package services.titlerules

import models.Country
import play.api.libs.json.Json

import scala.io.Source

/** Every title rule as one row of data (`title-rules.tsv`), read by the few mechanisms a row's KIND names:
 *
 *  - `banner` — a programme banner off the front of the lookup query, which the display also finds to case apart
 *    (`GlobalStructural`, tagged `programmePrefix`): "Klub Konesera: Ojczyzna" searches as "Ojczyzna";
 *  - `query` — a strip or rewrite of the lookup query only (`GlobalStructural`): the screening keeps its own row;
 *  - `key` — a fold of the merge key and the display spelling (`Canonical`): spellings of one film collapse into a row;
 *  - `name` — a `key` rewrite to a broadcaster's PROPER NAME ([[TitleRule.writesProperName]]);
 *  - `venue` — one venue's cleanup at ingest (`PerCinema`, the row's `cinema` the [[TitleRuleKey]]).
 *
 *  A row's rules fold in the order of the table: a tier (and each venue) folds its rows top to bottom. The table's
 *  `effect` and `season` columns describe a row (what it takes off, when its banner is billed) and change nothing: a
 *  seasonal row is in force all year, ready for the season's billings. */
object TitleRuleTable {
  val Resource = "title-rules.tsv"

  /** What a row takes off or rewrites — the inventory's grouping (docs/investigations/2026-10-22-rule-consolidation.md). */
  val Effects: Set[String] =
    Set("banner", "audience", "event", "edition", "format", "festival", "retrospective", "relay", "rename", "venue", "junk")

  private val Season = """[a-z]{3}(?:-[a-z]{3})?""".r

  /** One row: its rule, what it takes off, the months its billing comes back in, and its examples — billings with what
   *  the row's tier makes of each (the lookup query, the merge-key fold, the proper name, the venue's cleanup;
   *  `TitleRuleExamplesSpec`, `TitleRuleMutationSpec`). */
  final case class Row(rule: TitleRule, effect: String, season: Option[String], seed: Boolean, examples: Seq[(String, String)])

  /** Lines `id kind cinema countries flags effect season pattern replacement note examples`, tab-separated; `#`
   *  comments and blank lines skipped. Flags: `all` (replace every match, not the first), `seed` (the frozen baseline
   *  [[TitleRules]] names). Anything malformed fails loudly: a typo must not drop a rule silently, and a row without an
   *  example is refused — what a row does is shown by one, and the mutation spec reads it. */
  def parse(lines: Iterator[String]): Seq[Row] =
    lines.zipWithIndex.filter { case (line, _) => line.trim.nonEmpty && !line.startsWith("#") }.zipWithIndex.map {
      case ((line, lineNo), position) =>
        def fail(why: String) = throw new IllegalArgumentException(s"$Resource:${lineNo + 1}: $why: '$line'")
        val cells = line.split("\t", -1).toSeq.padTo(11, "")
        if (cells.size > 11) fail("more than 11 columns")
        val Seq(id, kind, cinema, countries, flags, effect, season, pattern, replacement, note, examples) = cells: @unchecked
        if (id.isEmpty || pattern.isEmpty) fail("no id or no pattern")
        val flagSet = flags.split(',').map(_.trim).filter(_.nonEmpty).toSet
        flagSet.diff(Set("all", "seed")).foreach(f => fail(s"unknown flag '$f'"))
        if (!Effects(effect)) fail(s"unknown effect '$effect'")
        if (season.nonEmpty && !Season.matches(season)) fail(s"season '$season' is not mmm or mmm-mmm")
        val scope = kind match {
          case "banner" | "query" => RuleScope.GlobalStructural
          case "key" | "name"     => RuleScope.Canonical
          case "venue"            => RuleScope.PerCinema
          case other              => fail(s"unknown kind '$other'")
        }
        if ((kind == "venue") != cinema.nonEmpty) fail("a venue row names its cinema, and only a venue row does")
        val countrySet = Option.when(countries.nonEmpty)(countries.split(',').toSet.map(code =>
          Country.byCode(code.trim).getOrElse(fail(s"unknown country '$code'"))))
        val rule = TitleRule(id, scope, Option.when(cinema.nonEmpty)(cinema), pattern, replacement, applyAll = flagSet("all"),
          order = position, tag = Option.when(kind == "banner")("programmePrefix"), note = Option.when(note.nonEmpty)(note),
          countries = countrySet, writesProperName = kind == "name")
        if (examples.isEmpty) fail("no examples: a row needs at least one [billing, reading] pair")
        val pairs = scala.util.Try(Json.parse(examples).as[Seq[Seq[String]]]).getOrElse(fail("examples are not a JSON list of pairs")).map {
          case Seq(title, query) => title -> query
          case other             => fail(s"an example is a [billing, reading] pair, not $other")
        }
        if (pairs.isEmpty) fail("no examples: a row needs at least one [billing, reading] pair")
        Row(rule, effect, Option.when(season.nonEmpty)(season), flagSet("seed"), pairs)
    }.toSeq

  lazy val rows: Seq[Row] = {
    val in = Option(getClass.getClassLoader.getResourceAsStream(Resource)).getOrElse(throw new IllegalStateException(s"no $Resource on the classpath"))
    try parse(Source.fromInputStream(in, "UTF-8").getLines()) finally in.close()
  }

  /** Every rule of the table, in its order. */
  lazy val all: Seq[TitleRule] = rows.map(_.rule)
}
