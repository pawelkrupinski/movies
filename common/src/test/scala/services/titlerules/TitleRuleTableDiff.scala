package services.titlerules

import models.Country
import play.api.libs.json.Json

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

/**
 * Two versions of the title-rule table folded over the same titles, every difference printed: the check a MERGE of
 * table rows answers to beyond [[TitleRulesGoldenSpec]]. Merging rows moves some of them to another place in the fold,
 * which matters only where two decorations meet on one title, and the corpus holds few such titles. So besides the
 * corpus this folds synthetic ones: each decoration a rule strips off a corpus title (what it took off the front or
 * the end), stacked two at a time around a bare film, at least one of the two from a rule whose row moved.
 *
 * `scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRuleTableDiff [--countries=pl,uk] [--extra=titles.txt] <before.tsv> <after.tsv>..."`
 */
object TitleRuleTableDiff {

  private def table(path: String) = TitleRuleTable.parse(Files.readAllLines(Paths.get(path), UTF_8).asScala.iterator)

  /** What a rule strips off `text`'s front and end, when it strips one of them. */
  private def decoration(rule: TitleRule, text: String): Option[Either[String, String]] = {
    val out = rule(text)
    if (out == text || out.trim.isEmpty || rule.replacement.nonEmpty) None
    else if (text.startsWith(out)) Some(Right(text.drop(out.length)))
    else if (text.endsWith(out)) Some(Left(text.dropRight(out.length)))
    else None
  }

  def main(args: Array[String]): Unit = {
    val (options, paths) = args.toSeq.partition(_.startsWith("--"))
    def option(name: String) = options.find(_.startsWith(s"--$name=")).map(_.stripPrefix(s"--$name="))
    val countries = option("countries").fold(Country.all)(_.split(',').toSeq.flatMap(Country.byCode))
    val before = table(paths.head)
    // `--extra=<file>`: more titles, one a line (the specs' own venue titles, say)
    val corpus = (TitleRulesGoldenSpec.golden.map(row => (row \ "text").as[String]) ++
      option("extra").toSeq.flatMap(f => Files.readAllLines(Paths.get(f), UTF_8).asScala.filter(_.nonEmpty))).distinct
    val decorations = before.map(_.rule).flatMap(rule => corpus.iterator.flatMap(decoration(rule, _)).distinct.take(2).map(rule.id -> _))
    paths.tail.foreach(after => compare(before, table(after), after, corpus, decorations, countries))
  }

  private def compare(before: Seq[TitleRuleTable.Row], after: Seq[TitleRuleTable.Row], name: String, corpus: Seq[String],
                      decorations: Seq[(String, Either[String, String])], countries: Seq[Country]): Unit = {
    // the rows a merge moved or made: a rule of `before` whose id or pattern `after` no longer has as such
    val kept   = after.map(row => row.rule.id -> row.rule.pattern).toSet
    val moved  = before.filterNot(row => kept(row.rule.id -> row.rule.pattern)).map(_.rule.id).toSet
    val (prefixes, suffixes) = (decorations.collect { case (id, Left(p)) => id -> p }, decorations.collect { case (id, Right(s)) => id -> s })
    val bases = Seq("Lalka", "The Long Walk")
    // each synthetic title tagged by the rows its decorations come from: one row, two moved rows (the merge's own
    // members stacked), or a moved row beside one that did not move (the merge's new place in the fold)
    def pair(a: String, b: String) = if (moved(a) && moved(b)) "stacked members" else "beside another row"
    val stacked = for {
      base <- bases
      text <- prefixes.map(p => (p._2 + base, "one decoration")) ++ suffixes.map(s => (base + s._2, "one decoration")) ++
                (for ((a, p) <- prefixes; (b, s) <- suffixes if moved(a) || moved(b)) yield (p + base + s, pair(a, b))) ++
                (for ((a, s1) <- suffixes; (b, s2) <- suffixes if a != b && (moved(a) || moved(b))) yield (base + s1 + s2, pair(a, b))) ++
                (for ((a, p1) <- prefixes; (b, p2) <- prefixes if a != b && (moved(a) || moved(b))) yield (p1 + p2 + base, pair(a, b)))
    } yield text
    val texts = (corpus.map(_ -> "corpus") ++ stacked).distinctBy(_._1)
    println(s"$name: ${moved.size} rows moved; ${texts.size} titles")
    val venues = (before ++ after).flatMap(_.rule.cinemaId).distinct
    def sets(rows: Seq[TitleRuleTable.Row]) = TitleRulesGoldenSpec.Sets(
      countries.map(c => c -> TitleRuleSet(rows.map(_.rule).filter(_.appliesTo(c)))), venues)
    val (b, a) = (sets(before), sets(after))
    val diffs = texts.iterator.flatMap { case (t, kind) =>
      val (x, y) = (TitleRulesGoldenSpec.reading(t, b), TitleRulesGoldenSpec.reading(t, a))
      Option.when(x != y)(kind -> s"  before ${Json.stringify(x)}\n  after  ${Json.stringify(y)}")
    }.toSeq
    val byKind = diffs.groupBy(_._1)
    println(s"$name: ${diffs.size} titles fold differently: " +
      Seq("corpus", "one decoration", "beside another row", "stacked members").map(k => s"$k ${byKind.get(k).fold(0)(_.size)}").mkString(", "))
    byKind.toSeq.sortBy(_._1).foreach { case (_, ds) => ds.take(3).foreach(d => println(s"[${d._1}]\n${d._2}")) }
  }
}
