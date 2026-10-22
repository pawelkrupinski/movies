package services.titlerules

import models.Country
import services.billing.BillingParse
import services.movies.TitleNormalizer

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

/**
 * Would the pipeline notice a row gone? For each named row, every country whose rule set holds it is built twice (the
 * table with and without the row), and each string the row could touch is read the ways the pipeline's consumers
 * read a title: each venue's cleanup (`cinemaClean`, which the scrapers store), the listing title after the format peel
 * (`BillingParse.peeled`), and over those and the fact-free title the lookup query (`apiQuery`, of the recased title
 * too), `searchQuery`, the stable key (`sanitize`), the display pick, the proper name, the programme banner and the
 * recasing. A row whose removal changes none of them on any string is one the parser (or another row) provably covers
 * on every title the repository knows. With `--together`, the named rows are removed at once instead (a batch's check).
 *
 * `scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRulePipelineDiff [--together] <ids.txt> <strings.txt> <out.tsv>"`
 * — strings one a line (the golden corpus is added); a string is read for a row only when the row's pattern, unanchored,
 * occurs in it or the row changes it.
 */
object TitleRulePipelineDiff {

  private val Anchors = """^(\(\?[a-zA-Z-]+\))?\^""".r

  def main(args: Array[String]): Unit = {
    val (flags, rest) = args.toSeq.partition(_.startsWith("--"))
    val Seq(idsFile, stringsFile, out) = rest: @unchecked
    val together = flags.contains("--together")
    val ids      = Files.readAllLines(Paths.get(idsFile), UTF_8).asScala.map(_.trim).filter(_.nonEmpty).toSeq
    val table    = TitleRuleTable.all
    val expanded = TitleRuleSet(table).effectiveRules.map(r => r.id -> r).toMap
    val strings  = (TitleRulesGoldenSpec.golden.map(row => (row \ "text").as[String]) ++
      Files.readAllLines(Paths.get(stringsFile), UTF_8).asScala.filter(_.nonEmpty)).distinct
    val venues   = table.flatMap(_.cinemaId).distinct
    println(s"${strings.size} strings, ${venues.size} venues")

    // the strings each row could touch, found in one pass: it changes them alone, or its pattern occurs in them unanchored
    val probes = ids.flatMap(id => expanded.get(id).map { r =>
      val loose = scala.util.Try(Anchors.replaceFirstIn(r.pattern, "$1").stripSuffix("$").r).toOption
      (id, r.copy(countries = None), loose)
    })
    val pool = new java.util.concurrent.ForkJoinPool(4)
    def parallel[A, B](as: Seq[A])(f: A => B): Seq[B] =
      pool.submit(() => as.asJava.parallelStream().map(a => f(a)).toList.asScala.toSeq).get()
    val touched: Map[String, Seq[String]] = parallel(probes) { case (id, r, loose) =>
      id -> strings.filter(s => r(s) != s || loose.forall(_.findFirstIn(s).nonEmpty))
    }.toMap
    ids.filterNot(expanded.contains).foreach(id => println(s"no row '$id' in the table"))

    def readings(n: TitleNormalizer, s: String): Seq[String] = {
      val cleans   = ("" +: venues).map(v => n.cinemaClean(v, s))
      val listings = cleans.map(c => BillingParse(c).peeled.title)
      val texts    = (n.factFree(s) +: (cleans ++ listings)).distinct
      cleans ++ listings ++ texts.flatMap(t => Seq(n.apiQuery(t), n.apiQuery(n.recase(t)), n.searchQuery(t), n.sanitize(t),
        n.properNamed(t), n.programmePrefix(t).toString, n.recase(t), n.preferredDisplay(Seq(t)).toString))
    }

    // stops at `limit` differences: a row is retired only with none
    def check(removed: Set[String], limit: Int): (Int, Seq[String]) = {
      val texts = removed.toSeq.flatMap(id => touched.getOrElse(id, Nil)).distinct
      val countries = Country.all.filter(c => table.exists(r => removed(r.id) && r.appliesTo(c)))
      val diffs = countries.iterator.flatMap { c =>
        val before = new TitleNormalizer(TitleRuleSet(table.filter(_.appliesTo(c))), facts = c.titleFacts)
        val after  = new TitleNormalizer(TitleRuleSet(table.filter(r => r.appliesTo(c) && !removed(r.id))), facts = c.titleFacts)
        texts.iterator.flatMap { s =>
          val (b, a) = (readings(before, s), readings(after, s))
          b.zip(a).find(p => p._1 != p._2).map { case (x, y) => s"${c.code}: '$s' read '$x', without '$y'" }
        }
      }.take(limit).toSeq
      (texts.size, diffs)
    }

    val lines =
      if (together) { val (n, d) = check(ids.toSet, 20); Seq((Seq("together", n.toString, d.size.toString) ++ d).mkString("\t")) }
      else parallel(ids.filter(expanded.contains)) { id =>
        val (n, d) = check(Set(id), 3)
        println(s"$id: $n strings, ${if (d.isEmpty) "no difference" else "differs"}")
        (Seq(id, n.toString, d.size.toString) ++ d).mkString("\t")
      }
    pool.shutdown()
    Files.write(Paths.get(out), lines.asJava, UTF_8)
    println(s"wrote $out: ${lines.count(_.split("\t")(2) == "0")} of ${lines.size} with no difference")
  }
}
