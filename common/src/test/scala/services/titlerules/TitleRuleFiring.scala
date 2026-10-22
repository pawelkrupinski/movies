package services.titlerules

import models.Country

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
 * Which of the named table rows fire on any of a list of strings: the check a row's removal answers to beyond the
 * golden corpus (the specs' literals, every checked-in fixture's text, every cached recording's billings). Three levels,
 * each row reported at the strongest it reaches:
 *
 *  - `fold`: the row fires inside a country's real fold (lookup query, merge key) or a venue's cleanup, any venue;
 *  - `alone`: the row alone changes the string (scoped to no country);
 *  - `unanchored`: its pattern, without the leading `^` / trailing `$`, matches inside the string — a decoration the
 *    row would take off once something around it is gone; listed for a reader to judge.
 *
 * `scripts/sbt-warm "common/Test/runMain services.titlerules.TitleRuleFiring <ids.txt> <strings.txt> <out.tsv>"`
 */
object TitleRuleFiring {

  private val Anchors = """^(\(\?[a-zA-Z-]+\))?\^""".r

  def main(args: Array[String]): Unit = {
    val Array(idsFile, stringsFile, out) = args: @unchecked
    val ids     = Files.readAllLines(Paths.get(idsFile), UTF_8).asScala.map(_.trim).filter(_.nonEmpty).toSet
    val all     = TitleRuleSet(TitleRuleTable.all.map(_.copy(countries = None))).effectiveRules
    val wanted  = all.filter(r => ids(r.id))
    ids.diff(wanted.map(_.id).toSet).foreach(id => println(s"no row '$id' in the table"))
    val loose   = wanted.map(r => r -> scala.util.Try(Anchors.replaceFirstIn(r.pattern, "$1").stripSuffix("$").r).toOption)
    val sets    = Country.all.map(TitleRuleSet.forCountry)
    val venues  = all.flatMap(_.cinemaId).distinct
    val hits    = mutable.Map.empty[(String, String), mutable.LinkedHashSet[String]]
    def hit(id: String, level: String, s: String) = {
      val set = hits.getOrElseUpdate(id -> level, mutable.LinkedHashSet.empty)
      if (set.size < 5) set += s
    }
    var n = 0
    Files.lines(Paths.get(stringsFile), UTF_8).iterator().asScala.foreach { s =>
      n += 1
      val near = loose.filter { case (r, re) => r(s) != s || re.exists(_.findFirstIn(s).nonEmpty) }
      if (near.nonEmpty) {
        val fired = sets.iterator.flatMap(set => set.firedSearch(s) ++ set.firedCanonical(s) ++ venues.flatMap(set.firedPerCinema(_, s))).toSet
        near.foreach { case (r, _) =>
          if (fired(r.id)) hit(r.id, "fold", s)
          else if (r(s) != s) hit(r.id, "alone", s)
          else hit(r.id, "unanchored", s)
        }
      }
    }
    val lines = wanted.map(_.id).sorted.map { id =>
      val level = Seq("fold", "alone", "unanchored").find(l => hits.contains(id -> l)).getOrElse("none")
      (Seq(id, level) ++ hits.get(id -> level).toSeq.flatten).mkString("\t")
    }
    Files.write(Paths.get(out), lines.asJava, UTF_8)
    println(s"$n strings; ${lines.count(!_.contains("\tnone"))} of ${wanted.size} rows reach something; wrote $out")
  }
}
