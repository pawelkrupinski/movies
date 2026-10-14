package scripts

import services.identity.SuspectTakes
import tools.{IdentityJudgements, SuspectTakesAudit, UnmatchedClusters}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * ONE-SHOT proof of the identity judgements' migration (scripts/identity-judgements-migrate.py): every Scala gate gives
 * the same verdicts reading the OLD files as reading `identity-judgements.tsv`, over the same takes —
 *
 *  - the unmatched-cluster ratchet (UnmatchedClustersRatchetSpec): every take's finding, and the known-wrong audit's
 *    fresh and stale, over the five checked-in captures replayed once;
 *  - the matched rows (MatchedTakesLabelSpec): the wrong takes standing and the tolerances stale;
 *  - the served suspect gate (CountryConvergenceBehaviour's): fresh suspects, wrong takes and gone tolerances, over a
 *    convergence leg's takes (`<cc>-takes/<cc>.json.gz` of its diagnostics), when a directory of them is named.
 *
 *   sbt "worker/Test/runMain scripts.JudgementsEquivalence <old dir> [<leg takes dir>] [<out takes.tsv>]"
 *
 * `<old dir>` holds the old views as the migration script writes them: labels.tsv (the old labels file), and
 * capture.tsv, served.tsv, rows.tsv (country, raw title, film). Exits 1 on any difference.
 */
object JudgementsEquivalence {

  def main(args: Array[String]): Unit = {
    val old       = Path.of(args(0))
    val legs      = args.lift(1).map(Path.of(_)).filter(Files.isDirectory(_))
    val out       = args.lift(2).map(Path.of(_))
    val judged    = IdentityJudgements.read()
    val newLabels = IdentityJudgements.labels(judged)
    val oldLabels = IdentityJudgements.labels(old.resolve("labels.tsv"))
    def keys(name: String): Set[(String, String, String)] = Files.readAllLines(old.resolve(name), StandardCharsets.UTF_8).asScala.toSeq
      .filter(_.nonEmpty).map(_.split("\t", -1)).map(a => (a(0), a(1), a(2))).toSet
    def tolerated(scope: String) = IdentityJudgements.tolerated(judged, scope).keySet
    var differences = 0
    def same[A](what: String, before: A, after: A): Unit =
      if (before == after) println(s"SAME  $what")
      else { differences += 1; println(s"DIFF  $what\n  old: $before\n  new: $after") }

    // the unmatched-cluster ratchet
    val captures = models.Country.all.map(UnmatchedClusters.fixturePath).filter(Files.exists(_)).map(UnmatchedClusters.read)
    val takes    = captures.flatMap(capture => UnmatchedClusters.takes(capture, UnmatchedClusters.replay(capture)))
    val expected = UnmatchedClusters.readTakeLines(UnmatchedClusters.Directory.resolve("expected-matches.tsv"))
    def findings(labels: Seq[UnmatchedClusters.Label]) =
      UnmatchedClusters.findings(takes, labels, expected).map(f => (f.status, f.country, f.venue, f.rawTitle, f.film, f.was))
    val (oldFindings, newFindings) = (findings(oldLabels), findings(newLabels))
    println(s"ratchet: ${takes.size} takes over ${captures.size} captures; findings " +
      newFindings.groupBy(_._1).view.mapValues(_.size).toSeq.sorted.mkString(", "))
    same("ratchet findings (status per take, and every expected take gone)", oldFindings.toSet -- newFindings, newFindings.toSet -- oldFindings)
    def audit(labels: Seq[UnmatchedClusters.Label], known: Set[(String, String, String)]) = {
      val a = UnmatchedClusters.auditWrong(takes.filter(UnmatchedClusters.verdict(_, labels).contains(false)), known)
      (a.fresh.map(UnmatchedClusters.takeLine).toSet, a.stale.map(k => (k._1, UnmatchedClusters.singleSpaced(k._2), k._3)).toSet)
    }
    same("ratchet known-wrong audit (fresh, stale)", audit(oldLabels, keys("capture.tsv")), audit(newLabels, tolerated(IdentityJudgements.Scope.Capture)))

    // the matched rows
    val rows = IdentityUnifiedFit.read(IdentityUnifiedFit.Training).filter(_.today).map(row => UnmatchedClusters.Take(row.country, row.venue,
      row.rawTitle, row.film.stripPrefix("tmdb:").toIntOption.filter(_ => row.film.startsWith("tmdb:")),
      Option.when(row.film.startsWith("imdb:"))(row.film.stripPrefix("imdb:")), "", row.filmTitle))
    def matched(labels: Seq[UnmatchedClusters.Label], listed: Set[(String, String, String)]) = {
      def key(t: UnmatchedClusters.Take) = (t.country, UnmatchedClusters.singleSpaced(t.rawTitle), t.film)
      val wrong = rows.filter(UnmatchedClusters.verdict(_, labels).contains(false))
      val known = listed.map(k => (k._1, UnmatchedClusters.singleSpaced(k._2), k._3))
      (wrong.filterNot(t => known(key(t))).map(UnmatchedClusters.takeLine).toSet, known -- wrong.map(key))
    }
    println(s"matched rows: ${rows.size} of today")
    same("matched rows (wrong standing, tolerances stale)", matched(oldLabels, keys("rows.tsv")), matched(newLabels, tolerated(IdentityJudgements.Scope.Rows)))

    // every take's verdict, ratchet and rows alike
    val all = takes ++ rows
    same("every take's verdict", all.filter(t => UnmatchedClusters.verdict(t, oldLabels) != UnmatchedClusters.verdict(t, newLabels))
      .map(t => s"${UnmatchedClusters.takeLine(t)} ${UnmatchedClusters.verdict(t, oldLabels)}").toSet, Set.empty[String])

    // the served suspect gate over a leg's takes
    legs.foreach { dir =>
      val dumps = Files.walk(dir).iterator.asScala.toSeq.filter(_.getFileName.toString.matches("[a-z]{2}\\.json\\.gz")).filter(_.getParent.getFileName.toString.endsWith("-takes"))
      dumps.sortBy(_.toString).foreach { path =>
        val dump   = UnmatchedClusters.read(path)
        val cc     = dump.country.code
        val films  = dump.films.collect { case (id, Some(f)) => id -> f }
        val corpus = SuspectTakes.corpus(UnmatchedClusters.resolutionOf(dump.decisions), dump.listings, new UnmatchedClusters.Replay(dump), films.get)
        def judge(labels: Seq[UnmatchedClusters.Label], listed: Set[(String, String, String)]) = {
          val v = SuspectTakesAudit.judge(cc, corpus, 2026, labels, listed)
          (v.fresh.map(_.key).toSet, v.wrong.map(t => (t.listing.venue, t.listing.rawTitle, t.film)).toSet,
            v.gone.map(k => (k._1, UnmatchedClusters.singleSpaced(k._2), k._3)).toSet)
        }
        val before = judge(oldLabels, keys("served.tsv"))
        println(s"served ${path.getParent.getFileName}/$cc: ${corpus.takes.size} takes; fresh ${before._1.size}, wrong ${before._2.size}, gone ${before._3.size}")
        same(s"served gate $cc (fresh, wrong, gone)", before, judge(newLabels, tolerated(IdentityJudgements.Scope.Served)))
        out.foreach { file =>
          Files.write(file, corpus.takes.map(t => Seq(cc, t.listing.venue, t.listing.rawTitle.replace('\t', ' ').replace('\n', ' '), t.film).mkString("\t"))
            .asJava, StandardCharsets.UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
        }
      }
    }
    out.foreach { file =>
      Files.write(file, takes.map(t => Seq(t.country, t.venue, t.rawTitle.replace('\t', ' ').replace('\n', ' '), t.film).mkString("\t")).asJava,
        StandardCharsets.UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
    }
    println(if (differences == 0) "EQUIVALENT: every gate gives the same verdicts" else s"$differences DIFFERENCE(S)")
    sys.exit(if (differences == 0) 0 else 1)
  }
}
