package scripts

import tools.{CommandLine, IdentityJudgements, UnmatchedClusters}
import tools.UnmatchedClusters.{Gap, Take}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
 * The clusters the identity model leaves unmatched on the recorded full corpora ([[UnmatchedClusters]]' fixture),
 * resolved and put to the agreement stage again over their captured answers — the ratchet for matching more of them:
 *
 *  - ZERO WRONG: but those the identity judgements hold known-wrong in scope `capture` (no safe fix removes them; a
 *    cured row fails as stale, so they only shrink), no take a judgement denies (`identity-judgements.tsv`: the recall
 *    targets, the must-not pairs, hand labels), none contradicting the right film one names, and every take judged — a
 *    take no judgement covers fails, to be judged there before it counts;
 *  - ONLY MORE: every right take of `expected-matches.tsv` is still taken. A run that takes more right ones writes the
 *    new list to `<out>/expected-matches.tsv` — copy it over the checked-in one to ratchet.
 *
 * Every take's finding goes to `<out>/ratchet-takes.tsv` ([[UnmatchedClusters.writeFindings]]) before any check reads
 * them, so a red ratchet still leaves it to read. `services.identity.UnmatchedClustersRatchetSpec` asserts the same
 * checks, one test each: the gate CI runs. This is its command line, and the identity takes check's:
 *
 * {{{
 *   sbt "worker/Test/runMain scripts.UnmatchedClustersRatchet [--measure] [--fixtures dir] [--judgements tsv] [--out dir]"
 * }}}
 *
 * `--measure` is the takes check's reading: it MEASURES a change, its verdict the base-vs-head diff of the takes, never
 * this one's. A lookup the capture lacks (one the change newly asks) is read as unknown and listed in
 * `<out>/capture-gaps.tsv` rather than failing the capture as stale and leaving every country without takes, and the
 * run exits 0 once the takes are written. Otherwise exit 0 is a green ratchet and 1 a check failed, each named.
 */
object UnmatchedClustersRatchet {

  val commandLine: CommandLine = CommandLine("UnmatchedClustersRatchet",
    "Replays the unmatched-cluster captures and checks their takes against the identity judgements.",
    CommandLine.Value("fixtures", "dir", "the captures (<cc>.json.gz) and expected-matches.tsv", default = Some(UnmatchedClusters.Directory.toString)),
    CommandLine.Value("judgements", "tsv", "the identity judgements", default = Some(IdentityJudgements.CheckedIn.toString)),
    CommandLine.Value("out", "dir", "where ratchet-takes.tsv, capture-gaps.tsv and a moved expected-matches.tsv go",
      default = Some(UnmatchedClusters.FindingsPath.getParent.toString)),
    CommandLine.Flag("measure", "read a capture's gaps as unknown, list them, and exit 0 once the takes are written"))

  /** The countries whose captures the fixture holds. */
  val Countries: Set[String] = Set("pl", "uk", "de", "es", "us")

  /** One check of the ratchet: what it holds, and why it fails, if it does — read on first use. */
  final class Check(val name: String, why: => Option[String]) { lazy val failure: Option[String] = why }

  /** One run of the ratchet over `fixtures`, judged by `judgements`, its files written to `out`. Each value is computed
   *  on first use, as the spec's tests read them; [[checks]] reads them all. */
  final class Run(fixtures: Path, judgements: Path, out: Path, measure: Boolean) {

    private val judged    = IdentityJudgements.read(judgements)
    private val labels    = IdentityJudgements.labels(judged)
    private val expected  = fixtures.resolve("expected-matches.tsv")
    val captures: Seq[UnmatchedClusters.Capture] =
      models.Country.all.map(country => fixtures.resolve(s"${country.code}.json.gz")).filter(Files.exists(_)).map(UnmatchedClusters.read)

    /** The takes of every capture's replay — or, outside a measurement, the first capture that lacks answers its replay
     *  asks, and those gaps (up to five): its findings are then never written. */
    lazy val takes: Either[(String, Seq[Gap]), Seq[Take]] = {
      val replayed = captures.map(capture => (capture, UnmatchedClusters.replay(capture)))
      val gaps     = replayed.flatMap { case (capture, outcome) => UnmatchedClusters.gaps(capture, outcome) }
      val stale    = if (measure) { UnmatchedClusters.writeGaps(out.resolve("capture-gaps.tsv"), gaps); None }
                     else captures.map(_.country.code).map(cc => cc -> gaps.filter(_.country == cc)).find(_._2.nonEmpty)
      stale.toLeft(replayed.flatMap { case (capture, outcome) => UnmatchedClusters.takes(capture, outcome) })
    }

    /** The findings file, written before any check reads the takes: a red ratchet still leaves it to read. */
    lazy val reported: Either[String, Seq[Take]] = takes.map { taken =>
      UnmatchedClusters.writeFindings(out.resolve("ratchet-takes.tsv"),
        UnmatchedClusters.findings(taken, labels, UnmatchedClusters.readTakeLines(expected)))
      taken
    }.left.map { case (cc, its) =>
      s"$cc: the fixture lacks answers this resolve asks — re-capture it (scripts/identity-capture.sh): " +
        its.take(5).map(gap => s"${gap.kind} ${gap.lookup}").mkString("; ")
    }

    def missingDetail: Seq[(String, Map[String, Int])] =
      captures.map(capture => capture.country.code -> UnmatchedClusters.hostsMissingDetail(capture)).filter(_._2.nonEmpty)

    lazy val wrongAudit: Either[String, UnmatchedClusters.WrongAudit] = reported.map(taken =>
      UnmatchedClusters.auditWrong(taken.filter(take => UnmatchedClusters.verdict(take, labels).contains(false)),
        IdentityJudgements.tolerated(judged, IdentityJudgements.Scope.Capture).keySet))

    lazy val unjudged: Either[String, Seq[Take]] = reported.map(_.filter(take => UnmatchedClusters.verdict(take, labels).isEmpty))

    lazy val right: Either[String, Seq[Take]] = reported.map(_.filter(take => UnmatchedClusters.verdict(take, labels).contains(true)))

    /** The expected right takes no longer taken; with the right takes moved, the new list written to `<out>`. */
    lazy val lost: Either[String, Set[(String, String, String, String)]] = right.map { taken =>
      val had = UnmatchedClusters.readTakeLines(expected)
      val now = taken.map(take => (take.country, take.venue, take.rawTitle, take.film)).toSet
      if (now != had) {
        val candidate = out.resolve("expected-matches.tsv")
        Files.createDirectories(candidate.getParent)
        Files.writeString(candidate, taken.map(UnmatchedClusters.takeLine).mkString("country\tvenue\trawTitle\tfilm\ttitle\n", "\n", "\n"),
          StandardCharsets.UTF_8)
        println(s"the right takes moved: wrote $candidate (+${(now -- had).size} −${(had -- now).size})")
      }
      had -- now
    }

    /** What the right takes are, per country, beside how many `expected-matches.tsv` holds. */
    def rightSummary: Either[String, String] = right.map { taken =>
      val counts = taken.groupBy(_.country).view.mapValues(_.size).toSeq.sorted.map { case (cc, n) => s"$cc $n" }.mkString(", ")
      s"right takes: ${taken.size} listings ($counts); expected ${UnmatchedClusters.readTakeLines(expected).size}"
    }

    def capturedCountries: Set[String] = captures.map(_.country.code).toSet

    /** Every check, in order, each failure said whole. */
    val checks: Seq[Check] = {
      def check[A](name: String, value: => Either[String, A])(failure: A => Option[String]) = new Check(name, value.fold(Some(_), failure))
      Seq(
        new Check("hold every country's capture", Option.when(capturedCountries != Countries)(
          s"the fixture holds captures of ${capturedCountries.toSeq.sorted.mkString(", ")}, not ${Countries.toSeq.sorted.mkString(", ")}")),
        new Check("hold the venue detail of every host it marks as having detail", Option.when(missingDetail.nonEmpty)(
          "a capture marks listings as having venue detail and holds none of a host's — its recording's reads were refused; " +
            s"re-record the corpus (country -> host -> listings): ${missingDetail.mkString(", ")}")),
        check("take no film a judgement denies or contradicts, but those known-wrong in scope capture", wrongAudit)(audit =>
          Option.when(audit.fresh.nonEmpty)(audit.fresh.map(UnmatchedClusters.takeLine).mkString("WRONG takes:\n", "\n", "\n"))),
        check("still take every wrong film known-wrong in scope capture — a cured row is struck, so they only shrink", wrongAudit)(audit =>
          Option.when(audit.stale.nonEmpty)(audit.stale.map(_.productIterator.mkString("\t")).mkString("known-wrong (capture) judgements " +
            "no longer taken (STALE — drop `capture` from the row's scope, or the row's state back to open):\n", "\n", "\n"))),
        check("take only films judged right — a new take is judged in identity-judgements.tsv before it counts", unjudged)(taken =>
          Option.when(taken.nonEmpty)(taken.map(take => s"${UnmatchedClusters.takeLine(take)}\t${take.basis}\t${take.ids.mkString(",")}")
            .mkString("UNJUDGED takes:\n", "\n", "\n"))),
        check("keep every right take of expected-matches.tsv, and write the list when more are right", lost)(gone =>
          Option.when(gone.nonEmpty)(gone.toSeq.sorted.mkString("right takes LOST:\n", "\n", "\n"))))
    }
  }

  def main(args: Array[String]): Unit = {
    val arguments = commandLine.parseOrExit(args)
    val measure   = arguments.flag("measure")
    val run       = new Run(arguments.requiredPath("fixtures"), arguments.requiredPath("judgements"), arguments.requiredPath("out"), measure)
    val checks    = run.checks
    run.rightSummary.foreach(println)
    checks.foreach { c => println(s"[ratchet] ${if (c.failure.isEmpty) "ok" else "FAILED"} — ${c.name}"); c.failure.foreach(println) }
    val failed = checks.count(_.failure.nonEmpty)
    if (measure) {
      // a measurement's verdict is the diff of its takes: it fails only when it wrote none
      if (run.takes.isLeft) { println("[ratchet] no takes written"); sys.exit(1) }
      println(s"[ratchet] measured: takes written to ${arguments.requiredPath("out").resolve("ratchet-takes.tsv")} ($failed check(s) failed, not judged here)")
    } else if (failed > 0) {
      println(s"[ratchet] $failed of ${checks.size} check(s) FAILED")
      sys.exit(1)
    } else println(s"[ratchet] green: ${checks.size} check(s) passed")
  }
}
