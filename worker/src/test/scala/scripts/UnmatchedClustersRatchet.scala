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
 *   sbt "worker/Test/runMain scripts.UnmatchedClustersRatchet [--measure] [--countries pl,uk] [--fixtures dir] [--judgements tsv] [--out dir]"
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
    CommandLine.Value("countries", "codes", "replay only these countries' captures, comma-separated (default: every capture)"),
    CommandLine.Flag("measure", "read a capture's gaps as unknown, list them, and exit 0 once the takes are written"),
    CommandLine.Value("trace", "file", "also write every title reading the replay made there (services.movies.NormalizerTrace): what " +
      "scripts/identity-measure --footprint replays under a title-rule change"))

  /** The countries whose captures the fixture holds. */
  val Countries: Set[String] = Set("pl", "uk", "de", "es", "us")

  /** One check of the ratchet: what it holds, and why it fails, if it does — read on first use. */
  final class Check(val name: String, why: => Option[String]) { lazy val failure: Option[String] = why }

  /** One run of the ratchet over `fixtures`, judged by `judgements`, its files written to `out`. Each value is computed
   *  on first use, as the spec's tests read them; [[checks]] reads them all. `countries` scopes it to those captures and
   *  their expected takes: a takes check row diffs one country's final takes, and replaying the other four's captures
   *  was most of its replay (Germany's and the UK's are the largest). */
  final class Run(fixtures: Path, judgements: Path, out: Path, measure: Boolean, countries: Set[String] = Countries) {

    private val judged    = IdentityJudgements.read(judgements)
    private val labels    = IdentityJudgements.labels(judged)
    private val expectedPath = fixtures.resolve("expected-matches.tsv")
    private lazy val expected = UnmatchedClusters.readTakeLines(expectedPath).filter(take => countries(take._1))
    val captures: Seq[UnmatchedClusters.Capture] = models.Country.all.filter(country => countries(country.code))
      .map(country => fixtures.resolve(s"${country.code}.json.gz")).filter(Files.exists(_)).map(UnmatchedClusters.read)

    /** The takes of every capture's replay — or, outside a measurement, the first capture that lacks answers its replay
     *  asks, and those gaps (up to five): its findings are then never written. */
    lazy val takes: Either[(String, Seq[Gap]), Seq[Take]] = {
      // the captures side by side, each replayed under its own country's normalizer (as IdentityRefit measures them):
      // one after another they were the ratchet spec's 342 s (CI, 2026-10-27)
      val pool     = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, captures.size))
      val replayed = try ParallelTasks.all(captures)(capture => (capture, UnmatchedClusters.replay(capture)))(
        using scala.concurrent.ExecutionContext.fromExecutor(pool)) finally pool.shutdown()
      val gaps     = replayed.flatMap { case (capture, outcome) => UnmatchedClusters.gaps(capture, outcome) }
      val stale    = if (measure) { UnmatchedClusters.writeGaps(out.resolve("capture-gaps.tsv"), gaps); None }
                     else captures.map(_.country.code).map(cc => cc -> gaps.filter(_.country == cc)).find(_._2.nonEmpty)
      stale.toLeft(replayed.flatMap { case (capture, outcome) => UnmatchedClusters.takes(capture, outcome) })
    }

    /** The findings file, written before any check reads the takes: a red ratchet still leaves it to read. */
    lazy val reported: Either[String, Seq[Take]] = takes.map { taken =>
      UnmatchedClusters.writeFindings(out.resolve("ratchet-takes.tsv"),
        UnmatchedClusters.findings(taken, labels, expected))
      taken
    }.left.map { case (cc, its) =>
      // a change that asks more of the captured clusters (a listing it leaves unmatched, a new search) needs only those
      // answers: a fill, minutes, not a capture of the whole corpus
      s"$cc: the fixture lacks answers this resolve asks — fill them: scripts/identity-capture.sh --fill $cc: " +
        its.take(5).map(gap => s"${gap.kind} ${gap.lookup}").mkString("; ")
    }

    def missingDetail: Seq[(String, Map[String, Int])] =
      captures.map(capture => capture.country.code -> UnmatchedClusters.hostsMissingDetail(capture)).filter(_._2.nonEmpty)

    lazy val wrongAudit: Either[String, UnmatchedClusters.WrongAudit] = reported.map(taken =>
      UnmatchedClusters.auditWrong(taken.filter(take => UnmatchedClusters.verdict(take, labels).contains(false)),
        IdentityJudgements.tolerated(judged, IdentityJudgements.Scope.Capture).keySet.filter(billing => countries(billing._1))))

    lazy val unjudged: Either[String, Seq[Take]] = reported.map(_.filter(take => UnmatchedClusters.verdict(take, labels).isEmpty))

    lazy val right: Either[String, Seq[Take]] = reported.map(_.filter(take => UnmatchedClusters.verdict(take, labels).contains(true)))

    /** The expected right takes no longer taken; with the right takes moved, the new list written to `<out>` -- by an
     *  unscoped run only: a scoped one's list would drop every other country's. */
    lazy val lost: Either[String, Set[(String, String, String, String)]] = right.map { taken =>
      val had = expected
      val now = taken.map(take => (take.country, take.venue, take.rawTitle, take.film)).toSet
      if (now != had && countries == Countries) {
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
      s"right takes: ${taken.size} listings ($counts); expected ${expected.size}"
    }

    def capturedCountries: Set[String] = captures.map(_.country.code).toSet

    /** Every check, in order, each failure said whole. */
    val checks: Seq[Check] = {
      def check[A](name: String, value: => Either[String, A])(failure: A => Option[String]) = new Check(name, value.fold(Some(_), failure))
      Seq(
        new Check("hold every country's capture", Option.when(capturedCountries != countries)(
          s"the fixture holds captures of ${capturedCountries.toSeq.sorted.mkString(", ")}, not ${countries.toSeq.sorted.mkString(", ")}")),
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
    val countries = arguments.get("countries").fold(Countries)(_.split(",").map(_.trim).filter(_.nonEmpty).toSet)
    countries.diff(Countries).foreach(code => arguments.fail(s"--countries: no capture of $code (one of ${Countries.toSeq.sorted.mkString(", ")})"))
    // the captures' reading and replay inside the trace: every check reads the takes they made, and is computed here
    val (run, checks) = services.movies.NormalizerTrace.recording(arguments.path("trace")) {
      val run = new Run(arguments.requiredPath("fixtures"), arguments.requiredPath("judgements"), arguments.requiredPath("out"), measure, countries)
      val checks = run.checks
      checks.foreach(_.failure)
      (run, checks)
    }
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
