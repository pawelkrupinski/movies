package tools

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** One offence a served-corpus check names: the check (`what`, the report's heading), the offender it names
 *  (`subject`: the film, venue and listing -- what stays the same while the same offence stands, never a count or a
 *  first showtime), and the line the report prints. */
final case class ServedFinding(what: String, subject: String, line: String)

/**
 * A served-corpus check's findings, judged against the BASE's over the same recording: what the code added fails,
 * what the base already found is the recording's.
 *
 * WHY. The served-corpus invariants ([[ServedCorpusInvariants]], the convergence suite's no-loss and next-day checks)
 * are strict: a single venue's own data error -- a "Kino 01.11.2026 nieczynne" placeholder listing (Verify #1), a wrong
 * year beside a director spelled otherwise (Verify #31) -- turned `Verify scrape recording` red, though no commit made
 * it. A verify judges ONE commit against ONE recording, so it had nothing to tell the code's findings from the data's.
 *
 * THE BASE IS THE RECORDING'S OWN SUITE. `Record scrape fixtures` runs this same suite over the corpus it records,
 * at the commit it recorded on, and every answer that suite read is what the recording holds (the hermetic premise: a
 * replay of the recording at that commit reads the same tree). Its findings, left in its diagnostics as
 * [[fileOf]], are the base's -- no second replay. The verify leg lays them in [[BaseDirectory]]; a finding the head
 * names and the base did not FAILS (the code since the recording added it), one both name is STANDING: reported, and
 * triaged from the Identity findings report (`scripts/ci/identity_findings_report.py`, which reads the recorder's
 * files), never failed. The drift gate's differential rule, over a recording instead of a seed.
 *
 * NO BASE, NO PASS. A base is used only when its file was written over the SAME recording as this replay (its
 * `# corpus-run` line, [[read]]): a missing file (a recording made before the base was written, a recorder suite that
 * died before the check), another recording's, or none at all leaves every finding failing, as before. The gate's
 * hermetic legs lay no base, so they stay strict -- which is why a re-pin refuses a recording whose verify shows a
 * standing finding (`convergence-publish.sh verified-pin`).
 */
object ServedFindings {

  /** Where a verify leg lays the recording's own suite's findings before its suite (country-convergence-leg.yml). */
  val BaseDirectory: Path = Paths.get("target", "convergence-base-findings")

  private val RunHeader = "# corpus-run "

  /** The diagnostics file one check of one corpus writes: `<corpus>-served-<check>.txt`. */
  def fileOf(corpusKey: String, check: String): String = s"$corpusKey-served-$check"

  /** The findings, under a header naming the recording they were found over; written with none too, so an empty
   *  file says "the check ran and found nothing", not "the check never ran". */
  def write(corpusKey: String, check: String, corpusRun: Option[String], findings: Seq[ServedFinding]): Path =
    ConvergenceDiagnostics.write(fileOf(corpusKey, check),
      corpusRun.map(RunHeader + _).toSeq ++ findings.map(f => Seq(f.what, f.subject, f.line).map(flat).mkString("\t")))

  /** The base's offences -- `(what, subject)` -- over `corpusRun`; None when there is no base for it. */
  def read(directory: Path, corpusKey: String, check: String, corpusRun: Option[String]): Option[Set[(String, String)]] = {
    val file = directory.resolve(fileOf(corpusKey, check) + ".txt")
    Option.when(Files.isRegularFile(file))(Files.readAllLines(file, UTF_8).asScala.toSeq).flatMap { lines =>
      val run = lines.headOption.filter(_.startsWith(RunHeader)).map(_.stripPrefix(RunHeader).trim)
      Option.when(corpusRun.isDefined && run == corpusRun)(
        lines.drop(1).map(_.split("\t", -1)).collect { case Array(what, subject, _*) => what -> subject }.toSet)
    }
  }

  /** What fails and what stands. */
  final case class Judged(failing: Seq[ServedFinding], standing: Seq[ServedFinding])

  def judge(findings: Seq[ServedFinding], base: Option[Set[(String, String)]]): Judged = base match {
    case None        => Judged(findings, Nil)
    case Some(known) =>
      val (standing, failing) = findings.partition(f => known((flat(f.what), flat(f.subject))))
      Judged(failing, standing)
  }

  /** Writes this run's findings (for the next replay's base, the findings report and a pin's check), judges them
   *  against the base laid in [[BaseDirectory]], and returns the failing ones' report -- empty when none fail. */
  def judged(corpusKey: String, check: String, corpusRun: Option[String], findings: Seq[ServedFinding],
             baseDirectory: Path = BaseDirectory): Judged = {
    write(corpusKey, check, corpusRun, findings)
    val verdict = judge(findings, read(baseDirectory, corpusKey, check, corpusRun))
    if (verdict.standing.nonEmpty)
      ConvergenceDiagnostics.summarise(fileOf(corpusKey, check) + "-standing",
        s"$corpusKey $check: ${verdict.standing.size} finding(s) the recording's own suite found too -- the data's, " +
          "reported in the Identity findings report, not failed", report(verdict.standing))
    verdict
  }

  /** The findings as the checks print them: per check, in the order they were found, `N <what>:` over its
   *  offenders' lines, sorted, the first [[ServedCorpusInvariants.Shown]] of them. */
  def report(findings: Seq[ServedFinding]): Seq[String] = {
    val shown = ServedCorpusInvariants.Shown
    findings.map(_.what).distinct.map { what =>
      val lines = findings.filter(_.what == what).map(_.line).sorted
      s"${lines.size} $what:\n  ${lines.take(shown).mkString("\n  ")}" +
        (if (lines.size > shown) s"\n  … and ${lines.size - shown} more" else "")
    }
  }

  private def flat(text: String): String = text.replaceAll("[\t\r\n]+", " ")
}
