package tools

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * THE IDENTITY JUDGEMENTS (`test/resources/fixtures/identity-judgements.tsv`): the one file every gate reads a hand
 * judgement of a take from — one row per (country, venue or "*", raw title, film), its verdict (right, wrong, or an
 * undecided suspect), its state (`open`; `known-wrong`, served with no safe fix yet, in the gates its scope names;
 * `accepted-loss` / `accepted-right-loss`, a loss the identity takes check accepts), why, when, and on what evidence.
 * Its columns, states and rules are documented, and checked, in `scripts/ci/identity_judgements.py` (the lint); this is
 * the Scala reader, keying a billing as the Python one does ([[normalise]], pinned for both by
 * `identity-judgements-normalise.jsonl`).
 *
 * Compiled into old refs by `scripts/identity-slice` (one of its tool files): it leans on nothing but
 * [[UnmatchedClusters.Label]], and reads a ref's legacy `labels.tsv` too.
 */
object IdentityJudgements {

  val CheckedIn: Path = Path.of("test", "resources", "fixtures", "identity-judgements.tsv")
  val Columns: Seq[String] = Seq("country", "venue", "rawTitle", "film", "verdict", "state", "scope", "filmTitle", "reason", "judged", "evidence")
  private val LegacyLabelColumns = Seq("country", "venue", "rawTitle", "film", "verdict", "note")

  /** The gates a known-wrong row lets its wrong takes stand in. */
  object Scope {
    val Capture      = "capture"       // the unmatched-cluster ratchet (UnmatchedClustersRatchetSpec)
    val Served       = "served"        // a convergence leg's suspect gate (SuspectTakesAudit)
    val Rows         = "rows"          // the model's matched takes in the recorded rows (MatchedTakesLabelSpec)
  }

  final case class Row(country: String, venue: String, rawTitle: String, film: String, verdict: String, state: String, scope: Set[String],
                       filmTitle: String, reason: String, judged: String, evidence: String) {
    def judgedRight: Option[Boolean] = verdict match { case "right" => Some(true); case "wrong" => Some(false); case _ => None }
    /** The billing a tolerance covers at every venue: (country, raw title, film). */
    def billing: (String, String, String) = (country, normalise(rawTitle), film)
  }

  /** A billing (or venue) as every judgement keys it: runs of whitespace — a tab, a line break, a no-break space, two
   *  spaces — one space, none at the ends. A venue's re-spacing is not a new billing: DE Caligari's
   *  "ES  (ULRICH SCHAMONI)", judged so, came back "ES (ULRICH SCHAMONI)" a recording later. */
  def normalise(raw: String): String = raw.replaceAll("(?U)\\s+", " ").stripPrefix(" ").stripSuffix(" ")

  def read(path: Path = CheckedIn): Seq[Row] = {
    val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq.filterNot(line => line.trim.isEmpty || line.startsWith("#"))
    val header = lines.headOption.map(_.split("\t", -1).toSeq).getOrElse(Nil)
    val body = lines.drop(1)
    if (header == Columns) body.map { line =>
      line.split("\t", -1) match {
        case Array(cc, venue, raw, film, verdict, state, scope, title, reason, judged, evidence) =>
          Row(cc, venue, raw, film, verdict, state, scope.split(",").filter(_.nonEmpty).toSet, title, reason, judged, evidence)
        case _ => throw new IllegalArgumentException(s"$path: malformed judgement line: $line")
      }
    }
    // a ref from before the judgements were one file judged by its own labels.tsv (scripts/identity-tools.sh)
    else if (header == LegacyLabelColumns) body.map { line =>
      line.split("\t", -1) match {
        case Array(cc, venue, raw, film, verdict, note) => Row(cc, venue, raw, film, verdict, "open", Set.empty, "", "", "", note)
        case _ => throw new IllegalArgumentException(s"$path: malformed label line: $line")
      }
    }
    else throw new IllegalArgumentException(s"$path: header is not ${Columns.mkString("\t")}")
  }

  /** The rows judging a film right or wrong, as the verdict reads them ([[UnmatchedClusters.verdict]]). */
  def labels(rows: Seq[Row]): Seq[UnmatchedClusters.Label] =
    rows.flatMap(row => row.judgedRight.map(UnmatchedClusters.Label(row.country, row.venue, row.rawTitle, row.film, _, row.evidence)))

  def labels(path: Path): Seq[UnmatchedClusters.Label] = labels(read(path))

  /** The checked-in judgements' labels. */
  def labels(): Seq[UnmatchedClusters.Label] = labels(read())

  /** The billings whose wrong takes the gate `scope` lets stand, each with why: a known-wrong row at any of its venues
   *  covers every venue billing it — and, for [[Scope.Served]], the undecided suspects standing since it was armed. */
  def tolerated(rows: Seq[Row], scope: String): Map[(String, String, String), String] =
    rows.filter(row => row.scope(scope) && (row.state == "known-wrong" || (scope == Scope.Served && row.verdict == "undecided")))
      .map(row => row.billing -> row.reason).toMap

  def tolerated(scope: String): Map[(String, String, String), String] = tolerated(read(), scope)
}
