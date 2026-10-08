package services.review

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.Instant
import scala.jdk.CollectionConverters._

/** One row of `test/resources/fixtures/identity-unmatched/labels.tsv`: a listing (country, the venue
 *  or `*` for every venue billing the raw title, the raw title) judged right or wrong for `film`. */
final case class LabelRow(country: String, venue: String, rawTitle: String, film: String, verdict: String, note: String) {
  def line: String = Seq(country, venue, rawTitle, film, verdict, note).mkString("\t")
  def right: Boolean = verdict == "right"
  /** Whether this row and `other` judge the same listing against the same film. */
  def sameJudgement(other: LabelRow): Boolean =
    country == other.country && rawTitle == other.rawTitle && film == other.film &&
      (venue == other.venue || venue == "*" || other.venue == "*")
}

object LabelsTsv {
  val Header = "country\tvenue\trawTitle\tfilm\tverdict\tnote"
  /** Where the checkout keeps it, relative to the repository root. */
  val RelativePath: Path = Paths.get("test", "resources", "fixtures", "identity-unmatched", "labels.tsv")

  /** The checkout's labels file, from the working directory or one above it (`sbt web/run` runs from the root,
   *  a forked `web` process from `web/`). */
  def locate(from: Path = Paths.get("").toAbsolutePath): Path =
    Iterator.iterate(from)(_.getParent).takeWhile(_ != null).take(3).map(_.resolve(RelativePath))
      .find(Files.exists(_)).getOrElse(from.resolve(RelativePath))

  def parse(lines: Seq[String]): Seq[LabelRow] =
    lines.filter(_.nonEmpty).filterNot(_ == Header).map { line =>
      line.split("\t", -1) match {
        case Array(country, venue, raw, film, verdict, note) => LabelRow(country, venue, raw, film, verdict, note)
        case other => throw new IllegalArgumentException(s"labels.tsv: ${other.length} fields, want 6: $line")
      }
    }

  def read(path: Path): Seq[LabelRow] =
    if (Files.exists(path)) parse(Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toSeq) else Nil

  def write(path: Path, rows: Seq[LabelRow]): Unit =
    Files.write(path, (Header +: rows.map(_.line)).mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8)): Unit

  /** Beside the labels file: when the newest review answer already turned into its rows was given. Checked in with
   *  the rows, so an export takes only the answers given since — never re-applies one a later edit overruled — and
   *  knows which answers stood when it last ran, to take back the rows of one withdrawn since. */
  def usedPath(path: Path): Path = path.resolveSibling(s"${path.getFileName}.used-until")

  def usedUntil(path: Path): Option[Instant] =
    Option.when(Files.exists(usedPath(path)))(Instant.parse(Files.readString(usedPath(path), StandardCharsets.UTF_8).trim))

  def markUsed(path: Path, until: Instant): Unit =
    Files.writeString(usedPath(path), s"$until\n", StandardCharsets.UTF_8): Unit
}

/**
 * Review answers as `labels.tsv` rows, merged into the rows the file already holds — only the answers given since
 * the last export ([[LabelsTsv.usedUntil]]).
 *
 *  - right / wrong: that verdict for the film the card showed;
 *  - another film: right for it, and wrong for the film the card showed when that is provably another film
 *    ([[FilmIdentity]]) — a correction naming the shown film by another database's id rules nothing out;
 *  - none of these / not a film / double bill: wrong for the film the card showed;
 *  - one row per distinct raw title of the cluster, under its venue when one venue bills it, `*` otherwise.
 *
 * A row the file already holds is never written twice. A row it holds with the OPPOSITE verdict is
 * flipped in place, its old verdict and note kept after "was:". An answer exported before and no longer
 * standing (undone, or replaced by a newer answer) takes its rows back: a row it flipped is its "was:"
 * again, a row it added goes — unless an answer still standing gives that row too.
 */
object LabelsExport {

  final case class Summary(added: Int, flipped: Int, unchanged: Int, unexportable: Seq[String], warnings: Seq[String],
                           alreadyUsed: Int = 0, withdrawn: Int = 0) {
    def render: String =
      ((s"labels.tsv: $added added, $flipped flipped, $unchanged already there" +
        (if (alreadyUsed > 0) s"; $alreadyUsed answer${if (alreadyUsed == 1) "" else "s"} already used, skipped" else "") +
        (if (withdrawn > 0) s"; $withdrawn row${if (withdrawn == 1) "" else "s"} of withdrawn answers taken back" else "")) +:
        (unexportable.map("not exported: " + _) ++ warnings.map("WARNING: " + _))).mkString("\n")
  }

  def rowsOf(answer: ReviewAnswer, identity: FilmIdentity = FilmIdentity.Unlinked): Seq[LabelRow] = {
    def rows(film: FilmFacts, right: Boolean, note: String): Seq[LabelRow] =
      answer.members.groupBy(_.rawTitle).toSeq.sortBy(_._1).map { case (raw, members) =>
        val venues = members.map(_.venue).distinct
        LabelRow(answer.country, if (venues.size == 1) venues.head else "*", raw, film.ref.render,
          if (right) "right" else "wrong", s"review page: $note")
      }
    val shown = answer.shown.toSeq
    answer.verdict match {
      case ReviewVerdict.Right       => shown.flatMap(f => rows(f, right = true, s"right: ${f.describe}"))
      case ReviewVerdict.Wrong       => shown.flatMap(f => rows(f, right = false, s"wrong: ${f.describe}"))
      case ReviewVerdict.NoneOfThese => shown.flatMap(f => rows(f, right = false, s"none of the candidates: ${f.describe}"))
      case ReviewVerdict.Event       => shown.flatMap(f => rows(f, right = false, s"not a film: ${f.describe}"))
      case ReviewVerdict.Bill        => shown.flatMap(f => rows(f, right = false, s"double bill: ${f.describe}"))
      case ReviewVerdict.Film        =>
        answer.ref.toSeq.flatMap { ref =>
          val chosen = shown.find(_.ref == ref).getOrElse(FilmFacts(ref))
          rows(chosen, right = true, s"hand label: ${chosen.describe}") ++
            // the film shown is ruled out only when it is PROVABLY another film: the same film often goes by another
            // database's id on the label the answer corrects (filmweb:10008278 and tmdb:1157322 are one Franz)
            shown.filter(f => identity.provablyDifferent(f.ref, ref)).flatMap(f => rows(f, right = false, s"must not: ${f.describe}"))
        }
      case ReviewVerdict.Unsure | ReviewVerdict.Undo => Nil
    }
  }

  def merge(existing: Seq[LabelRow], answers: Seq[ReviewAnswer], identity: FilmIdentity = FilmIdentity.Unlinked): (Seq[LabelRow], Summary) = {
    val rows = scala.collection.mutable.ArrayBuffer.from(existing)
    var added, flipped, unchanged = 0
    answers.flatMap(rowsOf(_, identity)).foreach { row =>
      rows.indexWhere(_.sameJudgement(row)) match {
        case -1 => rows += row; added += 1
        case i if rows(i).verdict == row.verdict => unchanged += 1
        case i =>
          val was = rows(i)
          rows(i) = was.copy(verdict = row.verdict, note = s"${row.note} (was: ${was.verdict}: ${was.note})")
          flipped += 1
      }
    }
    val unexportable = answers.filter(a => a.verdict != ReviewVerdict.Unsure && rowsOf(a, identity).isEmpty)
      .map(a => s"${a.country} ${a.title}: ${a.verdict.label} with no film to label")
    val warnings = answers.flatMap(a => a.warnings.map(w => s"${a.country} ${a.title}: $w"))
    (rows.toSeq, Summary(added, flipped, unchanged, unexportable, warnings))
  }

  /** `existing` without the rows of `withdrawn` answers, but those a `standing` answer gives too. Only a row the review
   *  page wrote with that verdict is touched: one it flipped becomes what it was, one it added goes. */
  def takeBack(existing: Seq[LabelRow], withdrawn: Seq[ReviewAnswer], standing: Seq[ReviewAnswer],
               identity: FilmIdentity = FilmIdentity.Unlinked): (Seq[LabelRow], Int) = {
    val kept  = standing.flatMap(rowsOf(_, identity))
    val stale = withdrawn.flatMap(rowsOf(_, identity))
      .filterNot(row => kept.exists(k => k.sameJudgement(row) && k.verdict == row.verdict))
    stale.foldLeft((existing, 0)) { case ((rows, taken), row) =>
      rows.indexWhere(r => r.sameJudgement(row) && r.verdict == row.verdict && r.note.startsWith(ReviewNote)) match {
        case -1 => (rows, taken)
        case i  => (wasRow(rows(i)).fold(rows.patch(i, Nil, 1))(rows.updated(i, _)), taken + 1)
      }
    }
  }

  private val ReviewNote = "review page: "
  private val Was        = """^review page: .*? \(was: (right|wrong): (.*)\)$""".r

  /** The row a flip replaced, from the "was:" it kept. */
  private def wasRow(row: LabelRow): Option[LabelRow] = row.note match {
    case Was(verdict, note) => Some(row.copy(verdict = verdict, note = note))
    case _                  => None
  }

  /** Bring the file at `path` up to `history`: take back the rows of the answers that stood at the last export and no
   *  longer do, merge in the current answers given since, write it back, and mark them used. */
  def exportTo(path: Path, history: Seq[ReviewAnswer], identity: FilmIdentity = FilmIdentity.Unlinked): Summary = {
    val used          = LabelsTsv.usedUntil(path)
    val current       = ReviewAnswers.current(history)
    val stood         = used.fold(Seq.empty[ReviewAnswer])(u => ReviewAnswers.current(history.filterNot(_.at.isAfter(u))))
    val (fresh, old)  = current.partition(a => used.forall(a.at.isAfter))
    val (kept, taken) = takeBack(LabelsTsv.read(path), stood.filterNot(s => current.exists(_ eq s)), current, identity)
    val (rows, summary) = merge(kept, fresh, identity)
    LabelsTsv.write(path, rows)
    history.map(_.at).maxOption.foreach(LabelsTsv.markUsed(path, _))
    summary.copy(alreadyUsed = old.size, withdrawn = taken)
  }
}
