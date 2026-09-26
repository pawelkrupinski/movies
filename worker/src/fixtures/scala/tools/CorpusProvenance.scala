package tools

import services.scrapes.ArchivedScrape

import java.nio.file.{Files, Path}

/** One `Record scrape fixtures` run whose corpus a convergence leg replayed. */
final case class CorpusRecording(runId: String, recordedAt: String) {
  override def toString: String = s"recording run $runId ($recordedAt)"
}

/**
 * What changed between two recorded corpora — the evidence that tells a DATA-driven
 * convergence failure from a code regression.
 *
 * The leg replays the NEWEST recorded corpus, so a red leg can be new data exactly as
 * easily as new code. Country-convergence run 35948292875 is the case that paid for
 * this: its UK leg went red on a commit whose code was fine, because the day's corpus
 * had lost every Mockingjay Part 1 listing that carried an original title, and the
 * bisect went looking through commits first.
 *
 * Cheap on purpose: venue ids, per-title showtime counts and a handful of field
 * coverages — nothing a whole corpus can't answer in one pass.
 */
final case class CorpusDiff(
  venuesAdded:   Seq[String],
  venuesRemoved: Seq[String],
  /** (title, showtimes before, showtimes after), largest absolute change first. */
  filmDeltas:    Seq[(String, Int, Int)],
  coverage:      Seq[CorpusDiff.FieldCoverage]
) {
  def identical: Boolean =
    venuesAdded.isEmpty && venuesRemoved.isEmpty && filmDeltas.isEmpty && coverage.forall(c => c.before == c.after)

  /** The fields whose coverage FELL — a field that disappears is the likeliest cause. */
  def coverageDrops: Seq[CorpusDiff.FieldCoverage] = coverage.filter(c => c.after.share < c.before.share)

  /** One line, for a failure message. */
  def summary: String =
    if (identical) "identical content"
    else {
      val drops = coverageDrops.map(c => s"${c.field} ${c.before.percent} → ${c.after.percent}")
      (Seq(s"+${venuesAdded.size}/−${venuesRemoved.size} venues",
           s"${filmDeltas.size} film(s) with a showtime change") ++ drops).mkString(", ")
    }

  def markdown: String = {
    def names(ns: Seq[String]) = if (ns.isEmpty) "—" else ns.take(CorpusDiff.ListedVenues).mkString(", ") +
      (if (ns.size > CorpusDiff.ListedVenues) s" … (+${ns.size - CorpusDiff.ListedVenues})" else "")
    val films = filmDeltas.map { case (t, b, a) => f"| $t | $b | $a | ${a - b}%+d |" }
    val fields = coverage.map(c => s"| ${c.field} | ${c.before.percent} | ${c.after.percent} |")
    (Seq(s"- venues added (${venuesAdded.size}): ${names(venuesAdded)}",
         s"- venues removed (${venuesRemoved.size}): ${names(venuesRemoved)}",
         "", "| film | showtimes before | after | Δ |", "|---|---|---|---|") ++ films ++
     Seq("", "| listing field | coverage before | after |", "|---|---|---|") ++ fields).mkString("\n")
  }
}

object CorpusDiff {

  /** How many listings carried a field, out of how many listings there were. */
  final case class Share(present: Int, of: Int) {
    def share: Double      = if (of == 0) 0.0 else present.toDouble / of
    def percent: String    = f"${share * 100}%.1f%% ($present/$of)"
  }
  final case class FieldCoverage(field: String, before: Share, after: Share)

  val TopFilms     = 10
  val ListedVenues = 15

  /** The listing fields the pipeline's identity and enrichment decisions read — the ones
   *  whose disappearance changes which films the settle sees. */
  private val Fields: Seq[(String, models.CinemaMovie => Boolean)] = Seq(
    "originalTitle"  -> (_.movie.originalTitle.exists(_.trim.nonEmpty)),
    "runtimeMinutes" -> (_.movie.runtimeMinutes.isDefined),
    "releaseYear"    -> (_.movie.releaseYear.isDefined),
    "director"       -> (_.director.nonEmpty))

  def of(before: Seq[ArchivedScrape], after: Seq[ArchivedScrape]): CorpusDiff = {
    val (venuesBefore, venuesAfter) = (before.map(_.cinema.displayName).toSet, after.map(_.cinema.displayName).toSet)
    def showtimes(rows: Seq[ArchivedScrape]): Map[String, Int] =
      rows.flatMap(_.films).groupMapReduce(_.movie.title)(_.showtimes.size)(_ + _)
    val (filmsBefore, filmsAfter) = (showtimes(before), showtimes(after))
    val deltas = (filmsBefore.keySet ++ filmsAfter.keySet).toSeq
      .map(t => (t, filmsBefore.getOrElse(t, 0), filmsAfter.getOrElse(t, 0)))
      .filter { case (_, b, a) => b != a }
      .sortBy { case (t, b, a) => (-math.abs(a - b), t) }
      .take(TopFilms)
    def share(rows: Seq[ArchivedScrape], has: models.CinemaMovie => Boolean): Share = {
      val listings = rows.flatMap(_.films)
      Share(listings.count(has), listings.size)
    }
    CorpusDiff(
      venuesAdded   = (venuesAfter -- venuesBefore).toSeq.sorted,
      venuesRemoved = (venuesBefore -- venuesAfter).toSeq.sorted,
      filmDeltas    = deltas,
      coverage      = Fields.map { case (field, has) => FieldCoverage(field, share(before, has), share(after, has)) })
  }
}

/**
 * Which corpus a leg replayed, which one its country's last GREEN leg replayed, and —
 * when they differ and the green one is still downloadable — what changed between them.
 * `convergence-setup` exports the environment this reads; a local run has none of it
 * and says so rather than guessing.
 */
final case class CorpusProvenance(replayed: Option[CorpusRecording], lastGreen: Option[CorpusRecording],
                                  diff: Option[CorpusDiff]) {

  /** The sentence a failure message carries. */
  def verdict: String = (replayed, lastGreen) match {
    case (None, _) =>
      "Corpus provenance unknown (no recording run recorded — a local run?)."
    case (Some(r), None) =>
      s"Replayed $r; no last-green corpus on record, so a data change cannot be ruled out."
    case (Some(r), Some(g)) if r.runId == g.runId =>
      s"Corpus UNCHANGED since the last green leg ($r) — this failure is the code, not the data."
    case (Some(r), Some(g)) => diff match {
      case Some(d) if d.identical =>
        s"Corpus re-recorded since the last green leg ($g → $r) but its content is identical — this failure is the code."
      case Some(d) =>
        s"Corpus CHANGED since the last green leg ($g → $r): ${d.summary}. Check the data before bisecting commits."
      case None =>
        s"Corpus CHANGED since the last green leg ($g → $r); that corpus is no longer downloadable, so no diff."
    }
  }

  def markdown(title: String): String =
    (Seq(s"### Corpus provenance — $title", "", verdict) ++ diff.filterNot(_.identical).map("\n" + _.markdown)).mkString("\n")
}

object CorpusProvenance {

  val RunEnv             = "KINOWO_CONVERGENCE_CORPUS_RUN"
  val RecordedAtEnv      = "KINOWO_CONVERGENCE_CORPUS_RECORDED_AT"
  val GreenRunEnv        = "KINOWO_CONVERGENCE_GREEN_CORPUS_RUN"
  val GreenRecordedAtEnv = "KINOWO_CONVERGENCE_GREEN_CORPUS_RECORDED_AT"
  /** The directory the last green leg's corpus was unpacked into — set only when it
   *  differs from the replayed one and could still be downloaded. */
  val GreenDirEnv        = "KINOWO_CONVERGENCE_GREEN_CORPUS_DIR"

  def of(corpusKey: String, replayedRows: Seq[ArchivedScrape], configuration: settings.ProcessConfiguration): CorpusProvenance = {
    val replayed  = configuration.corpusRunId.map(run =>
      CorpusRecording(run.value, configuration.corpusRecordedAt.fold("?")(_.value)))
    val lastGreen = configuration.greenCorpusRunId.map(run =>
      CorpusRecording(run.value, configuration.greenCorpusRecordedAt.fold("?")(_.value)))
    val greenFile: Option[Path] = configuration.greenCorpusDirectory
      .map(_.value.resolve(CorpusFixture.pathFor(corpusKey).getFileName))
      .filter(Files.exists(_))
    CorpusProvenance(replayed, lastGreen, greenFile.map(f => CorpusDiff.of(CorpusFixture.readFrom(f), replayedRows)))
  }
}
