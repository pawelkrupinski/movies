package services.movies

import controllers.FilmSchedule
import models.Showtime

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable

/** One replay pass's whole-corpus result — the three axes an order dependency can
 *  hide in: the persisted film records, the per-slot screenings, and the rows the
 *  web would actually render. */
final case class ReplayCorpus(
  records: Seq[StoredMovieRecord],
  screenings: Map[String, Map[String, Seq[Showtime]]],
  rows: Seq[FilmSchedule]
)

/**
 * Folds each concurrent replay pass into a comparison against the first pass to
 * arrive, so the heap holds the baseline corpus and AT MOST ONE other.
 *
 * WHY A LOCK, in a suite whose whole point is to fan the passes out. The passes
 * must stay concurrent: three serial whole-corpus replays do not fit the United
 * States' six-hour job, and its scrape-and-fold alone is ~3 hours per pass. But
 * the heap goes somewhere else entirely. A pass allocates its corpus only in its
 * LAST step — the film records with their showtimes embedded, the slot map, and
 * every rendered row for every one of the country's cities — and the old shape
 * returned all `Passes` of them so the caller could diff them afterwards. That
 * put three whole corpora, and the three read models they were rendered from, in
 * heap at the same instant.
 *
 * On 2026-09-05 the US order-independence leg died there: every pass finished
 * `replayReloadReadModel` within two seconds of the others, and sixteen seconds
 * later the JVM went through four GC cycles above 98% with free heap falling
 * 0.27 → 0.01 GB of 8 GB and exited on `-XX:+ExitOnOutOfMemoryError`, before
 * ScalaTest had reported one assertion. The run four hours earlier PASSED on an
 * identical corpus (2,303 films, 113,708 slots, 33,630 rendered rows) having
 * peaked at 5.9 GB of the same ceiling — so the leg had been running with no
 * margin rather than regressing, and a corpus that grows a little every day was
 * always going to spend it.
 *
 * Serialising only the TAIL keeps the fan-out where it pays and costs a reload
 * plus a diff per pass — seconds against hours — while never holding more than
 * two corpora. `materialise` runs INSIDE the lock for exactly that reason: it is
 * the allocation, so running it outside would put every pass's corpus in heap at
 * once again and leave only the cheap part serialised.
 *
 * The report stays FULL on both sides. Pass `i`'s real records and rows are in
 * heap while it is diffed, so `CorpusDiff` prints what actually differs on each
 * side — the alternative that also fits the heap, reducing later passes to
 * digests, can only say THAT they differ, and a divergence nobody can read is a
 * whole investigation on a run that takes five hours to reproduce.
 */
final class CorpusComparison {

  private val lock = new ReentrantLock()
  @volatile private var baseline: Option[(Int, ReplayCorpus)] = None
  private val found = mutable.ListBuffer.empty[String]

  /** Materialise pass `pass`'s corpus and fold it into the comparison. The first
   *  caller in becomes the baseline and is retained; every later one is compared
   *  and dropped before the lock is released.
   *
   *  Which pass gets to be the baseline is whichever finishes first, not pass 0.
   *  The claim is that all `Passes` are identical, and that is symmetric — but the
   *  clue has to name the pair it actually compared, so the labels carry the real
   *  pass numbers and the caller quotes the seeds beside them. */
  def submit(pass: Int)(materialise: () => ReplayCorpus): Unit = {
    lock.lock()
    try {
      val corpus = materialise()
      baseline match {
        case None                   => baseline = Some(pass -> corpus)
        case Some((base, baseCorpus)) => found ++= diff(base, baseCorpus, pass, corpus)
      }
    } finally lock.unlock()
  }

  /** The corpus every other pass was compared against — the caller's handle for
   *  what the run actually produced (that it is non-empty, and how big it is). */
  def reference: ReplayCorpus =
    baseline.map(_._2).getOrElse(throw new IllegalStateException("no replay pass was submitted"))

  /** One entry per axis per diverging pass, ready to go straight into a clue. */
  def divergences: List[String] = {
    lock.lock()
    try found.toList finally lock.unlock()
  }

  private def diff(base: Int, baseCorpus: ReplayCorpus, pass: Int, corpus: ReplayCorpus): Seq[String] = {
    val a = s"pass$base"
    val b = s"pass$pass"
    val axes = mutable.ListBuffer.empty[String]
    if (corpus.records != baseCorpus.records)
      axes += s"FILMS differ on $b:\n${CorpusDiff.records(baseCorpus.records, corpus.records, a, b)}"
    if (corpus.screenings != baseCorpus.screenings)
      axes += s"SCREENINGS differ on $b:\n${CorpusDiff.slots(baseCorpus.screenings, corpus.screenings, a, b)}"
    if (corpus.rows != baseCorpus.rows)
      axes += s"RENDERED ROWS differ on $b (${baseCorpus.rows.size} vs ${corpus.rows.size}):\n" +
              CorpusDiff.rows(baseCorpus.rows, corpus.rows, a, b)
    axes.toList
  }
}
