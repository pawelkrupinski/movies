package services.metrics

import play.api.Logging
import services.movies.{MovieRepository, StoredMovieRecord}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * The ONE periodic corpus scan behind every `movies`-census gauge the worker exposes
 * ([[WorkerCorpusMetrics]], [[WorkerSourceFilmsMetrics]], [[WorkerShowtimesMetrics]]).
 *
 * Each of those three used to own its own 5-minute timer AND its own full-corpus scan
 * of the very same data. Measured on prod (2026-07-18), for Poland alone — 788 movie
 * rows, 6,170 `screenings` docs — that was (3 × 788) + (2 × 6,170) = 14,704 documents
 * read every 5 minutes, ~4.2M/day, and it runs PER COUNTRY (pl, uk, de; UK is the
 * largest). One stitched scan serves all three: the films/showtimes collectors need
 * the stitched showtimes, and the corpus census simply ignores them.
 *
 * Failure semantics are passed through untouched: `foreachRecord` reports `false` when
 * a batch read failed mid-scan, and each collector decides for itself what to do with a
 * partial corpus (today all three publish regardless — exactly what they did when each
 * ignored the boolean on its own scan).
 */
class WorkerCorpusScan(
  repository:     MovieRepository,
  collectors:     Seq[CorpusMetricsCollector],
  sampleInterval: FiniteDuration = WorkerCorpusScan.DefaultSampleInterval
) extends Logging {

  private val scheduler = DaemonExecutors.scheduler("worker-corpus-scan")

  /** Scan the corpus ONCE, fan every row out to all collectors, then let each publish
   *  its gauges. Read-only and keyset-paged, so it adds no write load and never holds
   *  the whole corpus on the heap. */
  def sample(): Unit = {
    val samplers = collectors.map(_.startSample())
    val complete = repository.foreachRecord(row => samplers.foreach(_.accept(row)))
    samplers.foreach(_.publish(complete))
  }

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"worker-corpus-scan initial sample failed: ${e.getMessage}") }
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"worker-corpus-scan sample tick failed: ${e.getMessage}") },
      sampleInterval.toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object WorkerCorpusScan {
  /** Once every 5 minutes — the corpus changes on the order of a scrape cadence, far
   *  slower than the seconds-apart Fly `/metrics` scrape, so a more frequent re-scan
   *  would be wasted reads. */
  val DefaultSampleInterval: FiniteDuration = 5.minutes
}

/** A gauge family that is populated by censusing the whole `movies` corpus. It
 *  contributes an accumulator to each pass of [[WorkerCorpusScan]] instead of scanning
 *  on its own, so adding a fourth census costs zero extra reads. */
trait CorpusMetricsCollector {
  /** A fresh accumulator for ONE scan pass. Per-pass (not per-collector) state, so a
   *  tick can never publish counts blended with the previous tick's. */
  def startSample(): CorpusRowSampler
}

/** One collector's accumulator over a single corpus pass. */
trait CorpusRowSampler {

  /** Fold one corpus row in. Called once per row, in scan order. */
  def accept(row: StoredMovieRecord): Unit

  /** Write the accumulated counts onto the gauges. `scanComplete` is `false` when the
   *  scan stopped early on a failed batch read, i.e. the rows seen are NOT the whole
   *  corpus — a collector that must not publish a partial census gates on it. */
  def publish(scanComplete: Boolean): Unit
}
