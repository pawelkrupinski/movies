package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
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
 * Failure semantics: `foreachRecord` reports `false` when a batch read failed mid-scan,
 * and every collector SKIPS its publish on that — a partial census is fewer rows read,
 * not a smaller corpus, and as a gauge value the two are indistinguishable (2026-07-27:
 * a decode bug failed every batch and the censuses published 0 while the corpus sat
 * intact). Skipping leaves the last good value, so the miss is counted here instead: a
 * census that is genuinely stuck must not hide behind gauges frozen at plausible numbers.
 */
class WorkerCorpusScan(
  repository:     MovieRepository,
  collectors:     Seq[CorpusMetricsCollector],
  sampleInterval: FiniteDuration = WorkerCorpusScan.DefaultSampleInterval,
  // Counts the passes that could not read the whole corpus. Noop for tests that only
  // care about the gauges; the worker injects the Prometheus-backed sink.
  metrics:        CorpusScanMetrics = CorpusScanMetrics.noop
) extends Logging {

  private val scheduler = DaemonExecutors.scheduler("worker-corpus-scan")

  /** Scan the corpus ONCE, fan every row out to all collectors, then let each publish
   *  its gauges. Read-only and keyset-paged, so it adds no write load and never holds
   *  the whole corpus on the heap.
   *
   *  An incomplete pass publishes NOTHING and is counted + logged instead. The gauges
   *  keep their last complete values, which is the honest reading: this pass learned
   *  nothing about the corpus. */
  def sample(): Unit = {
    val samplers = collectors.map(_.startSample())
    val complete = repository.foreachRecord(row => samplers.foreach(_.accept(row)))
    if (!complete) {
      metrics.recordIncompleteSample()
      logger.warn("worker-corpus-scan: corpus scan incomplete — census gauges keep their previous values " +
        "rather than publishing a partial count as if the corpus had shrunk.")
    }
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

  val IncompleteMetricName = "kinowo_worker_corpus_scan_incomplete_total"

  /** Register the shared counter every country's scan increments (leading `country`
   *  label, like every other worker metric family). */
  def incompleteCounter(registry: PrometheusRegistry): Counter =
    Counter.builder()
      .name(IncompleteMetricName)
      .help("Corpus census passes that could not read the whole `movies` collection, by country. " +
        "The census gauges (kinowo_worker_corpus_movies / _showtimes / _movies_served) deliberately " +
        "publish NOTHING on such a pass — they hold their last complete values — so a rising rate here " +
        "is the only sign those gauges have gone stale, and stale is what they are.")
      .labelNames("country")
      .register(registry)
}

/** Where [[WorkerCorpusScan]] reports a pass that fell short of the whole corpus.
 *  A trait so the scan stays testable without a Prometheus registry, mirroring
 *  [[services.movies.ChangeStreamMetrics]]. */
trait CorpusScanMetrics {
  def recordIncompleteSample(): Unit
}

object CorpusScanMetrics {
  val noop: CorpusScanMetrics = new CorpusScanMetrics { def recordIncompleteSample(): Unit = () }

  /** Binds one country's slice of the shared counter, materializing the series at 0 up
   *  front. Same rule the census gauges follow: a healthy country must be an explicit 0,
   *  not an absent series — otherwise the metric only appears once it has already gone
   *  wrong, and an alert on it has nothing to compare against until then. */
  def prometheus(counter: Counter, countryCode: String): CorpusScanMetrics = {
    counter.labelValues(countryCode)
    new CorpusScanMetrics {
      def recordIncompleteSample(): Unit = counter.labelValues(countryCode).inc()
    }
  }
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
