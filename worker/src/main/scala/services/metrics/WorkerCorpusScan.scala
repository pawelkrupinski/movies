package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import play.api.Logging
import services.movies.{MovieRepository, StoredMovieRecord}
import tools.{DaemonExecutors, Stopwatch}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * The ONE periodic corpus scan behind every `movies`-census gauge the worker exposes
 * ([[WorkerCorpusMetrics]], [[WorkerSourceFilmsMetrics]], [[WorkerShowtimesMetrics]],
 * [[WorkerSlotFanoutMetrics]]).
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
  metrics:        CorpusScanMetrics = CorpusScanMetrics.noop,
  stopwatch:      Stopwatch         = Stopwatch.System
) extends Logging {

  private val scheduler = DaemonExecutors.scheduler("worker-corpus-scan")

  /** Scan the corpus ONCE, fan every row out to all collectors, then let each publish
   *  its gauges. Read-only and keyset-paged, so it adds no write load and never holds
   *  the whole corpus on the heap.
   *
   *  An incomplete pass publishes NOTHING and is counted + logged instead. The gauges
   *  keep their last complete values, which is the honest reading: this pass learned
   *  nothing about the corpus. */
  def sample(): WorkerCorpusScan.Pass = {
    val samplers = collectors.map(c => (WorkerCorpusScan.nameOf(c), c.startSample(), stopwatch.total()))
    val pass     = stopwatch.start()
    val complete = repository.foreachRecord { stored =>
      val row = new CorpusRow(stored)
      samplers.foreach { case (_, sampler, time) => time(sampler.accept(row)) }
    }
    if (!complete) {
      metrics.recordIncompleteSample()
      logger.warn("worker-corpus-scan: corpus scan incomplete — census gauges keep their previous values " +
        "rather than publishing a partial count as if the corpus had shrunk.")
    }
    samplers.foreach { case (_, sampler, time) => time(sampler.publish(complete)) }
    val result = WorkerCorpusScan.Pass(pass.elapsed, samplers.map { case (name, _, time) => name -> time.elapsed })
    logger.info(result.summary)
    result
  }

  // The first scan on the scan's own thread, after the boot's heavy stretch — never on the boot
  // thread, where it held a US boot for 43 s (see `SampledCensus`).
  def start(): Unit = {
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"worker-corpus-scan sample tick failed: ${e.getMessage}") },
      SampledCensus.FirstSampleDelay.min(sampleInterval).toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object WorkerCorpusScan {
  /** One pass's time, and each collector's share of it (its `accept`s and its `publish`).
   *  The rest is the stitched read itself — on US 12–14 s of a 25–31 s pass (2026-09-30),
   *  with most of what remained unaccounted for until this split it by collector. */
  final case class Pass(total: FiniteDuration, byCollector: Seq[(String, FiniteDuration)]) {
    def summary: String = {
      val collectors = byCollector.sortBy { case (_, time) => -time.toNanos }
        .map { case (name, time) => s"$name ${time.toMillis}ms" }.mkString(", ")
      s"worker-corpus-scan: pass took ${total.toMillis}ms — $collectors; the rest is the stitched read."
    }
  }

  private def nameOf(collector: CorpusMetricsCollector): String =
    Option(collector.getClass.getSimpleName).filter(_.nonEmpty).getOrElse(collector.getClass.getName).stripSuffix("$")

  /** Once every 15 minutes — the corpus changes on the order of a scrape cadence (hours; 14 on
   *  the US), and every rule these gauges feed holds 30 minutes or more over a gauge that keeps
   *  its value between samples. At 5 minutes the US pass was 29% of the worker's CPU and a third
   *  of its allocation (2026-09-30). */
  val DefaultSampleInterval: FiniteDuration = 15.minutes

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
  def accept(row: CorpusRow): Unit

  /** Write the accumulated counts onto the gauges. `scanComplete` is `false` when the
   *  scan stopped early on a failed batch read, i.e. the rows seen are NOT the whole
   *  corpus — a collector that must not publish a partial census gates on it. */
  def publish(scanComplete: Boolean): Unit
}

/** One corpus row as the scan hands it to every collector: the stored row, and its venues as the
 *  read model partitions them — each card's venues, their city and showtimes, the row itself not
 *  built — derived at most ONCE per row however many collectors ask, and only if one does. Two do
 *  (films served, upcoming showtimes), and each used to project every row itself: on the US worker
 *  a pass was 29% of the worker's CPU and 9 GB of allocation (2026-09-30). The counts need a venue's
 *  city and showtime SET only, so the row's ordering, listing keys and link are never built.
 *  `None` for a row not ready to project, or one that fails to — the rows the projector skips. */
final class CorpusRow(val stored: StoredMovieRecord) {
  private var partitioned: Option[(services.movies.TitleNormalizer, Option[Seq[Seq[services.readmodel.ReadModelProjection.VenueScreening]]])] = None

  /** The row's cards, each as its venues, as `normalizer` folds its titles. Shared only with a
   *  collector asking through the SAME normalizer — the wiring hands every collector of a country its one. */
  def venues(normalizer: services.movies.TitleNormalizer): Option[Seq[Seq[services.readmodel.ReadModelProjection.VenueScreening]]] =
    partitioned match {
      case Some((by, cards)) if by eq normalizer => cards
      case _ =>
        val cards = Option.when(stored.record.readyToProject)(
          scala.util.Try(services.readmodel.ReadModelProjection.partition(stored, normalizer).venuesAll).toOption).flatten
        partitioned = Some((normalizer, cards))
        cards
    }
}
