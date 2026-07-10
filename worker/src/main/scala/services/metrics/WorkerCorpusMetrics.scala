package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
import play.api.Logging
import services.movies.MovieRepository
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * A periodic census of the live `movies` corpus, exposed as Prometheus gauges on
 * the SAME registry/endpoint as [[WorkerTaskMetrics]] (`/metrics`, scraped by
 * Fly's managed Prometheus, charted by the self-hosted Grafana, `fly/grafana`).
 * Where WorkerTaskMetrics counts the task *pipeline*, this counts the corpus
 * *contents*: how much of it has been resolved and rated.
 *
 * One labelled gauge — `kinowo_worker_corpus_movies{subset=…}` — carries every
 * population so a single Grafana panel charts all eight series:
 *   - `total`            distinct movie records
 *   - `with_any_rating`  at least one of imdb / fw / rt / mc
 *   - `with_tmdb_id`     resolved a TMDB id
 *   - `with_imdb_id`     resolved an IMDb id
 *   - `imdb_rating` / `rt_rating` / `mc_rating` / `fw_rating`  per-source rating coverage
 *
 * Sampled on its own scheduler (default every 5 min), decoupled from the scrape
 * rate. The scan pages the cursor read-only (`foreachRecord` — never materialising
 * the ~13 MB corpus on the heap), so it adds no write load; between samples the
 * gauge just re-reads its cached value. Mirrors the web app's
 * [[controllers.WebMovieMetrics]] sample-and-cache shape.
 */
class WorkerCorpusMetrics(
  repository:     MovieRepository,
  corpus:         Gauge,
  countryCode:    String,
  sampleInterval: FiniteDuration = WorkerCorpusMetrics.DefaultSampleInterval
) extends Logging {
  import WorkerCorpusMetrics._

  // Materialize this country's every series at 0 so it exists from boot — no Grafana gaps.
  Subset.all.foreach(s => corpus.labelValues(countryCode, s).set(0.0))

  private val scheduler = DaemonExecutors.scheduler("worker-corpus-metrics")

  /** Scan the corpus once and publish the census onto the gauges. Read-only,
   *  paged; bounded to once per `sampleInterval` by the scheduler regardless of
   *  how often Fly scrapes `/metrics`. */
  def sample(): Unit = {
    var counts = CorpusCounts.empty
    // Counts ratings/ids only — no showtimes — so use the cheaper scan that skips the
    // per-scan `screenings` load (this runs on a 5-min timer).
    repository.foreachRecordWithoutShowtimes(row => counts = counts.add(row.record))
    counts.bySubset.foreach { case (subset, value) => corpus.labelValues(countryCode, subset).set(value.toDouble) }
  }

  def start(): Unit = {
    Try(sample()).recover { case e => logger.warn(s"worker-corpus-metrics initial sample failed: ${e.getMessage}") }
    scheduler.scheduleAtFixedRate(
      () => Try(sample()).recover { case e => logger.warn(s"worker-corpus-metrics sample tick failed: ${e.getMessage}") },
      sampleInterval.toSeconds, sampleInterval.toSeconds, TimeUnit.SECONDS)
    ()
  }

  def stop(): Unit = scheduler.shutdown()
}

object WorkerCorpusMetrics {
  val Name = "kinowo_worker_corpus_movies"

  /** Build and register the ONE shared gauge every country's sampler writes into
   *  (leading `country` label, then `subset`). Called once when the shared worker
   *  registry is built; each per-country [[WorkerCorpusMetrics]] then samples its
   *  own slice of it. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Distinct movie records in the live movies collection, by country and subset: total population, those with any rating, with a resolved tmdb/imdb id, and the per-source rating populations (imdb/rt/mc/fw).")
      .labelNames("country", "subset")
      .register(registry)

  /** Once every 5 minutes — the corpus changes on the order of a scrape cadence,
   *  far slower than the seconds-apart Fly scrape, so a frequent re-scan would be
   *  wasted reads. */
  val DefaultSampleInterval: FiniteDuration = 5.minutes

  /** Subset label values, in the order the Grafana panel lists them. */
  object Subset {
    val Total         = "total"
    val WithAnyRating  = "with_any_rating"
    val WithTmdbId    = "with_tmdb_id"
    val WithImdbId    = "with_imdb_id"
    val ImdbRating    = "imdb_rating"
    val RtRating      = "rt_rating"
    val McRating      = "mc_rating"
    val FwRating      = "fw_rating"
    val all: Seq[String] =
      Seq(Total, WithAnyRating, WithTmdbId, WithImdbId, ImdbRating, RtRating, McRating, FwRating)
  }

  /** Pure tally of a corpus, accumulated one record at a time so the worker's
   *  paged scan never holds the whole collection on the heap. */
  case class CorpusCounts(
    total: Int, withAnyRating: Int, withTmdbId: Int, withImdbId: Int,
    imdbRating: Int, rtRating: Int, mcRating: Int, fwRating: Int
  ) {
    def add(r: MovieRecord): CorpusCounts = CorpusCounts(
      total         = total + 1,
      withAnyRating = withAnyRating + bool(hasAnyRating(r)),
      withTmdbId    = withTmdbId + bool(r.tmdbId.isDefined),
      withImdbId    = withImdbId + bool(r.imdbId.isDefined),
      imdbRating    = imdbRating + bool(r.imdbRating.isDefined),
      rtRating      = rtRating + bool(r.rottenTomatoes.isDefined),
      mcRating      = mcRating + bool(r.metascore.isDefined),
      fwRating      = fwRating + bool(r.filmwebRating.isDefined)
    )

    /** Pair each subset label with its count, in `Subset.all` order. */
    def bySubset: Seq[(String, Int)] = Seq(
      Subset.Total -> total, Subset.WithAnyRating -> withAnyRating,
      Subset.WithTmdbId -> withTmdbId, Subset.WithImdbId -> withImdbId,
      Subset.ImdbRating -> imdbRating, Subset.RtRating -> rtRating,
      Subset.McRating -> mcRating, Subset.FwRating -> fwRating
    )
  }

  object CorpusCounts {
    val empty: CorpusCounts = CorpusCounts(0, 0, 0, 0, 0, 0, 0, 0)
    def from(records: IterableOnce[MovieRecord]): CorpusCounts =
      records.iterator.foldLeft(empty)((acc, r) => acc.add(r))
  }

  /** A row counts toward `with_any_rating` if any one of the four sources rated it. */
  def hasAnyRating(r: MovieRecord): Boolean =
    r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined

  private def bool(b: Boolean): Int = if (b) 1 else 0
}
