package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
import services.movies.StoredMovieRecord

/**
 * A periodic census of the live `movies` corpus, exposed as Prometheus gauges on
 * the SAME registry/endpoint as [[WorkerTaskMetrics]] (`/metrics`, scraped by the
 * fleet's Prometheus on monitoring-1, charted by the Grafana beside it).
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
 * Counted off the SHARED [[WorkerCorpusScan]] pass (default every 5 min), decoupled
 * from the scrape rate: this census reads ratings/ids only and simply ignores the
 * showtimes the pass stitches for its sibling collectors, so it costs no reads of its
 * own. Between samples the gauge just re-reads its cached value. Mirrors the web app's
 * [[controllers.WebMovieMetrics]] sample-and-cache shape.
 */
class WorkerCorpusMetrics(corpus: Gauge, countryCode: String) extends CorpusMetricsCollector {
  import WorkerCorpusMetrics._

  // Materialize this country's every series at 0 so it exists from boot — no Grafana gaps.
  Subset.all.foreach(s => corpus.labelValues(countryCode, s).set(0.0))

  def startSample(): CorpusRowSampler = new CorpusRowSampler {
    private var counts = CorpusCounts.empty

    def accept(row: StoredMovieRecord): Unit = counts = counts.add(row.record)

    /** Publishes ONLY a complete census. A partial scan's counts are not a smaller
     *  corpus, they are fewer rows read — and published as a gauge the two are
     *  indistinguishable. On 2026-07-27 the `Missing field: sourceData` decode bug failed
     *  every batch, so this published `total=0` for ~50 minutes while the corpus sat
     *  intact at 943 films; the panel read as "Poland's corpus is gone". Skipping leaves
     *  the last good value in place and [[WorkerCorpusScan]] counts the miss, so a
     *  census that is genuinely stuck is still visible — as a stuck census, which is what
     *  it is, rather than as an imaginary collapse. */
    def publish(scanComplete: Boolean): Unit =
      if (scanComplete)
        counts.bySubset.foreach { case (subset, value) => corpus.labelValues(countryCode, subset).set(value.toDouble) }
  }
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
      .help("Distinct movie records in the live movies collection, by country and subset: total population, those with any rating, with a resolved tmdb/imdb id, the per-source rating populations (imdb/rt/mc/fw), and those resolved to a film their own cinemas contradict (misresolved).")
      .labelNames("country", "subset")
      .register(registry)

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
    /** Resolved to a film this row's own cinemas contradict — see
     *  [[services.movies.CinemaCorroboration]]. Five such rows sat in prod
     *  undetected until a hand-written scan found them; this is so nobody has to
     *  go looking again. Expected to sit at or near zero. */
    val Misresolved   = "misresolved"
    val all: Seq[String] =
      Seq(Total, WithAnyRating, WithTmdbId, WithImdbId, ImdbRating, RtRating, McRating, FwRating, Misresolved)
  }

  /** Pure tally of a corpus, accumulated one record at a time so the worker's
   *  paged scan never holds the whole collection on the heap. */
  case class CorpusCounts(
    total: Int, withAnyRating: Int, withTmdbId: Int, withImdbId: Int,
    imdbRating: Int, rtRating: Int, mcRating: Int, fwRating: Int, misresolved: Int
  ) {
    def add(r: MovieRecord): CorpusCounts = CorpusCounts(
      total         = total + 1,
      withAnyRating = withAnyRating + bool(hasAnyRating(r)),
      withTmdbId    = withTmdbId + bool(r.tmdbId.isDefined),
      withImdbId    = withImdbId + bool(r.imdbId.isDefined),
      imdbRating    = imdbRating + bool(r.imdbRating.isDefined),
      rtRating      = rtRating + bool(r.rottenTomatoes.isDefined),
      mcRating      = mcRating + bool(r.metascore.isDefined),
      fwRating      = fwRating + bool(r.filmwebRating.isDefined),
      misresolved   = misresolved + bool(services.movies.CinemaCorroboration.contradicts(r))
    )

    /** Pair each subset label with its count, in `Subset.all` order. */
    def bySubset: Seq[(String, Int)] = Seq(
      Subset.Total -> total, Subset.WithAnyRating -> withAnyRating,
      Subset.WithTmdbId -> withTmdbId, Subset.WithImdbId -> withImdbId,
      Subset.ImdbRating -> imdbRating, Subset.RtRating -> rtRating,
      Subset.McRating -> mcRating, Subset.FwRating -> fwRating,
      Subset.Misresolved -> misresolved
    )
  }

  object CorpusCounts {
    val empty: CorpusCounts = CorpusCounts(0, 0, 0, 0, 0, 0, 0, 0, 0)
    def from(records: IterableOnce[MovieRecord]): CorpusCounts =
      records.iterator.foldLeft(empty)((acc, r) => acc.add(r))
  }

  /** A row counts toward `with_any_rating` if any one of the four sources rated it. */
  def hasAnyRating(r: MovieRecord): Boolean =
    r.imdbRating.isDefined || r.filmwebRating.isDefined || r.rottenTomatoes.isDefined || r.metascore.isDefined

  private def bool(b: Boolean): Int = if (b) 1 else 0
}
