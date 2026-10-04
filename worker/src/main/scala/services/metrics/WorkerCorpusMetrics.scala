package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * A census of the live `movies` corpus, exposed as Prometheus gauges on
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
 *   - `misresolved`      resolved to a film its own cinemas contradict
 *   - `unresolved_with_showtimes`  NOT resolved, yet still screening — invisible on the site
 *
 * Counted by [[CorpusCensus]], film by film as the worker's cache changes: most subsets read ids and ratings only;
 * `unresolved_with_showtimes` reads each venue slot's showtime starts, against the clock at each tick.
 */
object WorkerCorpusMetrics {
  val Name = "kinowo_worker_corpus_movies"

  /** Build and register the ONE shared gauge every country's census writes into
   *  (leading `country` label, then `subset`). Called once when the shared worker
   *  registry is built; each country's [[CorpusCensus]] then writes its own slice of it. */
  def gauge(registry: PrometheusRegistry): Gauge =
    Gauge.builder()
      .name(Name)
      .help("Distinct movie records in the live movies collection, by country and subset: total population, those with any rating, with a resolved tmdb/imdb id, the per-source rating populations (imdb/rt/mc/fw), those resolved to a film their own cinemas contradict (misresolved), and those NOT resolved that are still screening (unresolved_with_showtimes) — the population the read model prunes, i.e. films invisible on the site while their venues still sell tickets.")
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
    /** NOT resolved (fails [[models.MovieRecord.readyToProject]]) and STILL SCREENING —
     *  the row has at least one upcoming showtime.
     *
     *  This is the outcome half of `misresolved`, and the one that used to be silent.
     *  The mis-resolution sweep rejects a wrong film and then may find no right one;
     *  the row is left unresolved, the projector prunes its card, and the film goes
     *  INVISIBLE while its cinemas still list it — worse, for a row with screenings,
     *  than the wrong poster it started with. `misresolved` counts the sweep's INPUT,
     *  every other census gauge gates on `readyToProject` and so drops these rows
     *  silently, and one film is far below the `ReadModelFilmPruneBurst` threshold.
     *  Four such rows needed hand repair on 2026-09-06 before this existed. Expected
     *  to sit at or near zero; see `docs/misresolution-sweep.md`. */
    val UnresolvedWithShowtimes = "unresolved_with_showtimes"
    val all: Seq[String] =
      Seq(Total, WithAnyRating, WithTmdbId, WithImdbId, ImdbRating, RtRating, McRating, FwRating,
        Misresolved, UnresolvedWithShowtimes)
  }
}
