package services.sharecards

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry

/**
 * The share-card pipeline's `/metrics` series, registered ONCE on the shared worker registry with
 * a leading `country` label; each country's wiring holds a [[ShareCardMetrics]] bound to its code.
 *
 * DIRECTORY GAUGES ARE READ FROM THE DIRECTORY, so every replica of a country publishes the same
 * numbers for the same directory: aggregate them with `max by (country, …)`, never `sum` (which
 * would count one directory once per replica). The counters are per process: `sum` them.
 *
 *  - `kinowo_worker_share_cards_bytes{kind}` / `_files{kind}` — everything on disk: cards, the
 *    poster cache and the card bases (`kind="card"|"poster"|"base"`), temp files included. One
 *    budget covers the sum.
 *  - `kinowo_worker_share_cards_current_bytes{kind}` — the part no pruner may delete: the cards
 *    `web_movies` points at and the posters of films on screen.
 *  - `kinowo_worker_share_cards_budget_bytes` — `KINOWO_SHARE_CARD_BUDGET_MB`.
 *  - `kinowo_worker_share_cards_coverage_ratio` — films on screen whose card for their current
 *    inputs exists, over films on screen. What says a backfill is done.
 *  - `kinowo_worker_share_cards_render_total{outcome,reason}` — renders by result and by why.
 *  - `kinowo_worker_share_cards_render_path_total{path}` — cards drawn on a cached base or not.
 *  - `kinowo_worker_share_cards_pruned_total{kind,reason}` — deletions by the prune and the budget.
 *  - `kinowo_worker_share_cards_poster_cache_total{result}` — poster cache hits and misses.
 *  - `kinowo_worker_share_cards_poster_fetch_total{result}` — a miss's download + shrink, ok or failed.
 *  - `kinowo_worker_share_cards_rescrape_total{outcome}` — Facebook re-scrape requests.
 */
object ShareCardMetrics {
  object Outcome {
    val Rendered = "rendered"; val Existing = "existing"; val Failed = "failed"
    val all: Seq[String] = Seq(Rendered, Existing, Failed)
  }
  object PruneReason {
    val Retired = "retired"; val Superseded = "superseded"; val Unreferenced = "unreferenced"
    val Budget = "budget"; val Temp = "temp"
    val all: Seq[String] = Seq(Retired, Superseded, Unreferenced, Budget, Temp)
  }
  /** How a rendered card was drawn — see [[ShareCardService.render]]. */
  object Path {
    val BaseHit = "base_hit"; val BaseRebuild = "base_rebuild"; val Full = "full"
    val all: Seq[String] = Seq(BaseHit, BaseRebuild, Full)
  }
  object RescrapeOutcome {
    val Sent = "sent"; val Failed = "failed"; val Disabled = "disabled"
    val all: Seq[String] = Seq(Sent, Failed, Disabled)
  }

  final class Series(countryCodes: Seq[String], registry: PrometheusRegistry) {
    private[ShareCardMetrics] val bytes = Gauge.builder().name("kinowo_worker_share_cards_bytes")
      .help("Bytes in the country's share-card directory by kind (card, poster, base), temp files included. Read from the directory: max by country across replicas.")
      .labelNames("country", "kind").register(registry)
    private[ShareCardMetrics] val files = Gauge.builder().name("kinowo_worker_share_cards_files")
      .help("Files in the country's share-card directory by kind (card, poster, base).")
      .labelNames("country", "kind").register(registry)
    private[ShareCardMetrics] val currentBytes = Gauge.builder().name("kinowo_worker_share_cards_current_bytes")
      .help("Bytes no pruner may delete, by kind: cards web_movies points at, their bases, and posters of films on screen.")
      .labelNames("country", "kind").register(registry)
    private[ShareCardMetrics] val budget = Gauge.builder().name("kinowo_worker_share_cards_budget_bytes")
      .help("The country's share-card disk budget (KINOWO_SHARE_CARD_BUDGET_MB), cards, posters and bases together.")
      .labelNames("country").register(registry)
    private[ShareCardMetrics] val coverage = Gauge.builder().name("kinowo_worker_share_cards_coverage_ratio")
      .help("Films on screen whose share card for their current inputs exists, over films on screen.")
      .labelNames("country").register(registry)
    private[ShareCardMetrics] val renders = Counter.builder().name("kinowo_worker_share_cards_render")
      .help("Share-card render attempts by outcome (rendered, existing, failed) and reason (new_film, backfill, or the input part that moved).")
      .labelNames("country", "outcome", "reason").register(registry)
    private[ShareCardMetrics] val paths = Counter.builder().name("kinowo_worker_share_cards_render_path")
      .help("Cards drawn, by how: base_hit (a cached base plus the badges — a ratings change), base_rebuild (the base redrawn from the cached poster), full (a posterless card, drawn whole).")
      .labelNames("country", "path").register(registry)
    private[ShareCardMetrics] val pruned = Counter.builder().name("kinowo_worker_share_cards_pruned")
      .help("Share-card files deleted, by kind and reason (retired, superseded, unreferenced, budget, temp).")
      .labelNames("country", "kind", "reason").register(registry)
    private[ShareCardMetrics] val posterCache = Counter.builder().name("kinowo_worker_share_cards_poster_cache")
      .help("Poster-cache lookups for a render: hit (a cached slot used) or miss (fetched).")
      .labelNames("country", "result").register(registry)
    private[ShareCardMetrics] val posterFetch = Counter.builder().name("kinowo_worker_share_cards_poster_fetch")
      .help("Poster fetch + decode on a cache miss: ok, or failed (every candidate unreachable, oversized or undecodable).")
      .labelNames("country", "result").register(registry)
    private[ShareCardMetrics] val rescrapes = Counter.builder().name("kinowo_worker_share_cards_rescrape")
      .help("Facebook re-scrape requests for film pages published before their card: sent, failed, or disabled (no app credentials).")
      .labelNames("country", "outcome").register(registry)

    for (c <- countryCodes) {
      for (k <- ShareCardStore.Kind.all) { bytes.labelValues(c, k); files.labelValues(c, k); currentBytes.labelValues(c, k) }
      budget.labelValues(c)
      for (o <- Outcome.all; r <- ShareCardReason.all) renders.labelValues(c, o, r)
      Path.all.foreach(paths.labelValues(c, _))
      for (k <- ShareCardStore.Kind.all; r <- PruneReason.all) pruned.labelValues(c, k, r)
      for (r <- Seq("hit", "miss")) posterCache.labelValues(c, r)
      for (r <- Seq("ok", "failed")) posterFetch.labelValues(c, r)
      RescrapeOutcome.all.foreach(rescrapes.labelValues(c, _))
    }

    def forCountry(code: String): ShareCardMetrics = new ShareCardMetrics(code, Some(this))

    /** Test seam: the coverage gauge's current value. */
    private[sharecards] def coverageFor(country: String): Double = coverage.labelValues(country).get()
    /** Test seam: how many cards were drawn by `path`. */
    private[sharecards] def pathCount(country: String, path: String): Double = paths.labelValues(country, path).get()
  }

  /** Records nothing — for specs that don't assert on metrics. */
  def noop: ShareCardMetrics = new ShareCardMetrics("xx", None)
}

/** One country's view of [[ShareCardMetrics.Series]]. */
final class ShareCardMetrics private[sharecards] (country: String, series: Option[ShareCardMetrics.Series]) {
  def render(outcome: String, reasons: Seq[String]): Unit =
    series.foreach(s => reasons.foreach(reason => s.renders.labelValues(country, outcome, reason).inc()))
  def renderPath(path: String): Unit = series.foreach(_.paths.labelValues(country, path).inc())
  def pruned(kind: String, reason: String): Unit = series.foreach(_.pruned.labelValues(country, kind, reason).inc())
  def posterCache(hit: Boolean): Unit = series.foreach(_.posterCache.labelValues(country, if (hit) "hit" else "miss").inc())
  def posterFetch(ok: Boolean): Unit  = series.foreach(_.posterFetch.labelValues(country, if (ok) "ok" else "failed").inc())
  def rescrape(outcome: String): Unit = series.foreach(_.rescrapes.labelValues(country, outcome).inc())
  def coverage(ratio: Double): Unit = series.foreach(_.coverage.labelValues(country).set(ratio))

  /** The directory's state as the janitor last measured it. */
  def directory(bytesByKind: Map[String, Long], filesByKind: Map[String, Long], currentByKind: Map[String, Long], budgetBytes: Long): Unit =
    series.foreach { s =>
      ShareCardStore.Kind.all.foreach { kind =>
        s.bytes.labelValues(country, kind).set(bytesByKind.getOrElse(kind, 0L).toDouble)
        s.files.labelValues(country, kind).set(filesByKind.getOrElse(kind, 0L).toDouble)
        s.currentBytes.labelValues(country, kind).set(currentByKind.getOrElse(kind, 0L).toDouble)
      }
      s.budget.labelValues(country).set(budgetBytes.toDouble)
    }
}
