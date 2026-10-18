package services.metrics

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.movies.{ListingIntakeMetrics, RepositoryWriteMetrics}

/** The corpus write path's series: the scrape guards' verdicts and skipped writes, failed repository
 *  writes, undecodable documents, and what a cache rehydrate changed. Every series is seeded to 0 for
 *  each country at construction. */
final class CorpusWriteSeries(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val cacheRehydrateChanges = Counter.builder()
    .name("kinowo_worker_cache_rehydrate_changes")
    .help("Rows the MovieCache's periodic backstop rehydrate (full findAll reload) caught that the INCREMENTAL change stream missed, by country and kind (changed=a put whose cached value differed = a missed upsert; deleted=a key gone from Mongo the delete-apply didn't drop). After resume-token persistence + cache delete-apply this should be ~0 in steady state; a rate flat at 0 proves the 30-min rehydrate is redundant and can be retired. NOTE: the one-time BOOT hydrate counts EVERY row as changed — read the rate over steady state, not the raw counter.")
    .labelNames("country", "kind")
    .register(registry)

  private val scrapeGuardVerdicts = Counter.builder()
    .name("kinowo_worker_scrape_guard_verdicts")
    .help("The listing intake's depth and breadth guards (services.movies.ScrapeHealth) rejecting or accepting a tick, by country, guard (depth|breadth) and verdict (reject|accept). `healthy` is not counted — the overwhelming default on every tick of every cinema, and answered better by a scrape-completed counter elsewhere. Added 2026-09-13: before this, a guard stuck rejecting (or repeatedly giving up) for hours was visible only by grepping [scrape-depth]/[scrape-prune] log lines by cinema name — which is how long Kino Aurum's breadth-guard deadlock (57 accumulated slot-keys against an 11-film board, permanently below the prune-floor ratio) went unnoticed. A `reject` RATE that never falls is the signal to alert on; a sustained run of `accept`s on one axis means a scraper that has been degraded in the SAME shape for hours, not a one-off.")
    .labelNames("country", "guard", "verdict")
    .register(registry)

  private val scrapeWriteSkipped = Counter.builder()
    .name("kinowo_worker_scrape_write_skipped")
    .help("A MovieCache.putIfPresent write that did not land, by country and reason (services.movies.ListingIntakeMetrics.SkipReason): cache-miss-race is a concurrent rekey of some OTHER title invalidating this key between the read and the compute; repository-write-failed is MovieRepository.updateIfPresent answering false for a row the cache holds. Neither had a counter before 2026-09-13 — a skip this shaped throws nothing and logs nothing on its own, so a real, sustained skip and a once-off race were otherwise indistinguishable without reading screenings/movie_slots directly.")
    .labelNames("country", "reason")
    .register(registry)

  private val repositoryWriteFailed = Counter.builder()
    .name("kinowo_worker_repository_write_failed")
    .help("A MovieRepository / SlotsRepository / ScreeningsRepository write that THREW, by country, collection (movies|movie_slots|screenings), op (upsert, replaceFilm, upsertSlot, updateIfPresent, delete, ...) and exception (the class's simple name). ZERO IS THE HEALTHY READING. Added 2026-09-24: a codec bug failed 34 upserts over ~6h and every one was logged at WARN and returned Unit, while the cache kept the unwritten row, so two new films never reached the site until a restart and nothing counted it. Every failure is also a WARN line naming the film; the cache rolls the row back so the next identical scrape retries. Alerted by RepositoryWritesFailing (worker-pipeline.rules). Every known (collection, op) is seeded at 0 for the expected exception classes (RepositoryWriteMetrics.SeededExceptions) so increase() sees the first failure; any other class appears on its first failure and counts from its second.")
    .labelNames("country", "collection", "op", "exception")
    .register(registry)

  private val decodeFailures = Counter.builder()
    .name("kinowo_worker_decode_failures")
    .help("Documents that could not be decoded, by country and collection. web_movies|web_screenings: SKIPPED by a whole-collection scan, a film or screening the reader goes without while the rest of its page is kept. movies: a read that FAILED on it — a point read answering unreadable, or the whole corpus scan left incomplete. movies|screenings|movie_slots from a change stream: a post-image it SKIPPED (not applied) rather than end its cursor on, which before 2026-09-24 killed the stream for good. ZERO IS THE HEALTHY READING; each was a WARN line and nothing else before 2026-09-24. Alerted by DocumentsUndecodable (worker-pipeline.rules). Seeded at 0 per collection so increase() sees the first skip.")
    .labelNames("country", "collection")
    .register(registry)

  countryCodes.foreach { c =>
    ListingIntakeMetrics.Guards.foreach(g =>
      ListingIntakeMetrics.Verdicts.foreach(v => scrapeGuardVerdicts.labelValues(c, g, v).inc(0.0)))
    ListingIntakeMetrics.SkipReasons.foreach(r => scrapeWriteSkipped.labelValues(c, r).inc(0.0))
    RepositoryWriteMetrics.Writes.foreach { case (collection, op) =>
      RepositoryWriteMetrics.SeededExceptions.foreach(e => repositoryWriteFailed.labelValues(c, collection, op, e).inc(0.0))
    }
    services.readmodel.DecodeFailureMetrics.Collections.foreach(coll => decodeFailures.labelValues(c, coll).inc(0.0))
    Seq("changed", "deleted").foreach(k => cacheRehydrateChanges.labelValues(c, k))
  }

  def recordRehydrate(country: String, changedUpserts: Int, deletes: Int): Unit = {
    if (changedUpserts > 0) cacheRehydrateChanges.labelValues(country, "changed").inc(changedUpserts.toDouble)
    if (deletes > 0)        cacheRehydrateChanges.labelValues(country, "deleted").inc(deletes.toDouble)
  }

  def recordScrapeGuardVerdict(country: String, guard: String, verdict: String): Unit =
    scrapeGuardVerdicts.labelValues(country, guard, verdict).inc()

  def recordScrapeWriteSkipped(country: String, reason: String): Unit =
    scrapeWriteSkipped.labelValues(country, reason).inc()

  def recordRepositoryWriteFailed(country: String, collection: String, op: String, exception: String): Unit =
    repositoryWriteFailed.labelValues(country, collection, op, exception).inc()

  def recordDecodeFailure(country: String, collection: String): Unit =
    decodeFailures.labelValues(country, collection).inc()
}