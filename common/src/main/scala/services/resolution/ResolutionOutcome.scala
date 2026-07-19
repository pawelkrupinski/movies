package services.resolution

/**
 * What one [[ResolutionCache.getOrResolve]] call actually did, as a stable metric
 * label value. One call → one outcome.
 *
 * The split is deliberately four-way rather than the obvious hit/miss, because
 * the question the counter exists to answer is "what does this cache save, and
 * what does it fail to save":
 *
 *  - `hit_memory` / `hit_store` — a probe chain AVOIDED (the saving). Splitting
 *    the two layers shows how much of the benefit survives a restart: only
 *    `hit_store` does, so a high memory / low store ratio means the durable half
 *    is carrying little and could go.
 *  - `miss_resolved` — the chain ran and produced an answer, now cached. Each of
 *    these mints a future `hit_*`.
 *  - `miss_unresolved` — the chain ran and produced NOTHING. Hits-only means this
 *    is not cached, so the same film burns the same full chain (up to ~30 GETs
 *    for RT/Metacritic) on every subsequent cycle. This is the category the
 *    cache gives zero protection to, and the one worth watching.
 *
 * Kept in `common` alongside the cache because the taxonomy is generic; the
 * worker supplies the [[ResolutionOutcomeRecorder]] that turns an outcome into a
 * Prometheus increment, so `common` stays free of any metrics dependency (same
 * seam as [[tools.HttpOutcome]] / [[tools.HttpOutcomeRecorder]]).
 */
object ResolutionOutcome {
  val HitMemory      = "hit_memory"       // served by Caffeine; loader never ran
  val HitStore       = "hit_store"        // Caffeine cold, served by the durable store
  val MissResolved   = "miss_resolved"    // resolved live, written through
  val MissUnresolved = "miss_unresolved"  // resolved live to None — NOT cached, will retry

  /** Every outcome, for seeding the metric at 0 so no Grafana line pops in when a
   *  category first fires. */
  val all: Seq[String] = Seq(HitMemory, HitStore, MissResolved, MissUnresolved)

  /** The per-source label value for a resolution-cache Mongo collection
   *  (`resolve_rt` → `rt`), so the wiring's one factory can label without each
   *  call site repeating the source name. */
  def sourceOf(collection: String): String = collection.stripPrefix("resolve_")
}

/**
 * Sink for resolution-cache outcomes. The worker binds one of these per
 * (country, source) — baking both labels — onto its Prometheus counter; `common`
 * sees only this narrow interface.
 */
trait ResolutionOutcomeRecorder {
  def record(outcome: String): Unit
}

object ResolutionOutcomeRecorder {
  /** A no-op recorder — the default when nothing is wired (scripts, tests that
   *  don't assert on outcomes), so the cache is safe to construct unmetered. */
  val noop: ResolutionOutcomeRecorder = (_: String) => ()
}
