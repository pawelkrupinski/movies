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
 *  - `miss_unresolved` — the chain ran and produced NOTHING. Under
 *    [[UnresolvedPolicy.Retry]] that answer isn't cached, so the same film burns
 *    the same full chain (up to ~20 GETs for RT/Metacritic, ~55 for Filmweb) on
 *    every subsequent cycle — the category the cache gives zero protection to,
 *    and the one worth watching. Under `Remember` the miss is cached like a hit,
 *    so it fires once and then shows up as `hit_*`; a source that stays pinned
 *    at a high `miss_unresolved` rate with no hits is one whose keys are churning
 *    (different hints every cycle), not one the policy failed to help.
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
  val MissUnresolved = "miss_unresolved"  // resolved live to None (cached or not, per UnresolvedPolicy)

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
