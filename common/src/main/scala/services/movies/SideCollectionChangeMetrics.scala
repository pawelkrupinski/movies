package services.movies

/**
 * The change-stream half of a side collection's observability — what its cursor
 * delivered, and what the apply coalesced away. Shared by the two side collections
 * split out of `movies` and keyed per cinema slot ([[SlotKeyed]]): `screenings`
 * (through [[ScreeningsMetrics]], which adds the store's write counters) and
 * `movie_slots`.
 *
 * WHY THE SLOTS CURSOR IS METERED FROM ITS FIRST DAY. `ReadModelProjector` is driven by
 * every cursor that can ring it, and the `ReadModelProjectionTriggerUnaccounted` alert
 * reads its projection rate against the SUM of the metered ones — the `screenings`
 * cursor ran unmetered until 2026-09-05 and cost an evening of "unattributable"
 * projections. A third cursor that nothing counted would trip that same rule on the day
 * it shipped, and the rule's own text says so.
 *
 *  - `recordChangeEvent(op)` — one delivered event, by operation. Part of the projector's
 *    REAL input rate.
 *  - `recordCoalescedChange()` — one event that rode an apply already queued for its film
 *    instead of buying its own. See [[ScreeningsMetrics]] for why that number stays
 *    useful after every write guard has done its work.
 *
 * The worker wires the Prometheus-backed [[services.metrics.WorkerTaskMetrics]]; the
 * web, scripts and unit tests use [[SideCollectionChangeMetrics.noop]].
 */
trait SideCollectionChangeMetrics {
  def recordChangeEvent(op: String): Unit
  def recordCoalescedChange(): Unit
}

object SideCollectionChangeMetrics {
  val noop: SideCollectionChangeMetrics = new SideCollectionChangeMetrics {
    def recordChangeEvent(op: String): Unit = ()
    def recordCoalescedChange(): Unit       = ()
  }
}
