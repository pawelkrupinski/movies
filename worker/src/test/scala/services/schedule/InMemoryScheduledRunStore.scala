package services.schedule

/** In-memory [[ScheduledRunStore]] with real claim-once-per-id semantics — the
 *  first `claim` for an id wins, repeats lose, and a claim is gone 48 h past
 *  `clock`'s instant, as Mongo's TTL deletes it. The single-machine analogue used
 *  to exercise the reaper gating without Mongo. */
class InMemoryScheduledRunStore private (claims: tools.KeyedStore[String, java.time.Instant], clock: java.time.Clock)
    extends KeyedScheduledRunStore(claims, clock) {
  def this(clock: java.time.Clock = InMemoryScheduledRunStore.Unmoving) =
    this(KeyedScheduledRunStore.claims(tools.InMemoryStoreDocuments.expiring(KeyedScheduledRunStore.Expiry, clock)), clock)
  def claimedIds: Set[String] = claims.all().map(_._1).toSet
}

object InMemoryScheduledRunStore {
  /** A clock that never reaches a claim's expiry: the default for a spec not about it. */
  val Unmoving: java.time.Clock = java.time.Clock.fixed(java.time.Instant.parse("2026-01-01T00:00:00Z"), java.time.ZoneOffset.UTC)
}

/** [[ScheduledRunStore]] that never grants a claim — stands in for "another
 *  machine already owns every occurrence", so the caller always skips. */
object NeverClaimScheduledRunStore extends ScheduledRunStore {
  def claim(occurrenceId: String): Boolean = false
}
