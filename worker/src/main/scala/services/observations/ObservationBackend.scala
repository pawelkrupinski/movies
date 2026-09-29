package services.observations

import java.time.Instant

/**
 * One stored observation, opaque to storage: a key, the scope it is reported under (the venue
 * or the service host), a content hash, the gzipped payload, and its timestamps. Every rule about
 * what these mean lives in [[ObservationStore]]; a backend only keeps them.
 *
 * `current` marks the newest observation of its key — exactly one per live key.
 */
final case class StoredObservation(
  key:        String,
  scope:      String,
  hash:       String,
  payload:    Array[Byte],
  observedAt: Instant,
  lastSeenAt: Instant,
  expireAt:   Instant,
  current:    Boolean
)

/**
 * Where one kind of observation lives — the storage seam, and nothing else. The Mongo backend
 * keeps them in a shadow collection (`obs_listings`, `obs_lookups`) that nothing serving reads;
 * the in-memory one holds a map. Neither decides anything: dedup, the transient-failure rule and
 * expiry are [[ObservationStore]]'s, above this line.
 */
trait ObservationBackend {

  /** The current observation of `key`, expired or not — the store filters. */
  def current(key: String): Option[StoredObservation]

  /** The current observations of `keys`, in one round-trip where the backend can: what many
   *  concurrent readers' `current` calls cost together ([[CoalescedObservationBackend]]). */
  def currents(keys: Seq[String]): Map[String, StoredObservation] =
    keys.flatMap(key => current(key).map(key -> _)).toMap

  /** Every observation of `key`, oldest first, expired or not. */
  def history(key: String): Seq[StoredObservation]

  /** Every current observation, expired or not. */
  def allCurrent(): Seq[StoredObservation]

  /** The current observations whose key starts with `keyPrefix` (all, without one), a page at a
   *  time: a scan that never holds more than a page — the identity store's backfill. */
  def eachCurrent(keyPrefix: Option[String])(page: Seq[StoredObservation] => Unit): Unit =
    allCurrent().filter(o => keyPrefix.forall(o.key.startsWith)).grouped(1000).foreach(page)

  /** Add `observation`, which is current. The caller has retired the previous one first. */
  def insert(observation: StoredObservation): Unit

  /** Mark `key`'s current observation superseded, expiring at `expireAt`. */
  def retire(key: String, expireAt: Instant): Unit

  /** Restamp `key`'s current observation: seen again at `lastSeenAt` (when given), and
   *  expiring at `expireAt`. */
  def renew(key: String, lastSeenAt: Option[Instant], expireAt: Instant): Unit

  def close(): Unit = ()
}

/** The test / Mongo-less backend: a map, nothing more. */
class InMemoryObservationBackend extends ObservationBackend {
  private val byKey = scala.collection.mutable.Map.empty[String, Vector[StoredObservation]]

  def current(key: String): Option[StoredObservation] = synchronized(byKey.get(key).flatMap(_.find(_.current)))
  def history(key: String): Seq[StoredObservation] = synchronized(byKey.getOrElse(key, Vector.empty))
  def allCurrent(): Seq[StoredObservation] = synchronized(byKey.values.flatMap(_.filter(_.current)).toSeq)

  def insert(observation: StoredObservation): Unit = synchronized {
    byKey.update(observation.key, byKey.getOrElse(observation.key, Vector.empty) :+ observation)
  }

  def retire(key: String, expireAt: Instant): Unit = update(key)(_.copy(current = false, expireAt = expireAt))

  def renew(key: String, lastSeenAt: Option[Instant], expireAt: Instant): Unit =
    update(key)(o => o.copy(lastSeenAt = lastSeenAt.getOrElse(o.lastSeenAt), expireAt = expireAt))

  private def update(key: String)(f: StoredObservation => StoredObservation): Unit = synchronized {
    byKey.get(key).foreach(all => byKey.update(key, all.map(o => if (o.current) f(o) else o)))
  }
}
