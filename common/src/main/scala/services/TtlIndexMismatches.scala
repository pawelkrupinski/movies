package services

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** Collections whose TTL expiry is known to disagree with the code, published as
 *  `kinowo_worker_ttl_index_mismatches` by `services.metrics.TtlIndexMetrics`. One instance
 *  per process, built by the worker's metrics bundle and handed to every reconciler — never
 *  a global, so a spec's mismatches stay its own.
 *
 *  A COUNT THAT IS ALWAYS PRESENT, not a per-collection series that vanishes when
 *  healthy: an alerting expression fires on the PRESENCE of a sample rather than
 *  on its truth, and a gauge that disappears in the good case cannot be told from
 *  a gauge that disappeared because the exporter did. The collection NAMES are in
 *  [[MongoTtlIndex]]'s WARN lines, which is where triage reads them. */
final class TtlIndexMismatches {
  /** Keyed by NAMESPACE (`database.collection`) — see `MongoTtlIndex.reconcile`. Every country
   *  in this JVM has a collection of each name, so the bare name is not an identity. */
  private val namespaces = ConcurrentHashMap.newKeySet[String]()

  /** ONLY THE RECONCILER MAY CREATE A MISMATCH. Package-private so no other caller can
   *  invent one — a gauge anybody can raise is a gauge nobody trusts. */
  private[services] def record(namespace: String): Unit = { namespaces.add(namespace); () }

  /** Clearing is idempotent and cannot manufacture a problem — at worst it silences one
   *  until the next construction re-reads the index and records it again. */
  private[services] def resolved(namespace: String): Unit = { namespaces.remove(namespace); () }

  /** How many TTL indexes are known to be wrong right now. Zero is healthy. */
  def count: Int = namespaces.size

  /** The namespaces, for a diagnostic page or a test — not for a metric label. */
  def names: Set[String] = namespaces.asScala.toSet
}
