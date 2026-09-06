package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Indexes
import play.api.Logging

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Bring a single-field TTL index in line with a wanted `expireAfterSeconds`, by
 *  READING IT BACK FIRST and then REBUILDING it if it disagrees.
 *
 *  `createIndex` can create a TTL index but never ALTER one. The obvious way to
 *  change an existing expiry is `collMod` — and this fleet cannot use it. The
 *  callers used to fire it unconditionally on every construction and swallow the
 *  result at `logger.debug`, which hid two things at once:
 *
 *  1. `kinowo_app` holds `readWrite` on the country databases, and `readWrite`
 *     DOES NOT INCLUDE `collMod`. Every one of those commands came back
 *     `Unauthorized` (13), so the reconciliation the comments described has
 *     never once run in production, and nothing said so because `debug` is not
 *     enabled there. Measured 2026-09-05: ~30 rejected `collMod`s per pod boot,
 *     ~300 across a ten-pod rollout, each counted by mongod as a USER ASSERT —
 *     enough to help trip `MongodUserAssertionsRising` on a five-worker
 *     `Recreate` rollout.
 *  2. AND IT WAS HIDING A REAL DRIFT. The `uptimeBuckets` and `resolve_*` indexes
 *     did all happen to carry the wanted expiry, which made the dead mechanism look
 *     harmless. `detailCache-cinema-city` did not: read on 2026-09-06 it was still
 *     reaping at 6h in all five countries although `5096417e3` had set it to 2h the
 *     day before — and that commit existed because 6h against a 6h
 *     `Freshness.ttlFor(DetailEnrich)` window made half the scheduled refreshes
 *     answer from the cache's own copy and stamp `lastFetchedAt = now`, a refresh
 *     that cannot observe a change recorded as though it had. It took a read-back
 *     to notice; it was fixed by hand, and this class is why the reconciliation now
 *     has to work rather than merely be attempted.
 *
 *  `listIndexes`, `dropIndex` and `createIndex` ARE all covered by `readWrite`
 *  (asked of the server directly, not assumed). So the order turns around: read the
 *  expiry, and when it disagrees REBUILD the index rather than editing it — the one
 *  reconciliation this credential can actually perform. In the steady state that is
 *  a single `listIndexes` and nothing else.
 *
 *  Every step is a `Try`: a collection whose TTL cannot be reconciled must not
 *  stop the monitor or the resolution store from running. */
object MongoTtlIndex extends Logging {

  /** Ensure `collection` carries a TTL index on `field` expiring after
   *  `wantedSeconds` — creating it when absent, reconciling it when it
   *  disagrees, and doing nothing at all when it already agrees.
   *
   *  `label` names the CALLER, not the collection: this logs the collection
   *  itself, so passing a collection name here reads `resolve_tmdb: resolve_tmdb
   *  TTL index on ...`. */
  def reconcile(
    collection:    MongoCollection[Document],
    field:         String,
    wantedSeconds: Long,
    label:         String
  ): Unit = {
    val name = collection.namespace.getCollectionName
    // THE KEY IS THE NAMESPACE, NOT THE COLLECTION NAME. A worker JVM builds one wiring per
    // country in `KINOWO_COUNTRIES`, and every country has its own `uptimeBuckets`,
    // `resolve_*` and `detailCache-*` in its own database. Keyed by the bare name, Germany
    // reconciling its `uptimeBuckets` would clear the entry Poland recorded for a broken one
    // of the same name — the gauge would fall to zero and the alert clear itself while the
    // index was still wrong. That is the exact false negative this metric exists to prevent.
    val key = collection.namespace.getFullName
    currentExpiry(collection, field) match {
      case Some(actual) if actual == wantedSeconds =>
        Mismatches.resolved(key)

      case Some(actual) =>
        logger.warn(s"$label: $name TTL index on `$field` expires after ${actual}s, want ${wantedSeconds}s — rebuilding it.")
        rebuild(collection, field, wantedSeconds, label)

      case None =>
        Try {
          Await.result(collection.createIndex(
            Indexes.ascending(field),
            new JIndexOptions().expireAfter(wantedSeconds, TimeUnit.SECONDS)
          ).toFuture(), 10.seconds)
          // SYMMETRY, so a namespace cannot stay counted after it is healthy again. Every
          // branch that ends with the index correct clears the entry; every branch that ends
          // with it wrong records one. There is no path to this today — `reconcile` runs once
          // per namespace per process, so nothing recorded a mismatch earlier in this JVM for
          // the clear to matter — which is why no case reaches it and none is invented. It is
          // here so the invariant holds if that ever stops being true.
          Mismatches.resolved(key)
        }.recover { case exception =>
          // `IndexOptionsConflict` HERE MEANS THE READ ABOVE FAILED, not that the index is
          // absent — `currentExpiry` returns None for an unreadable collection too, and
          // `createIndex` is then rejected by the index it could not see. Saying "could not
          // be created" would send a reader looking for a missing index that is right there
          // with the wrong expiry, so name both possibilities.
          logger.warn(s"$label: $name has no readable TTL index on `$field` and one could not be created — " +
            s"if it exists, it KEEPS ITS OLD EXPIRY rather than ${wantedSeconds}s: ${exception.getMessage}")
          Mismatches.record(key)
        }
    }
  }

  /** The `expireAfterSeconds` of the existing single-field TTL index on `field`,
   *  or None when there is no such index — or when the read itself failed, in
   *  which case `createIndex` is the right next move and reports properly if the
   *  index is in fact already there. */
  private def currentExpiry(collection: MongoCollection[Document], field: String): Option[Long] =
    Try {
      Await.result(collection.listIndexes().toFuture(), 10.seconds).flatMap { index =>
        val onFieldAlone = index.get("key")
          .collect { case keys: org.bson.BsonDocument => keys }
          .exists(keys => keys.size == 1 && keys.containsKey(field))
        if (onFieldAlone) index.get("expireAfterSeconds").collect { case seconds: org.bson.BsonNumber => seconds.longValue() }
        else None
      }.headOption
    }.recover { case exception =>
      logger.debug(s"${collection.namespace.getCollectionName} index read failed, treating `$field` as un-indexed: ${exception.getMessage}")
      None
    }.toOption.flatten

  /** DROP AND RECREATE, because `collMod` is the one operation `readWrite` does not
   *  carry. Asked directly (`db.getRole("readWrite", {showPrivileges: true})` on
   *  mongo-1), the role grants `listIndexes`, `createIndex` and `dropIndex` and NOT
   *  `collMod` — so the in-place edit is unavailable to `kinowo_app` and the rebuild
   *  is the only reconciliation it can perform itself. That matters more than the
   *  elegance of the in-place edit: the alternative was a hand-granted role per
   *  country, and this repository has already shipped a country whose grant was
   *  forgotten (see `mongo-ci-read.nix`). Nothing has to be granted for a sixth
   *  country here.
   *
   *  THE WINDOW IS REAL AND SMALL. Between the drop and the create nothing reaps
   *  this collection. These are caches and rolling metric buckets, the largest of
   *  them ~6k documents per country, so the rebuild is milliseconds and a few
   *  unreaped seconds cost nothing.
   *
   *  THE FAILURE THAT MATTERS IS A DROP THAT SUCCEEDS AND A CREATE THAT DOES NOT —
   *  the collection is then left with NO TTL index and grows without bound. So the
   *  expiry is READ BACK AGAIN afterwards and a disagreement is recorded in
   *  [[Mismatches]] for `TtlIndexMetrics` to publish, because this is exactly the
   *  class of failure that spent months invisible in a `logger.debug`. */
  private def rebuild(collection: MongoCollection[Document], field: String, wantedSeconds: Long, label: String): Unit = {
    val name = collection.namespace.getCollectionName
    val key  = collection.namespace.getFullName
    Try {
      Await.result(collection.dropIndex(Indexes.ascending(field)).toFuture(), 10.seconds)
      Await.result(collection.createIndex(
        Indexes.ascending(field),
        new JIndexOptions().expireAfter(wantedSeconds, TimeUnit.SECONDS)
      ).toFuture(), 10.seconds)
    } match {
      case Failure(exception) =>
        logger.warn(s"$label: $name TTL index on `$field` could not be rebuilt to ${wantedSeconds}s: ${exception.getMessage}")
        Mismatches.record(key)
      case Success(_) =>
        currentExpiry(collection, field) match {
          case Some(actual) if actual == wantedSeconds =>
            logger.info(s"$label: $name TTL index on `$field` now expires after ${wantedSeconds}s.")
            Mismatches.resolved(key)
          case other =>
            logger.warn(s"$label: $name TTL index on `$field` reads back as ${other.map(_.toString).getOrElse("ABSENT")} " +
              s"after a rebuild to ${wantedSeconds}s — the collection may now have NO TTL index and will grow.")
            Mismatches.record(key)
        }
    }
  }

  /** Collections whose TTL expiry is known to disagree with the code, published as
   *  `kinowo_worker_ttl_index_mismatches` by `services.metrics.TtlIndexMetrics`.
   *
   *  A COUNT THAT IS ALWAYS PRESENT, not a per-collection series that vanishes when
   *  healthy: an alerting expression fires on the PRESENCE of a sample rather than
   *  on its truth, and a gauge that disappears in the good case cannot be told from
   *  a gauge that disappeared because the exporter did. The collection NAMES are in
   *  the WARN lines above, which is where triage reads them. */
  object Mismatches {
    /** Keyed by NAMESPACE (`database.collection`) — see `reconcile`. Every country in this JVM
     *  has a collection of each name, so the bare name is not an identity. */
    private val namespaces = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    /** ONLY THE RECONCILER MAY CREATE A MISMATCH. Package-private so no other caller can
     *  invent one — a gauge anybody can raise is a gauge nobody trusts. */
    private[services] def record(namespace: String): Unit = { namespaces.add(namespace); () }

    /** Clearing is public and idempotent, because it has honest callers outside this package:
     *  a successful reconcile, and any test that has to leave this process-wide register as it
     *  found it. Clearing an entry cannot manufacture a problem — at worst it silences one
     *  until the next construction re-reads the index and records it again. */
    def resolved(namespace: String): Unit = { namespaces.remove(namespace); () }
    /** How many TTL indexes are known to be wrong right now. Zero is healthy. */
    def count: Int = namespaces.size
    /** The namespaces, for a diagnostic page or a test — not for a metric label. */
    def names: Set[String] = { import scala.jdk.CollectionConverters._; namespaces.asScala.toSet }
  }
}
