package services.identity.agreement

import scala.collection.mutable

import models.ListingKey
import services.identity.StoredFamily
import services.identity.measures.{Resolution, ResolverDecision}
import services.identity.vocabulary.StableNames

/**
 * The agreement stage's cluster KEYS ([[StableNames]], docs/design/identity-cluster-identity.md): a counter of its own,
 * assigned by overlap as the projection's film ids are, and kept in `store` with each cluster's first listing, which a
 * restart re-attaches it by — what the staging screen and the drift check name a cluster by. A key only NAMES a cluster:
 * nothing the stage decides is found by it, and no memory goes with it (docs/design/identity-no-drift.md).
 *
 * Concurrency: [[assign]] and [[publish]] run on the applying thread alone (the stage's apply is synchronized), which
 * owns `live`, `firsts` and the counter. [[keyOf]] is read from any thread, unlocked: it reads two `@volatile` maps that
 * no one changes once [[publish]] swapped them in — the apply's own keys and the apply's before, which stays readable
 * beside it since the projection's last written takes are that apply's until the projection writes.
 */
final class ClusterKeys(store: AgreementKeys) {
  /** Each decision's cluster key: of the model's decisions the last apply that read anything was handed, and of what it
   *  took them as. Built whole by that apply and never changed after. */
  @volatile private var keyed       = new java.util.IdentityHashMap[ResolverDecision, String]()
  @volatile private var keyedBefore = new java.util.IdentityHashMap[ResolverDecision, String]()
  /** Each live key's model decision, as the last apply was handed it: those the model no longer hands over are the
   *  clusters gone, whose keys the clusters overlapping them take ([[StableNames]]). */
  private val live    = mutable.HashMap.empty[Long, ResolverDecision]
  /** Each live key's first listing as `store` holds it, and the next key the counter hands out. */
  private val firsts  = mutable.HashMap.empty[Long, String]
  private var nextKey = 0L

  /** `decision`'s cluster key as the last apply left it: of a model decision it was handed, or of what it took one as.
   *  Read from any thread, allocating nothing; a decision no apply was handed is keyed by its first listing
   *  ([[StoredFamily.idOf]]), as nothing the stage keeps is. */
  def keyOf(decision: ResolverDecision): String = {
    val now = keyed.get(decision)
    if (now != null) now
    else {
      val before = keyedBefore.get(decision)
      if (before != null) before else StoredFamily.idOf(decision.members)
    }
  }

  /** Keys each decision of `resolution` with listings into `keys`: one the last apply was handed keeps its key; the rest
   *  take the keys of the clusters gone since by overlap — on the `first` apply, the stored key of their first listing
   *  first, every other fresh. Each key a merge absorbed goes into `merged`, beside the survivor's
   *  ([[services.identity.staging.StagingEvent.Merged]]); the keys that moved are stored. */
  def assign(resolution: Resolution, first: Boolean, keys: java.util.IdentityHashMap[ResolverDecision, String],
             merged: mutable.Buffer[(String, String)]): Unit = {
    val before  = keyed
    // on the first apply, every key stored: re-attached by first listing, else retired — and the counter past them all
    val stored  = Option.when(first)(store.all())
    stored.foreach(held => nextKey = nextKey max held.next)   // a retired key is never handed out again, keyed now or not
    val unkeyed = mutable.ArrayBuffer.empty[ResolverDecision]
    resolution.decisions.foreach { decision =>
      if (decision.members.nonEmpty) { val key = before.get(decision); if (key != null) keys.put(decision, key) else unkeyed += decision }
    }
    val gone    = mutable.ArrayBuffer.empty[(Long, Seq[ListingKey])]
    live.foreachEntry((key, decision) => if (!keys.containsKey(decision)) gone += key -> decision.members)
    if (unkeyed.nonEmpty || gone.nonEmpty || stored.exists(_.keys.nonEmpty)) rekeyed(unkeyed, gone, stored, keys, merged)
  }
  /** The decisions no key named (`unkeyed`) keyed: re-attached by first listing to a key `stored` holds (the first
   *  apply's), else named by overlap with the clusters `gone` ([[StableNames.assign]]); the keys retired and moved stored
   *  first, then held, and each merge's younger half told in `merged`. */
  private def rekeyed(unkeyed: mutable.ArrayBuffer[ResolverDecision], gone: mutable.ArrayBuffer[(Long, Seq[ListingKey])],
                      stored: Option[AgreementKeys.Held], keys: java.util.IdentityHashMap[ResolverDecision, String],
                      merged: mutable.Buffer[(String, String)]): Unit = {
    val storedFirsts = stored.fold[collection.Map[Long, String]](firsts)(held => held.keys.iterator.map(k => k.key -> k.first).toMap)
    val counter      = nextKey
    val firstOf  = unkeyed.map(decision => StoredFamily.idOf(decision.members))
    val attached = mutable.LongMap.empty[Int]
    stored.foreach { held =>
      val byFirst = held.keys.iterator.map(k => k.first -> k.key).toMap
      unkeyed.indices.foreach(i => byFirst.get(firstOf(i)).foreach(key => attached(key) = i))
    }
    val open     = unkeyed.indices.filterNot(attached.valuesIterator.toSet)
    val assigned = StableNames.assign(gone, open.map(unkeyed(_).members), counter)
    val keyOfIndex = Array.fill(unkeyed.size)(-1L)
    attached.foreachEntry((key, i) => keyOfIndex(i) = key)
    open.indices.foreach(j => keyOfIndex(open(j)) = assigned.names(j))
    val retired = (gone.iterator.map(_._1) ++ stored.iterator.flatMap(_.keys.map(_.key))).toSet -- keyOfIndex
    val moved   = unkeyed.indices.collect { case i if !storedFirsts.get(keyOfIndex(i)).contains(firstOf(i)) => StoredKey(keyOfIndex(i), firstOf(i)) }
    // stored first: a write that fails leaves the keys as they were, and the next apply keys the same decisions again
    if (retired.nonEmpty || moved.nonEmpty || assigned.nextFresh != counter) store.replace(retired, moved, assigned.nextFresh)
    nextKey = assigned.nextFresh
    stored.foreach { _ => firsts.clear(); firsts ++= storedFirsts }
    firsts --= retired
    moved.foreach(k => firsts(k.key) = k.first)
    live --= retired
    // a merge's younger half, named to the staging screen beside the cluster holding most of its listings
    assigned.absorbed.foreach { case (from, j) => merged += keyOfIndex(open(j)).toString -> from.toString }
    unkeyed.indices.foreach { i =>
      live(keyOfIndex(i)) = unkeyed(i)
      keys.put(unkeyed(i), keyOfIndex(i).toString)
    }
  }

  /** `keys`, the apply's (what [[assign]] filled, and what the apply took each keyed decision as), are the keys
   *  [[keyOf]] reads from now; the last apply's stay readable beside them. `keys` must not change after. */
  def publish(keys: java.util.IdentityHashMap[ResolverDecision, String]): Unit = {
    keyedBefore = keyed
    keyed = keys
  }
}
