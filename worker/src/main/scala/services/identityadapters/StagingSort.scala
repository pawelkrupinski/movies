package services.identityadapters

import services.identity.*
import services.identity.staging.{StagedCluster, StagingEvent}
import models.ListingKey

/** How a staging-screen snapshot ([[StagingSnapshots]]) sorts the clusters of a resolution against its events: by
 *  `waitingOn` (what a cluster waits on now) and `keyOf` (a cluster's key on the screen). */
private[services] final class StagingSort(waitingOn: ResolverDecision => WaitingOn, keyOf: ResolverDecision => String) {
  /** Each cluster of `resolution` sorted: waiting now, settled after staging, or a wait begun and ended between two
   *  publishes; the clusters and listings each basis holds; and each changed listing's cluster now. */
  def apply(resolution: Resolution, wasStaging: Map[String, StagedCluster], digest: StagingSort.EventDigest): StagingSort.Sorted = {
    val clusters = resolution.decisions.filter(_.members.nonEmpty)
    val byBasis  = clusters.groupBy(_.basis.toString)
    val (waiting, settled, between) = byWait(clusters, wasStaging, digest)
    StagingSort.Sorted(waiting, settled, between, byBasis.view.mapValues(_.size).toMap,
      byBasis.view.mapValues(_.iterator.map(_.members.size).sum).toMap, clustersOfChanged(clusters, digest))
  }

  /** `clusters` by their wait: waiting now, settled after staging, or a wait begun and ended between two publishes --
   *  since it began, when it ended, and on what. */
  private def byWait(clusters: Seq[ResolverDecision], wasStaging: Map[String, StagedCluster], digest: StagingSort.EventDigest)
      : (Seq[(String, ResolverDecision, WaitingOn)], Map[String, ResolverDecision], Seq[(String, ResolverDecision, StagingEvent.Waiting)]) = {
    val waiting = Seq.newBuilder[(String, ResolverDecision, WaitingOn)]
    val settled = Map.newBuilder[String, ResolverDecision]
    val between = Seq.newBuilder[(String, ResolverDecision, StagingEvent.Waiting)]
    clusters.foreach { decision =>
      val key   = keyOf(decision)
      val waits = waitingOn(decision)
      if (!waits.settled) waiting += ((key, decision, waits))
      else if (wasStaging.contains(key)) settled += key -> decision
      else digest.startedOf.get(key).foreach(w => between += ((key, decision, w)))
    }
    (waiting.result(), settled.result(), between.result())
  }

  /** Each changed listing's cluster now: only those changed are looked up. */
  private def clustersOfChanged(clusters: Seq[ResolverDecision], digest: StagingSort.EventDigest): collection.Map[ListingKey, ResolverDecision] = {
    val changed   = digest.changed.iterator.map(_.change.listing).toSet
    val clusterOf = scala.collection.mutable.HashMap.empty[ListingKey, ResolverDecision]
    if (changed.nonEmpty) clusters.foreach(decision => decision.members.foreach(m => if (changed(m)) clusterOf(m) = decision))
    clusterOf
  }
}

private[services] object StagingSort {
  /** What a publish reads of its events: each cluster's first wait started and last wait ended since — when it began
   *  waiting (and on what), and when it stopped; most new clusters wait only inside one projection (their families asked
   *  inline), never seen staging — the clusters each merged cluster absorbed (a wait one of them began is the merged
   *  cluster's, since it began), and the projections' changes. */
  final case class EventDigest(startedOf: Map[String, StagingEvent.Waiting], endedOf: Map[String, Long],
                                       absorbedBy: Map[String, Seq[String]], changed: Seq[StagingEvent.Changed])
  object EventDigest {
    def of(events: Seq[StagingEvent]): EventDigest = EventDigest(
      events.foldLeft(Map.empty[String, StagingEvent.Waiting]) {
        case (m, w: StagingEvent.Waiting) => val key = StagingSnapshots.keyOfId(w.cluster); if (m.contains(key)) m else m.updated(key, w)
        case (m, _)                       => m
      },
      events.collect { case StagingEvent.Settled(cluster, at) => StagingSnapshots.keyOfId(cluster) -> at }.toMap,
      events.collect { case StagingEvent.Merged(cluster, absorbed, _) => StagingSnapshots.keyOfId(cluster) -> StagingSnapshots.keyOfId(absorbed) }.groupMap(_._1)(_._2),
      events.collect { case c: StagingEvent.Changed => c })
  }

  /** The clusters of a resolution, sorted ([[StagingSort.apply]]). */
  final case class Sorted(waiting: Seq[(String, ResolverDecision, WaitingOn)], settled: Map[String, ResolverDecision],
                                  between: Seq[(String, ResolverDecision, StagingEvent.Waiting)],
                                  clustersBy: Map[String, Int], listingsBy: Map[String, Int],
                                  clusterOf: collection.Map[ListingKey, ResolverDecision])
}
