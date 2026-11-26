package services.identitystaging

import models.ListingKey
import services.identity.*
import services.identity.measures.ResolverDecision
import services.identity.staging.{StagedCluster, StagedTake, StagingEvent}

/** A staging-screen snapshot's ([[StagingSnapshots]]) documents of the projections' change lists, its clusters keyed by
 *  `clusterKey` ([[agreement.AgreementStage.keyOf]]). */
private[services] final class StagedChanges(clusterKey: ResolverDecision => String) {
  /** The projections' change lists as RESOLUTIONS, every change in exactly one: one document per projection, cluster,
   *  kind and film before — a cluster `decided` (its listings all new), listings that `joined` a cluster already on the
   *  films, or listings whose film was `gained`, `lost` or `changed` (`was` the film they held) — listing the listings
   *  that changed (the first [[StagingSnapshots.ListingsShown]], `listingCount` all of them), its cluster's take as it stands now and,
   *  for a move, the take before ([[StagingReading.prior]]). A
   *  listing in no cluster now is a document of its own, its take the change's film. Keyed by the projection and what
   *  changed, so a publish that fails and writes them with the next writes the same documents. */
  def apply(changed: Seq[StagingEvent.Changed], clusterOf: collection.Map[ListingKey, ResolverDecision],
                        reading: StagingReading): Seq[StagedCluster] =
    changed.groupBy { c =>
      val change = c.change
      (c.projection, clusterOf.get(change.listing).fold(ListingKey.serialised(change.listing))(clusterKey),
        change.kind, change.before)
    }.toSeq.map { case ((projection, cluster, kind, was), changes) =>
      val at       = changes.head.at
      val listings = changes.map(_.change.listing).distinct.sorted
      val shown    = listings.take(StagingSnapshots.ListingsShown)
      val take     = clusterOf.get(listings.head).fold {
        val first = changes.head.change
        StagedTake(first.after, first.after.flatMap(reading.title), first.basisAfter.fold("none")(_.toString), 0.0, None, None, None,
          filmFacts = reading.filmFacts(first.after))
      }(reading.take(_, shown))
      val key      = StagingSnapshots.keyOfId(cluster)
      val priors   = changes.flatMap(c => c.change.prior.map(c.change.listing -> _)).distinctBy(_._1)
      val formerly = changes.iterator.flatMap(c => c.change.formerly.map(c.change.listing -> _)).toMap
      val listed   = shown.zip(reading.listings(shown)).map { case (k, l) => formerly.get(k).fold(l)(f => l.copy(formerly = StagedEvidence.listing(f))) }
      StagedCluster(s"$key@$projection/${kind.label}${was.fold("")("/" + _)}", listed, listings.size, take, 0,
        onTmdb = false, Nil, 0, 0, at, resolvedAt = Some(at), resolution = Some(kind.label), was = was,
        before = Option.when(changes.head.change.moved)(reading.prior(was, priors, take.film)), trigger = changes.head.change.trigger)
    }
}
