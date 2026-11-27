package services.identity.agreement

import models.ListingKey
import services.identity.NonFilmEvents
import services.identity.measures.{Listing, PosterEvidence, Resolution, ResolverDecision}

/** What the pipeline read of each model decision, kept while the model hands that same object over ([[AgreementPipeline]]):
 *  what it was taken as ([[reused]]), its cluster's listings and their digest ([[clusterOf]]), and whether it is an event
 *  no film database holds ([[eventOf]]). The model hands over a new decision when one of its listings or a fact it reads
 *  moves, so a quiet tick reads no listing again. Each venue detail page a listing's digest reads comes from
 *  `evidence` ([[AgreementEvidence.detailMark]]). */
private[agreement] final class DecisionCaches(evidence: AgreementEvidence) {
  /** Each model decision the pipeline took, with what it took it as: handed back as that same object while neither moved,
   *  so the projection, which diffs decisions by identity, redrafts a cluster only when its take moved ([[reused]]). */
  private val takenAs = new java.util.IdentityHashMap[ResolverDecision, ResolverDecision]()
  /** Each model decision's cluster id, listings and their digest, kept while the decision is the same object — one
   *  re-published with a field only the stage reads is forgotten ([[forgetRead]]). */
  private[agreement] val digested = new java.util.IdentityHashMap[ResolverDecision, AgreementEvidence.Cluster]()
  /** Each model decision's verdict as an event no film database holds ([[NonFilmEvents]]), kept while the decision is
   *  the same object: such a cluster is asked nothing — neither the agreement's questions nor a model take's correction. */
  private val events = new java.util.IdentityHashMap[ResolverDecision, Option[String]]()

  /** `now`, what the model's `decision` is taken as — `decision` itself while it is taken as it stands, else the object
   *  it was last taken as while the two are equal ([[takenAs]]). */
  def reused(decision: ResolverDecision, now: ResolverDecision): ResolverDecision =
    if (now eq decision) decision
    else Option(takenAs.get(decision)).filter(_ == now).getOrElse { takenAs.put(decision, now); now }

  /** Why the decision's cluster is an event no film database holds, or `None` — read once per decision object. */
  def eventOf(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): Option[String] =
    Option(events.get(decision)).getOrElse {
      // a title shaped as a franchise's marathon is a film's own as the MODEL's searches found ([[ResolverDecision.filmTitled]]):
      // its answers, in its decision -- one moved hands the stage a new decision, read again -- never a search of the stage's
      val event = NonFilmEvents.of(decision.members.flatMap(listingOf), listing => decision.filmTitled(listing.key))
      events.put(decision, event); event
    }

  /** The decision's cluster key (`key`), its listings sorted and their digest, kept while the model hands the same
   *  decision over — which keeps its key as long. */
  def clusterOf(decision: ResolverDecision, key: String, listingOf: ListingKey => Option[Listing]): AgreementEvidence.Cluster =
    Option(digested.get(decision)).getOrElse {
      val listings = AgreementEvidence.sortedListings(decision, listingOf)
      val fresh    = AgreementEvidence.Cluster(key, listings,
        AgreementStage.digest(Seq(digestOf(listings).toString, decision.leaning.toString, decision.candidate.toString,
          PosterEvidence.urls(listings).mkString("\u0001"))))
      digested.put(decision, fresh); fresh
    }

  /** The listings as published, the people their own text names, and the venue detail page the picks read of each. */
  private def digestOf(listings: Seq[Listing]): Long =
    AgreementStage.digest(listings.map(l => l.sortKey + l.screenedSeason.fold("")(season => s"\u0002$season") +
      (if (l.names.isEmpty) "" else s"\u0003${l.names.##}") +
      evidence.detailMark(l)))

  /** Forget the listings read for `decisions`: each read again. */
  def forgetRead(decisions: Iterable[ResolverDecision]): Unit = decisions.foreach(digested.remove)
  /** Forget the listings read for every decision. */
  def forgetAllRead(): Unit = digested.clear()

  /** Forget every decision `resolution` no longer holds. */
  def retainOnly(resolution: Resolution): Unit = {
    val current = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[ResolverDecision, java.lang.Boolean]())
    resolution.decisions.foreach(current.add)
    takenAs.keySet.retainAll(current)   // the model's decisions this resolution no longer holds
    digested.keySet.retainAll(current)
    events.keySet.retainAll(current)
  }
}
