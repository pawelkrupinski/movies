package services.identityadapters

import services.identity.*
import services.identity.staging.{StagedFact, StagedListing, StagedTake}
import models.ListingKey

/** What one publish reads its takes against: the films of the model's resolution, else of the TMDB store
 *  (`storedFilm`), and each listing's facts (`factsOf`, none when it is not known). */
final class StagingReading(resolution: Resolution, storedFilm: Int => Option[IdentityMeasures.Film],
                            factsOf: ListingKey => Seq[StagedFact]) {
  def film(id: Int): Option[IdentityMeasures.Film] = resolution.films.get(id).orElse(storedFilm(id))
  def title(id: Int): Option[String]                = film(id).map(_.title)
  def filmFacts(id: Option[Int]): Seq[StagedFact]   = id.flatMap(i => film(i).map(StagedEvidence.film(i, _))).getOrElse(Nil)

  /** The listings a cluster lists of `keys`: one per venue and title, the first [[StagingSnapshots.ListingsShown]]. */
  def shown(keys: Seq[ListingKey]): Seq[ListingKey] =
    keys.distinctBy(k => (k.venue, k.rawTitle)).sortBy(k => (k.venue, k.rawTitle)).take(StagingSnapshots.ListingsShown)
  def listings(keys: Seq[ListingKey]): Seq[StagedListing] = keys.map(k => StagedListing(k.venue, k.rawTitle, factsOf(k)))

  /** `decision`'s take as the screen shows it: its films titled, the film's facts, and its trace of each of `shown`. */
  def take(decision: ResolverDecision, shown: Seq[ListingKey]): StagedTake = {
    val leaning = decision.leaning.orElse(decision.candidate).map(_.film)
    StagedTake(decision.film, decision.film.flatMap(title), decision.basis.toString, decision.confidence,
      leaning, leaning.flatMap(title),
      decision.fallback.map(f => s"${f.source}:${f.id}${f.title.fold("")(" " + _)}"), decision.explanationShown(StagingSnapshots.ExplanationShown),
      filmFacts = filmFacts(decision.film.orElse(leaning)),
      nodes = shown.flatMap(k => decision.trace.nodes.get(k).map(StagedEvidence.node(k, _, title,
        Some(DecidedBy.of(decision, k)), Some(DecidedBy.stage(decision, k))))))
  }

  /** The take moved listings held: film `was`, and the decision it came from (`priors`, each listing's, when the
   *  projection saw it) — its basis and confidence; as why its last reason, the rule that decided it and the runner-up
   *  it weighed beside it, said to be the film taken `now` when that one is; that film's facts, and each listing's
   *  trace against it. */
  def prior(was: Option[Int], priors: Seq[(ListingKey, TakeChange.Prior)], now: Option[Int]): StagedTake = {
    val first    = priors.headOption.map(_._2)
    val runnerUp = first.flatMap(_.runnerUp).map { id =>
      s"runner-up then: tmdb $id${title(id).fold("")(" " + _)}" + (if (now.contains(id)) " — the film taken now" else "")
    }
    StagedTake(was, was.flatMap(title), first.fold("unknown")(_.basis.toString), first.fold(0.0)(_.confidence),
      explanation = first.toSeq.flatMap(p => p.reason.toSeq ++ p.decidedBy.map("decided by " + _)) ++ runnerUp,
      filmFacts = filmFacts(was),
      nodes = priors.take(StagingSnapshots.ListingsShown).flatMap { case (k, p) => p.node.map(StagedEvidence.node(k, _, title, p.decidedBy)) })
  }
}

