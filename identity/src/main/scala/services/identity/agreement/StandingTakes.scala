package services.identity.agreement

import services.identity.{Question, Resolution, ResolverDecision}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
 * The takes the agreement stage serves while a cluster's take waits ([[served]]): each as decided, but one that lost its
 * film to a wait. A cluster whose take now waits on a question -- a candidate's poster or record, a family's answer, a
 * search, opened by an answer filed since (US Alamo "Game" x12, 2026-11-09 and 11-12: a TMDB search refreshed with "I'm
 * Game", then "Blind Game", the fill waited on the new candidate's poster and the cluster was served the model's "no
 * film" for 5-7 s) -- and takes no film meanwhile is served its LAST SETTLED take over the same listings, typed Pending
 * ([[AgreementStage.outcomeOf]]), until its answers are in: the owner, 2026-11-12/13, "they should stay the same until
 * all is resolved for the new one", the input gate's rule ([[services.identity.ListingReadiness]]) for one more trigger.
 * Never a film the cluster was not served settled (the dropped Option B: no new film early), never across a change of
 * its listings, and for at most [[AgreementStage.HoldAtMost]], the gate's bound: past it the take is served as it
 * stands, and counted. A restart holds nothing: what stood before it is not kept.
 *
 * Concurrency: [[served]] runs on the applying thread (the stage's apply is synchronized), the one writer. [[holdOf]],
 * [[due]] and [[held]] are read from any thread: both maps are concurrent, each entry replaced whole.
 */
final class StandingTakes(clock: java.time.Clock) {
  /** Each cluster's last take served with nothing open -- a film or an IMDb fallback -- by cluster key. */
  private val settledTakes = TrieMap.empty[String, ResolverDecision]
  /** The clusters whose last settled take stands while their take waits, since when and on what. */
  private val holds = TrieMap.empty[String, AgreementStage.Hold]

  /** The previous take standing for cluster `id` — one made for `members`, not past its bound. */
  def holdOf(id: String, members: Seq[services.movies.ListingKey]): Option[AgreementStage.Hold] =
    holds.get(id).filter(h => !h.expired && h.members == members)

  /** Has a previous take stood its bound? Then the next apply reads again, though nothing moved, and serves the take as
   *  it stands. */
  def due: Boolean = {
    val now = clock.millis()
    holds.valuesIterator.exists(h => !h.expired && now - h.since >= AgreementStage.HoldAtMost.toMillis)
  }

  /** The previous takes standing now: how many listings they hold, and how long the oldest has stood. */
  def held(): AgreementStage.Held = {
    val now      = clock.millis()
    val standing = holds.valuesIterator.filterNot(_.expired).toSeq
    AgreementStage.Held(standing.map(_.members.size).sum, standing.map(_.since).minOption.fold(0.0)(since => (now - since).max(0L) / 1000.0))
  }

  /** The takes served of `took`, the model's `resolution` as the apply decided it: each keyed decision whose take waits
   *  (`waitsOf` its cluster key: the cluster's, its take's and its correction's open waits) and takes no film served the
   *  cluster's last settled take — keyed as its cluster in `pass` — while that stands; how long each hold that ended
   *  stood is noted in `pass`. */
  def served(resolution: Resolution, took: Seq[ResolverDecision], pass: AgreementStage.Pass,
             waitsOf: String => Seq[Set[Question]]): Seq[ResolverDecision] = {
    val now  = clock.millis()
    val live = mutable.Set.empty[String]
    val served = resolution.decisions.iterator.zip(took).map { case (model, take) =>
      val id = pass.keys.get(model)
      if (id == null || model.members.isEmpty) take
      else {
        live += id
        val waits = waitsOf(id)
        if (take.awaits.isEmpty && waits.isEmpty) settled(id, take, now, pass)
        else standing(id, take, waits, now, pass)
      }
    }.toVector
    settledTakes.filterInPlace((id, _) => live(id))
    holds.filterInPlace((id, _) => live(id))
    served
  }

  private def filmed(d: ResolverDecision) = d.film.isDefined || d.fallback.isDefined

  /** `take`, of cluster `id`, waits on nothing: its hold ends, and a filmed take is the cluster's settled one. */
  private def settled(id: String, take: ResolverDecision, now: Long, pass: AgreementStage.Pass): ResolverDecision = {
    holds.remove(id).filterNot(_.expired).foreach(h => pass.holdsEnded += (now - h.since).max(0L) / 1000.0)
    if (!filmed(take)) settledTakes -= id else if (!settledTakes.get(id).exists(_ eq take)) settledTakes(id) = take
    take
  }

  /** `take`, of cluster `id`, waits on `waits`: the cluster's last settled take over the same listings, while `take`
   *  takes no film and the hold is within its bound — else `take`. */
  private def standing(id: String, take: ResolverDecision, waits: Seq[Set[Question]], now: Long, pass: AgreementStage.Pass): ResolverDecision =
    settledTakes.get(id).filter(before => !filmed(take) && before.members == take.members) match {
      case None         => holds -= id; take
      case Some(before) =>
        val waiting = take.awaits ++ waits.flatten
        val hold    = holds.getOrElse(id, AgreementStage.Hold(now, waiting, take.members))
        if (hold.expired) take
        else if (now - hold.since >= AgreementStage.HoldAtMost.toMillis) {
          holds(id) = hold.copy(expired = true)
          pass.holdsBounded += (now - hold.since) / 1000.0
          take
        }
        else {
          holds(id) = hold.copy(waits = waiting)
          pass.keys.put(before, id)
          before
        }
    }
}
