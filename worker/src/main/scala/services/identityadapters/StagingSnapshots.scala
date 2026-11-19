package services.identityadapters

import services.identity.*
import services.identity.staging.{StagedCluster, StagedQuestion, StagingBeat, StagingEvent}

/**
 * One staging-screen snapshot ([[IdentityStagingPublisher]]), worked out from the takes the projection last wrote, the
 * events since the last written publish and what the last publish left staged: the clusters staging now, the ones that
 * RESOLVED since, and the beat. Pure but for its collaborators' reads: `waitingOn` (what a cluster waits on now),
 * `outcomes` (the question memory's last ask of each question), `storedFilm` and `read` (the facts of films and
 * listings the resolution does not hold) and `clusterKey` (a cluster's key, [[agreement.AgreementStage.keyOf]]).
 */
final class StagingSnapshots(waitingOn: ResolverDecision => WaitingOn,
                             outcomes: Set[Question] => Map[Question, QuestionMemory.Outcome],
                             /** A TMDB film's record where the resolution holds none: a film only a later stage took
                              *  (an agreement), or the one a moved listing held, read from the TMDB store, on the
                              *  publisher's thread. */
                             storedFilm: Int => Option[IdentityMeasures.Film] = _ => None,
                             /** The listings the projection last read, for their facts: walked on the publisher's
                              *  thread ([[IdentityProjection.Projected.read]]). */
                             read: () => Iterator[Listing] = () => Iterator.empty,
                             /** A cluster's key: the agreement stage's ([[agreement.AgreementStage.keyOf]]), which
                              *  a listing sorting first, or leaving, does not move. */
                             clusterKey: ResolverDecision => String = decision => StoredFamily.idOf(decision.members),
                             stagingCap: Int = StagingSnapshots.StagingCap) {
  import StagingSnapshots.*

  /** A cluster's key on the screen: its key ([[clusterKey]]), hashed short. */
  private def keyOf(decision: ResolverDecision): String = keyOfId(clusterKey(decision))
  private val sorting = new StagingSort(waitingOn, keyOf)
  private val resolutions = new StagedResolutions(new StagedChanges(clusterKey))

  /** The snapshot of `resolution`, against `wasStaging` (what the last publish left in the store, by key), over `events`
   *  (those since the last written publish), at `now`; `dropped`: the events let go since the last written beat. */
  def of(resolution: Resolution, wasStaging: Map[String, StagedCluster], events: Vector[StagingEvent], now: Long, dropped: Long): Snapshot = {
    val digest  = StagingSort.EventDigest.of(events)
    val reading = StagingReadings.of(resolution, digest.changed, storedFilm, read)
    val sorted  = sorting(resolution, wasStaging, digest)
    // every question this tick shows, its last ask's outcome read from the memory at once
    val resolvedQuestions = wasStaging.valuesIterator.flatMap(_.questions).flatMap(q => Question.parse(q.text)).toSet
    val known = outcomes(sorted.waiting.iterator.flatMap(_._3.questions).toSet ++ sorted.between.iterator.flatMap(_._3.questions) ++
      resolvedQuestions)
    val nowStaging = sorted.waiting.map { case (key, decision, waits) =>
      val since = (wasStaging.get(key).map(_.since).orElse(digest.startedOf.get(key).map(_.at)) ++
        digest.absorbedBy.getOrElse(key, Nil).flatMap(wasStaging.get).map(_.since)).minOption.getOrElse(now)
      key -> StagedClusters.stagedOf(key, decision, reading, waits, since, known, wasStaging.get(key))
    }.toMap
    val resolvedNow  = resolutions(wasStaging, nowStaging, sorted, digest, reading, known, now)
    val shownStaging = nowStaging.values.toSeq.sortBy(c => (c.since, c.key)).take(stagingCap)
    val open         = nowStaging.values.toSeq.flatMap(_.questions)
    val beat         = StagingBeat(at = now, staging = nowStaging.size, shown = shownStaging.size, resolved = resolvedNow.size,
      openByKind = open.filter(_.outcome != StagedQuestion.Answered).groupMapReduce(_.kind)(_ => 1)(_ + _),
      modelGaps = nowStaging.valuesIterator.map(_.modelGaps).sum, dropped = Option.when(dropped > 0)(dropped))
    Snapshot(shownStaging.map(c => c.key -> c).toMap, resolvedNow.sortBy(_.key), beat, sorted.clustersBy, sorted.listingsBy)
  }

}

object StagingSnapshots {
  /** One publish's snapshot: the staging clusters shown (`next`, by key, the longest waiting), what resolved since (by
   *  key), the beat, and the clusters and listings each basis holds. */
  final case class Snapshot(next: Map[String, StagedCluster], resolved: Seq[StagedCluster], beat: StagingBeat,
                            clustersBy: Map[String, Int], listingsBy: Map[String, Int])

  /** The most staging clusters a country lists, the longest waiting; its beat counts them all. */
  val StagingCap = 500
  /** The most questions a cluster lists; it counts them all. */
  val QuestionsShown = 60
  /** The most listings a cluster lists; it counts them all. */
  val ListingsShown = 12
  /** The most explanation lines a take keeps, the most telling first; a last line counts the rest. */
  val ExplanationShown = 6
  /** The screen's key of the cluster whose key ([[agreement.AgreementStage.keyOf]]) is `id`: hashed short. */
  def keyOfId(id: String): String = java.lang.Long.toHexString(agreement.AgreementStage.digest(Seq(id)))
}
