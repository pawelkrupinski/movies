package services.identityadapters

import services.identity.*
import services.identity.staging.StagedCluster
import services.identity.vocabulary.Question

/** What a staging-screen snapshot ([[StagingSnapshots]]) shows as resolved since the last publish: the clusters that
 *  stopped waiting, and the projections' change lists as `changes` documents them. */
private[services] final class StagedResolutions(changes: StagedChanges) {
  /** What resolved since the last publish. A cluster that stopped waiting and whose listings' take the projection changed
   *  is that change — gained, lost, changed, decided or joined — after its wait (since it began, with its questions);
   *  `settled` only when no take moved. Each written once, under its own key (the cluster's and when it was published,
   *  or what changed). */
  def apply(wasStaging: Map[String, StagedCluster], nowStaging: Map[String, StagedCluster], sorted: StagingSort.Sorted,
                            digest: StagingSort.EventDigest, reading: StagingReading, known: Map[Question, QuestionMemory.Outcome], now: Long): Seq[StagedCluster] = {
    val changeDocs = changes(digest.changed, sorted.clusterOf, reading)
    val changedClusters = changeDocs.groupBy(_.key.takeWhile(_ != '@'))
    val waited = wasStaging.collect { case (key, was) if !nowStaging.contains(key) =>
      StagedClusters.resolvedOf(was, sorted.settled.get(key), reading, digest.endedOf.getOrElse(key, now), known)
    } ++ sorted.between.map { case (key, decision, w) =>
      // never seen waiting: its take, and the questions it waited on as the memory tells them now
      StagedClusters.stagedOf(key, decision, reading, WaitingOn(0, w.questions, onTmdb = false), w.at, known, None)
        .copy(resolvedAt = Some(digest.endedOf.getOrElse(key, now)), resolution = Some(StagedCluster.Settled))
    }
    val (waitedThenChanged, settledAlone) = waited.toSeq.partition(c => c.resolution.contains(StagedCluster.Settled) && changedClusters.contains(c.key))
    val afterWaits = waitedThenChanged.iterator.map(c => c.key -> c).toMap
    settledAlone.map(c => c.copy(key = s"${c.key}@$now")) ++ changeDocs.map { doc =>
      afterWaits.get(doc.key.takeWhile(_ != '@')).fold(doc)(w => doc.copy(since = w.since, questions = w.questions,
        questionCount = w.questionCount, attempted = w.attempted))
    }
  }
}
