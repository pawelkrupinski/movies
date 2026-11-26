package services.identitystaging

import services.tmdb.QuestionMemory
import services.identity.measures.{ResolverDecision, WaitingOn}
import services.identity.staging.{StagedCluster, StagedFact, StagedQuestion}
import services.identity.vocabulary.Question

/** A staging-screen snapshot's ([[StagingSnapshots]]) cluster documents: one staging now, and one that stopped waiting. */
private[services] object StagedClusters {
  /** `earlier`: the cluster as the last publish staged it, whose listings' facts stand for those the projection did not
   *  read this time (a scoped one's, outside its scope). */
  def stagedOf(key: String, decision: ResolverDecision, reading: StagingReading, waits: WaitingOn, since: Long,
                       known: Map[Question, QuestionMemory.Outcome], earlier: Option[StagedCluster]): StagedCluster = {
    val questions = waits.questions.toSeq.map(StagedQuestions.of(_, known)).sortBy(q => (StagedQuestion.Outcomes.indexOf(q.outcome), q.kind, q.text))
    val shown     = reading.shown(decision.members)
    val heldFacts = earlier.fold(Map.empty[(String, String), Seq[StagedFact]])(_.listings.map(l => (l.venue, l.rawTitle) -> l.facts).toMap)
    val listings  = reading.listings(shown).map(l => if (l.facts.nonEmpty) l else l.copy(facts = heldFacts.getOrElse((l.venue, l.rawTitle), Nil)))
    StagedCluster(key, listings, decision.members.size, reading.take(decision, shown), waits.modelGaps, waits.onTmdb,
      questions.take(StagingSnapshots.QuestionsShown), questions.size, questions.count(_.outcome != StagedQuestion.NotAttempted), since)
  }

  /** A cluster that stopped waiting: its questions as the memory last tells them — answered, or left out. */
  def resolvedOf(was: StagedCluster, settled: Option[ResolverDecision], reading: StagingReading, now: Long,
                         known: Map[Question, QuestionMemory.Outcome]): StagedCluster = {
    val questions = was.questions.map(q => Question.parse(q.text).fold(q)(StagedQuestions.of(_, known)))
    was.copy(take = settled.fold(was.take)(d => reading.take(d, reading.shown(d.members))), questions = questions,
      attempted = was.attempted.max(questions.count(_.outcome != StagedQuestion.NotAttempted)),
      resolvedAt = Some(now), resolution = Some(if (settled.isDefined) StagedCluster.Settled else StagedCluster.Regrouped))
  }
}
