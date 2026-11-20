package services.identityadapters

import services.identity.*
import services.identity.measures.{DecisionTrace, ResolverDecision}

/** The model as a reader sees it ([[ModelSnapshot]]), built from the engine only when read, on the model's thread
 *  ([[IdentityModelService.current]]): its resolution, gaps, listings and questions to renew, with each new listing the
 *  input gate holds and shows meanwhile ([[IdentityModelService.NewWhilePending]]) beside them as its own decision. */
object ModelSnapshots {
  /** `engine`'s snapshot, `shown` -- the held new listings shown while their questions are open -- among its decisions. */
  def of(engine: IncrementalResolver, shown: Seq[ModelInputGate.Held]): ModelSnapshot = {
    val resolution = engine.resolution
    if (shown.isEmpty) ModelSnapshot(resolution, engine.gaps, engine.listings, QuestionRenewals.of(engine.familyQuestions))
    else ModelSnapshot(resolution.copy(decisions = resolution.decisions ++ shown.map(waiting)), engine.gaps,
      engine.listings ++ shown.map(_.newest), QuestionRenewals.of(engine.familyQuestions))
  }

  /** A held new listing as the projection reads it: its own decision, no film, Pending on its questions -- `unanswered`
   *  set so the agreement stage leaves it as it is, as it leaves any decision the model's questions still hold. */
  private[services] def waiting(held: ModelInputGate.Held): ResolverDecision =
    ResolverDecision(Seq(held.newest.key), scala.None, 0.0, ResolverDecision.Basis.NoEvidence,
      Seq(s"held: ${held.waits.size} question(s) of its own not answered yet"), unanswered = held.waits.size, awaits = held.waits)(DecisionTrace.Empty)
}
