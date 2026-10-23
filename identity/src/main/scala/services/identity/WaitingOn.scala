package services.identity

/**
 * What a listing's take still waits on, the model's and the agreement's alike (docs/design/identity-questions.md, step
 * 2): the model's candidate queries with no answer yet (`modelGaps`, a count: the resolver keeps no more of them), the
 * agreement's open questions for its cluster and for its take's correction, and whether the agreement's fill waits on a
 * TMDB answer (`onTmdb`, which it does not name). A take waiting on none of them is settled.
 */
final case class WaitingOn(modelGaps: Int, questions: Set[Question], onTmdb: Boolean) {
  def settled: Boolean = modelGaps == 0 && questions.isEmpty && !onTmdb
}

object WaitingOn {
  /** What a decision the agreement does not wait on waits on: the model's gaps alone. */
  def model(decision: ResolverDecision): WaitingOn = WaitingOn(decision.unanswered, Set.empty, onTmdb = false)
}
