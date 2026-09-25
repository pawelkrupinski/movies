package tools

/**
 * Real, unfixed findings the convergence legs have caught, held apart so a leg stays a
 * verdict on everything else. Each entry must keep firing: a leg that holds the film and
 * no longer finds the issue fails until the entry is deleted, so an entry can never
 * outlive its bug and hide the next one.
 */
object ConvergenceKnownIssues {

  /** Film keys `ServedCorpusInvariants.wrongMerges` names, by country code.
   *
   *  - `happytogether|2018` (pl): Kinoteka's Wong Kar Wai "Happy Together" (1997), listed
   *    for its 2026 screening, resolves to TMDB 551655 — Kim Jeong-hwan's 2018 film of the
   *    same name — and the misresolution sweep keeps it ("its cinemas name no film").
   *    Found by the wrong-merge check on the PL sample, 2026-09-25. */
  val WrongMerges: Map[String, Set[String]] = Map(
    "pl" -> Set("happytogether|2018"))
}
