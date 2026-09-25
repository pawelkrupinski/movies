package tools

/**
 * Real, unfixed findings the convergence legs have caught, held apart so a leg stays a
 * verdict on everything else. Each entry must keep firing: a leg that holds the film and
 * no longer finds the issue fails until the entry is deleted, so an entry can never
 * outlive its bug and hide the next one.
 */
object ConvergenceKnownIssues {

  /** Film keys `ServedCorpusInvariants.wrongMerges` names, by country code. Empty: the
   *  three it has held — PL "Happy Together" resolved to the 2018 film, and the Met opera
   *  broadcasts folded onto the 1949 "Samson i Dalila" and the 1992 "Così fan tutte" — were
   *  fixed at their root (a candidate a venue denies is refused; the fold keeps a denying
   *  venue apart; a bare listing keeps its one home). */
  val WrongMerges: Map[String, Set[String]] = Map.empty
}
