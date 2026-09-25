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
   *    Found by the wrong-merge check on the PL sample, 2026-09-25.
   *  - `samsonidalila|1949`, `cosifantutte|1992` (pl): the Met's live broadcasts of the operas
   *    (Kino Nowe Horyzonty: 2026, Darko Tresnjak / Phelim McDermott) fold onto DeMille's 1949
   *    film and Tinto Brass's 1992 one, because a bare listing elsewhere resolved the title to
   *    them and a group that resolved to one film folds as one row (60120769c). Keeping them
   *    apart needs a bare listing to have ONE home when the title's group holds a resolved
   *    film and an unresolved one — the landing and the settle currently disagree. */
  val WrongMerges: Map[String, Set[String]] = Map(
    "pl" -> Set("happytogether|2018", "samsonidalila|1949", "cosifantutte|1992"))
}
