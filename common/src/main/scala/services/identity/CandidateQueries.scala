package services.identity

/**
 * CANDIDATE GENERATION's question set (docs/design/identity-resolver.md, phase 2, stage a): what
 * the resolver asks the lookup source about one listing's evidence. A family's set is the union of
 * its members' — a function of the family's listing SET, never of which member arrived first or of
 * an earlier answer (A1).
 *
 *  - every title search `IdentityMeasures.searchQueries` names (each title shape and the original
 *    title, yearless) — the calibration's candidate pools come from the very same list;
 *  - every credited director's filmography;
 *  - the films IMDb lists under the listing's own title, by their IMDb ids — records TMDB's search
 *    misses (an edition, a re-cut) that its IMDb cross-reference still reaches. The title as the
 *    venue published it, whole: a path to the film it names, not to each piece's.
 *
 * The recording sweep (`tools.IdentityLookupSweep`) runs the resolver itself, so it asks exactly
 * these, and every candidate's film record, and nothing else.
 */
object CandidateQueries {

  def of(e: Evidence): Seq[CandidateQuery] = {
    val titles    = IdentityMeasures.searchQueries(e.measured).map(CandidateQuery.Title(_))
    val directors = e.directors.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).distinct.map(CandidateQuery.Director(_))
    val imdb      = Seq(e.title.trim).filter(_.nonEmpty).map(CandidateQuery.Imdb(_))
    // a model's proposed original title is searched as one more title ([[Proposal]])
    val proposed  = e.proposal.filter(_.isFilm).flatMap(_.originalTitle).map(_.trim).filter(_.nonEmpty).map(CandidateQuery.Title(_))
    (titles ++ directors ++ imdb ++ proposed).distinct.sorted
  }
}
