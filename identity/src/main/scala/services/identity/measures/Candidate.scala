package services.identity.measures

import services.identity.vocabulary.{Answer, Hit}

/** A film some listing of a family may be: its TMDB id and what TMDB says about it — its own
 *  record when the lookup source holds one (`TmdbFilmRecord`), else what the hits naming it carry. `awaitingRecord`: its
 *  record is not answered yet (a record given up on is not awaited: decided on its hits), so the hits alone carry it and nothing its record states (a running time, credits, where it
 *  was released) can deny it — a family takes a film only where its pool with and without it agree ([[FamilyScope]]). */
final case class Candidate(tmdbId: Int, film: Film, awaitingRecord: Boolean = false) {
  /** Is it a fallback source's film ([[FallbackIds]]), which only a no-match's fallback scores, never TMDB's? */
  def isFallback: Boolean = FallbackIds.isFallback(tmdbId)
  /** A fallback source's film as that source names it: the source's label and the IMDb id it stands for. */
  def fallbackRecord: Option[(String, String)] =
    FallbackIds.unapply(tmdbId).flatMap { case (source, _) => FallbackIds.imdbId(tmdbId).map(source.label -> _) }
}

object Candidate {

  def of(tmdbId: Int, hits: Seq[Hit], record: Answer[Option[Film]]): Candidate = {
    val popularity = hits.map(_.popularity).maxOption
    val best = Hit.best(hits)
    // A record that carries its popularity is the candidate's film itself: a copy restating it was a second Film per
    // recorded film beside the corpus's `records` (30k on worker-uk, 2026-10-07).
    Candidate(tmdbId, record.answered.flatten.map(f => if (f.popularity.isDefined) f else f.copy(popularity = popularity)).getOrElse(
      Film(best.fold("")(_.title), best.flatMap(_.originalTitle), Nil, best.flatMap(_.year),
        popularity = popularity)), awaitingRecord = record.isAwaited)
  }
}
