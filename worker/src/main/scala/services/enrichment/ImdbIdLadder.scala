package services.enrichment

import tools.AnswerLadder

/**
 * Every rung that can name a film's IMDb id — IMDb's suggestion endpoint and director search ([[ImdbSuggestions]]),
 * then Wikidata, Letterboxd, OMDb and Cinemeta ([[ImdbCrossReferences]]), then IMDb again under the film's other venue
 * titles — asked in turn, cache-free beyond the suggestion cache. The first id wins; a rung whose source FAILED does
 * not stop the ones after it, but if none answers the failure is thrown, not booked as "no match" (AnswerLadder).
 *
 * Asked for any row, cached or not: a recovery holding a row that is not cached (the since-deleted staging fold's)
 * once reached only the first rung; one replay leg logged 1,696 such lookups against 11 that reached the full ladder,
 * precisely the long tail the other rungs exist for. IMDb's suggestion endpoint answers a Polish query with the film's
 * ENGLISH title ("Brzezina" → "The Birch Wood") and the matcher rejects it, while Cinemeta returns tt0068321 for the
 * same query — the exact id production holds.
 */
final class ImdbIdLadder(suggestions: ImdbSuggestions, crossReferences: ImdbCrossReferences) {

  def lookupId(query: ImdbIdQuery): Option[String] =
    AnswerLadder.firstAnswer(
      suggestions.rungs(query) ++ crossReferences.rungs(query) ++ suggestions.otherTitleRungs(query)*)
}
