package services.identity

import services.cinemas.common.DetailEnricher

/**
 * The identity model's venue detail and nothing else: a listing's page as venue_pages holds it
 * ([[VenuePageIndex]]) — a gap until the page is read, re-asked when its read is announced. TMDB and IMDb are the store's ([[StoredTmdbLookups]], whose `details` this is).
 */
final class VenueDetailLookups(enrichers: Seq[DetailEnricher], index: VenuePageIndex,
                               reads: ObservationReads = ObservationReads.Untracked) extends IdentityLookups {
  private val gaps    = new LookupGaps
  private val details = new VenueDetails(enrichers.map(new VenuePageDetailEnricher(_, index, gaps, reads)), gaps)

  def hasDetail(listing: Listing): Boolean                  = details.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = details.detail(listing)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]   = Answer.Unknown
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
}

/**
 * The model's lookups answered from what it keeps first (`stored`: its normalized TMDB store and the
 * venues' enriched details), and only a TMDB or IMDb question the store has no answer to asked `live`
 * — through the fetch that files its answer into the store (`NormalizingHttpFetch`), then read back from
 * there. So a take-up reads the store in batches instead of asking every question again; venue details
 * are never fetched here, only waited for.
 */
final class StoredFirstLookups(stored: IdentityLookups, live: IdentityLookups) extends ForwardingIdentityLookups(stored) {
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] = storedFirst(stored.candidates(query), live.candidates(query))
  override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = storedFirst(stored.film(tmdbId), live.film(tmdbId))
  /** The store's day of a record it holds — `Unknown` for one it holds dated by its year alone, which the agreement
   *  stage asks TMDB to read again (`agreement.AgreementStage.wantedRecords`) — else the day of the record asked live. */
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId) match {
    case Answer.Known(_) if stored.film(tmdbId).isKnown => stored.releaseDay(tmdbId)
    case answer                                         => answer.map(_.flatMap(_.released))
  }
  override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = storedFirst(stored.cast(tmdbId), live.cast(tmdbId))
  override def overview(tmdbId: Int): Answer[Option[ProseWords]] = storedFirst(stored.overview(tmdbId), live.overview(tmdbId))

  /** The store's answer; on a gap, the live one -- read back from the store it was just filed into, so the question
   *  answers as the store holds it (popularity to its bucket), as every later ask of it will: answered as TMDB stated
   *  it, a take-up decided on one input and its next drain on another. The live answer only when the store took none. */
  private def storedFirst[A](fromStore: => Answer[A], asked: => Answer[A]): Answer[A] =
    fromStore.orElse { val answer = asked; fromStore.orElse(answer) }
}
