package services.identity

import services.cinemas.common.DetailEnricher

/**
 * The identity model's venue detail and nothing else: a listing's page as the pipeline's own
 * enrichment left it ([[VenueDetailSlots]]) — a gap until the enrichment asked it, re-asked when it
 * announces the page. TMDB and IMDb are the store's ([[StoredTmdbLookups]], whose `details` this is).
 */
final class VenueDetailLookups(enrichers: Seq[DetailEnricher], slots: VenueDetailSlots,
                               reads: ObservationReads = ObservationReads.Untracked) extends IdentityLookups {
  private val gaps    = new LookupGaps
  private val details = new VenueDetails(enrichers.map(new SourceDataDetailEnricher(_, slots, gaps, reads)), gaps)

  def hasDetail(listing: Listing): Boolean                  = details.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = details.detail(listing)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]   = Answer.Unknown
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
}

/**
 * The model's lookups answered from what it keeps first (`stored`: its normalized TMDB store and the
 * venues' enriched details), and only a TMDB or IMDb question the store has no answer to asked `live`
 * — through the fetch that files its answer into the store (`NormalizingHttpFetch`), whose change
 * re-asks it from there. So a take-up reads the store in batches, as the shadow's does, instead of
 * asking every question again; venue details are never fetched here, only waited for.
 */
final class StoredFirstLookups(stored: IdentityLookups, live: IdentityLookups) extends IdentityLookups {
  def hasDetail(listing: Listing): Boolean                  = stored.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = stored.detail(listing)

  override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit =
    stored.prefetch(queries, films, details)
  override def prefetchAnswered(): Unit = stored.prefetchAnswered()
  override def released(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[services.movies.ListingKey]): Unit =
    stored.released(queries, films, details)

  def candidates(query: CandidateQuery): Answer[Seq[Hit]] = stored.candidates(query) match {
    case Answer.Unknown => live.candidates(query)
    case known          => known
  }
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = stored.film(tmdbId) match {
    case Answer.Unknown => live.film(tmdbId)
    case known          => known
  }
}
