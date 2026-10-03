package services.identity

/** A source that has asked everything and found no film anywhere: no venue detail page, every
 *  TMDB search empty and every film id unknown to TMDB. The model over it settles every listing
 *  unmatched, with nothing left to ask. */
object NoFilmLookups extends IdentityLookups {
  def hasDetail(listing: Listing): Boolean                     = false
  def detail(listing: Listing): Answer[Option[DetailFacts]]    = Answer.Known(None)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = Answer.Known(Nil)
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
}

/** Listings with no venue detail page whose TMDB questions are not answered yet: every search
 *  and film read is a gap — what a fill round asks before the store holds anything. */
object UnansweredTmdbLookups extends IdentityLookups {
  def hasDetail(listing: Listing): Boolean                     = false
  def detail(listing: Listing): Answer[Option[DetailFacts]]    = Answer.Known(None)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = Answer.Unknown
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
}
