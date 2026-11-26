package services.identityadapters

import services.lookupfill.{LookupGap, LookupGaps}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.identity.vocabulary.ObservationReads
import tools.LookupQuery

/**
 * A venue's detail as the identity model reads it: from venue_pages ([[VenuePageIndex]]), never fetched
 * here. Everything but the fetch is the wrapped enricher's. A page not read yet is a gap — `Answer.Unknown`
 * to the model, which re-asks it when the read is announced — never a failure and never a live fetch.
 */
final class VenuePageDetailEnricher(underlying: DetailEnricher, index: VenuePageIndex, gaps: LookupGaps,
                                    reads: ObservationReads = ObservationReads.Untracked) extends DetailEnricher {

  override def cinema: models.Cinema                      = underlying.cinema
  override def detailGroup: String                        = underlying.detailGroup
  override def detailTarget: models.Source                = underlying.detailTarget
  override def enrichmentServiceOverride: Option[String] = underlying.enrichmentServiceOverride
  override def defersTmdbResolution: Boolean              = underlying.defersTmdbResolution
  override def pagesSharedAcrossVenues: Boolean           = underlying.pagesSharedAcrossVenues

  override def fetchFilmDetail(ref: String): Option[FilmDetail] = {
    val key = VenuePageIndex.keyOf(detailGroup, ref)
    reads.read(key)
    index.answer(underlying, ref).getOrElse {
      gaps.record(LookupQuery(key)); throw new LookupGap(key)
    }
  }
}
