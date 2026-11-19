package services.identity

import models.{Cinema, CinemaMovie, ListingKey, Source, SourceData}

/**
 * Where a projection's venue slots come from: each venue's rows, read again by the listing key each is published under,
 * a digest of the detail pages its listings publish, and a film's slots at a venue built from those rows over the films
 * the projection's index stored. Drafting builds its lean slots from it ([[VenueSlotBuilds]]) and the write completes a
 * draft from it ([[ProjectionDraft.complete]]): handed in from the composition root ([[VenueSlots.FromArchive]]), never
 * held by the draft, which is data.
 */
trait VenueSlotSource {
  /** `cinemas`' rows now, each venue's by the listing key each row is published under. */
  private[identity] def rowsOf(cinemas: Set[Cinema]): Map[Cinema, Map[ListingKey, Seq[CinemaMovie]]]
  /** `cinema`'s slots on the film of `keys` from its `rows`, over the films `index` stored. */
  private[identity] def build(index: ProjectionIndex, cinema: Cinema, keys: Seq[ListingKey],
                              rows: Map[ListingKey, Seq[CinemaMovie]]): Seq[(Source, SourceData)]
  /** A digest of the detail pages the listings `keys` publish at `cinema`: a venue's slots are built from them beside its
   *  rows, so a page that moved is a change. */
  private[identity] def pagesDigest(cinema: Cinema, keys: Seq[ListingKey]): Int
}
