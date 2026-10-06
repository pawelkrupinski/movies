package services.movies

import models.{Cinema, SourceData}

/**
 * What a venue's own page for a listing states about its film, as `venue_pages` last read it — landed as a cinema slot
 * holds a page's fields — or `None` when the page is not read yet, is gone, or is no page whose detail lands on the
 * venue's own slot (a chain's detail lands on its network source). The one source a venue slot's page-written detail
 * comes from ([[CinemaSlotBuilder]]), so a value carried from another page's slot never outlives a read of its own.
 */
trait VenuePageFacts {
  def of(cinema: Cinema, page: String): Option[SourceData]

  /** What `cinema`'s pages among `keys` state, as one number: a slot built from those listings is built from it too. */
  final def digest(cinema: Cinema, keys: Seq[ListingKey]): Int =
    keys.collect { case ListingKey.Native(_, page, _) => of(cinema, page) }.##
}

object VenuePageFacts {
  /** No page read anywhere: every slot carries its page-written detail from the slot it is built over. */
  val none: VenuePageFacts = (_, _) => None
}
