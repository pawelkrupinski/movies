package services.cinemas.common

import models.Cinema

/** How one landed listing came out on completeness, and why not, when not. */
enum ListingCompleteness(val label: String) {
  case Complete extends ListingCompleteness("complete")
  /** A page the scrape read failed ([[ListingReads]]). */
  case PageFailed extends ListingCompleteness("page_failed")
  /** The scraper's own structure says it is short: a chunked run reduced with a chunk
   *  missing, or with a read inside a chunk or the plan that failed. */
  case ChunkIncomplete extends ListingCompleteness("chunk_incomplete")
}

object ListingCompleteness {
  /** The reasons a listing is incomplete — the closed `reason` label set. */
  val Reasons: Seq[ListingCompleteness] = Seq(PageFailed, ChunkIncomplete)

  def of(structure: ListingStructure, readsComplete: Boolean): ListingCompleteness =
    if (structure == ListingStructure.Partial) ChunkIncomplete else if (!readsComplete) PageFailed else Complete
}

/** Whether a scraper's own structure vouches for the listing its `fetch()` returned being the venue's whole listing
 *  ([[CinemaScraper.listingStructure]]). */
enum ListingStructure {
  case Whole
  /** A chunked run reduced with a chunk missing, or with a read inside a chunk or the plan that failed. */
  case Partial
}

object ListingStructure {
  /** [[Whole]] when `whole`, else [[Partial]]. */
  def wholeWhen(whole: Boolean): ListingStructure = if (whole) Whole else Partial
  /** Whole only when every listing composed is. */
  def of(listings: Seq[ListingStructure]): ListingStructure = wholeWhen(listings.forall(_ == Whole))
}

/** What an EMPTY listing means ([[CinemaScraper.emptyListing]]). */
enum EmptyListing {
  /** Nothing vouches for it: as likely a parser that stopped matching as a venue with nothing on. */
  case Unvouched
  /** The venue's own source parsed and says it has no schedule. */
  case NoScheduleListed
}

object EmptyListing {
  /** The source said so only when every listing composed did. */
  def of(listings: Seq[EmptyListing]): EmptyListing =
    if (listings.forall(_ == NoScheduleListed)) NoScheduleListed else Unvouched
}

/** Told the completeness of every listing the runner lands, venue by venue — what keeps a
 *  venue that is ALWAYS incomplete (whose stopped films are therefore never pruned) visible. */
trait ListingCompletenessRecorder {
  def landed(cinema: Cinema, completeness: ListingCompleteness): Unit
}

object ListingCompletenessRecorder {
  val none: ListingCompletenessRecorder = (_, _) => ()
}
