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

  def of(structureComplete: Boolean, readsComplete: Boolean): ListingCompleteness =
    if (!structureComplete) ChunkIncomplete else if (!readsComplete) PageFailed else Complete
}

/** Told the completeness of every listing the runner lands, venue by venue — what keeps a
 *  venue that is ALWAYS incomplete (whose stopped films are therefore never pruned) visible. */
trait ListingCompletenessRecorder {
  def landed(cinema: Cinema, completeness: ListingCompleteness): Unit
}

object ListingCompletenessRecorder {
  val none: ListingCompletenessRecorder = (_, _) => ()
}
