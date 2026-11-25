package services.identityadapters

import org.bson.BsonDocument
import services.venuepages.{MongoVenuePageStore, VenuePage}

/** A venue page's answers in the identity history (`answer:venue_pages`, by page id), as `venue_pages` holds them:
 *  what [[HistoryVenuePageStore]] appends and a replay reads back ([[IdentityHistoryReplay]]). */
object VenuePageAnswerEntries {
  val Kind: String = s"answer:${MongoVenuePageStore.Collection}"

  /** The page as `venue_pages` holds it: what a replay's captured store decodes (`CapturedVenuePages`). */
  def docOf(page: VenuePage): BsonDocument = MongoVenuePageStore.documentOf(page).toBsonDocument

  /** `page` appended to `history` at `at` when its answer moved from `before` (the outcome last kept of it, `None` when
   *  none is): a re-read stating the same is no entry. */
  def appendMoved(history: IdentityHistory, before: Option[VenuePage], page: VenuePage, at: Long, build: String): Unit =
    if (!before.map(_.outcome).contains(page.outcome))
      history.append(Seq(HistoryEntry(Kind, page.key.id, at, before.map(docOf), Some(docOf(page)), build, beforeUnknown = before.isEmpty)))
}
