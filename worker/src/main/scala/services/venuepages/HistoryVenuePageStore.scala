package services.venuepages

import org.bson.BsonDocument
import services.cinemas.common.FilmDetail
import services.identity.adapters.{HistoryEntry, IdentityHistory}

/** `inner`, every page whose ANSWER a read moved — its detail, or its being gone — appended to `history` as `venue_pages`
 *  holds it (`answer:venue_pages`, by page id): what a listing's venue detail said at any moment, for a replay. A re-read
 *  stating the same is no entry, its new `readAt` aside — told by the outcome last kept here, never by a read of the
 *  store: a write costs no read for the history (`before` is the outcome kept here, when it is). Bounded: a page
 *  forgotten is kept once more at its next read, no more. */
final class HistoryVenuePageStore(inner: VenuePageStore, history: IdentityHistory, clock: java.time.Clock, build: String) extends VenuePageStore {
  def get(key: VenuePageKey): Option[VenuePage]         = inner.get(key)
  def stored(key: VenuePageKey): StoredVenuePage        = inner.stored(key)
  def land(key: VenuePageKey, detail: FilmDetail): Boolean = inner.land(key, detail)
  def foreach(onPage: VenuePage => Unit): tools.ScanOutcome = inner.foreach(onPage)

  private val last: com.github.benmanes.caffeine.cache.Cache[String, VenuePage] =
    tools.BoundedCache.ofSize(HistoryVenuePageStore.PagesHeld).build[String, VenuePage]()

  def put(page: VenuePage): Boolean = {
    val kept = inner.put(page)
    if (kept && (history ne IdentityHistory.Discard)) {
      val before = Option(last.asMap.put(page.key.id, page))
      if (!before.map(_.outcome).contains(page.outcome))
        history.append(Seq(HistoryEntry(HistoryVenuePageStore.Kind, page.key.id, clock.millis(), before.map(HistoryVenuePageStore.docOf),
          Some(HistoryVenuePageStore.docOf(page)), build, beforeUnknown = before.isEmpty)))
    }
    kept
  }
}

object HistoryVenuePageStore {
  /** How many pages' last outcomes are remembered to tell a re-read alike. */
  val PagesHeld = 100000
  val Kind: String = s"answer:${MongoVenuePageStore.Collection}"
  /** The page as `venue_pages` holds it: what a replay's captured store decodes (`CapturedVenuePages`). */
  def docOf(page: VenuePage): BsonDocument = MongoVenuePageStore.documentOf(page).toBsonDocument
}
