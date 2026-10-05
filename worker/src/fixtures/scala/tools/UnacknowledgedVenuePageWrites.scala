package tools

import services.venuepages.{VenuePage, VenuePageKey, VenuePageStore}

import java.util.concurrent.ConcurrentHashMap

/**
 * `inner`, each page's FIRST write landing but reported as failed — what `MongoVenuePageStore.put`
 * returns when the server applies the replace and its acknowledgement misses the 10 s wait, as it does
 * on a loaded Mongo (an `itAll` run: "venue_pages write failed for kino-pionier|…/ghost-in-the-shell:
 * Future timed out after [10 seconds]"). Every later write of the page is `inner`'s.
 */
final class UnacknowledgedVenuePageWrites(inner: VenuePageStore) extends VenuePageStore {
  private val written = ConcurrentHashMap.newKeySet[VenuePageKey]()

  def get(key: VenuePageKey): Option[VenuePage] = inner.get(key)
  def put(page: VenuePage): Boolean = inner.put(page) && !written.add(page.key)
  def foreach(onPage: VenuePage => Unit): ScanOutcome = inner.foreach(onPage)
}
