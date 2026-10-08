package tools

import services.venuepages.{VenuePage, VenuePageKey, VenuePageStore}

import java.util.concurrent.ConcurrentHashMap

/**
 * `inner`, each page's FIRST write landing but reported as failed — what `MongoVenuePageStore.put`
 * returns when the server applies the replace and its acknowledgement misses the 10 s wait, as it does
 * on a loaded Mongo (an `itAll` run: "venue_pages write failed for kino-pionier|…/ghost-in-the-shell:
 * Future timed out after [10 seconds]"). Every later write of the page is `inner`'s.
 */
final class UnacknowledgedVenuePageWrites(inner: VenuePageStore) extends ForwardingVenuePageStore(inner) {
  private val written = ConcurrentHashMap.newKeySet[VenuePageKey]()

  override def put(page: VenuePage): Boolean = inner.put(page) && !written.add(page.key)
}
