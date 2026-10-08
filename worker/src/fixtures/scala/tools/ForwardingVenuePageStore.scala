package tools

import services.cinemas.common.FilmDetail
import services.venuepages.{StoredVenuePage, VenuePage, VenuePageKey, VenuePageStore}

/** `inner`, every call forwarded: a test store overrides only the calls it changes. */
abstract class ForwardingVenuePageStore(inner: VenuePageStore) extends VenuePageStore {
  def get(key: VenuePageKey): Option[VenuePage]                 = inner.get(key)
  def put(page: VenuePage): Boolean                             = inner.put(page)
  def foreach(onPage: VenuePage => Unit): ScanOutcome           = inner.foreach(onPage)
  def stored(key: VenuePageKey): StoredVenuePage                = inner.stored(key)
  def land(key: VenuePageKey, detail: FilmDetail): Boolean      = inner.land(key, detail)
}
