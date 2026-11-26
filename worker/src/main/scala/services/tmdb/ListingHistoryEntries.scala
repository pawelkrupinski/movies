package services.tmdb

import models.ListingKey
import services.identity.ListingBson
import services.identity.measures.Listing

/** What the model's drain keeps in the identity history of the listings it takes in and lets go
 *  ([[IdentityModelService]], [[HistoryEntry.Listing]]): what a replay of a moment's takes resolves. */
object ListingHistoryEntries {
  /** `seen` and `gone`, as they move what the model holds (`held`), as entries at `at` by build `build`: a listing new to
   *  it, held otherwise -- the model's fields or its poster -- or let go. Its screening days alone moving is no entry: a
   *  scrape adds days daily, and the model reads only their season. */
  def of(held: ListingKey => Option[Listing], seen: Seq[Listing], gone: Seq[ListingKey], at: Long, build: String): Seq[HistoryEntry] = {
    def key(k: ListingKey) = ListingKey.serialised(k)
    val moved = seen.flatMap { listing =>
      val was = held(listing.key)
      Option.when(!was.contains(listing) || was.exists(_.poster != listing.poster))(
        HistoryEntry(HistoryEntry.Listing, key(listing.key), at, was.map(ListingBson.doc), Some(ListingBson.doc(listing)), build))
    }
    val left = gone.flatMap(k => held(k).map(was => HistoryEntry(HistoryEntry.Listing, key(k), at, Some(ListingBson.doc(was)), None, build)))
    moved ++ left
  }
}
