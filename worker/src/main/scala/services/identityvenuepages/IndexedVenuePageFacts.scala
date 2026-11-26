package services.identityvenuepages

import services.cinemas.common.DetailEnricher

/**
 * venue_pages as a venue slot is built from it ([[services.movies.VenuePageFacts]]): the index's answer for the listing's
 * page, landed as the detail enrichment lands it on the slot (`FilmDetail.landed`) — never fetched, and only for a venue
 * whose detail lands on its own slot (a chain's lands on its network source, which no listing builds).
 */
final class IndexedVenuePageFacts(enrichers: Seq[DetailEnricher], index: VenuePageIndex, enrichmentLanguage: java.util.Locale,
                                  feed: services.identity.agreement.AnswerFeed)
    extends services.movies.VenuePageFacts {
  private val ownSlot: Map[models.Cinema, DetailEnricher] =
    enrichers.iterator.filter(e => e.detailTarget == e.cinema).map(e => e.cinema -> e).toMap

  private val cinemasOf: Map[String, Seq[models.Cinema]] = ownSlot.toSeq.groupMap(_._2.detailGroup)(_._1)

  def of(cinema: models.Cinema, page: String): Option[models.SourceData] =
    ownSlot.get(cinema).flatMap(index.answer(_, page)).flatten.map(_.landed(enrichmentLanguage).pageFields)

  /** The venue pages' filings on the ONE feed every input's change comes on ([[VenuePageIndex]] files there each page
   *  whose answer a settle moved, as the model and the agreement stage read their own): the projection's view of them
   *  (docs/design/identity-no-drift.md, "One dependency index"). */
  private val filings = feed.view(key => Option.when(VenuePageIndex.isKey(key))(key))
  private var takenAt = filings.version

  /** Each venue page whose answer moved since the last call, as the venue and the page a listing's key names
   *  (`ListingKey.Native`): a slot built from it is built otherwise now, though nothing of its listing moved. None when
   *  the feed no longer holds every filing since the last call: then any page may have moved. */
  def takeMoved(): Option[Seq[(models.Cinema, String)]] = synchronized {
    val at    = filings.version
    val since = filings.changedSince(takenAt)
    takenAt = at   // a filing after `at` is in `since` and comes again next call: drafted twice, never missed
    since.map(_.toSeq.sorted.flatMap(key => cinemasOf.iterator.flatMap { case (group, cinemas) =>
      VenuePageIndex.pageOf(key, group).iterator.flatMap(page => cinemas.map(_ -> page)) }))
  }
}
