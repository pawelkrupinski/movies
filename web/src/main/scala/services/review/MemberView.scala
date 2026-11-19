package services.review

import models.ListingKey

/** One member listing of a card: its listing row, its own film page, and the catalogue ids its feed names it by.
 *  When the feed is an aggregator's that copies its catalogue entry's facts onto the listing
 *  ([[services.identity.SourceReliability.feedStated]] — Webedia's ids, kinoprogramm.com's pages), the listing row's
 *  year, directors, running time and poster are THAT CATALOGUE's claim, not the venue's, and are shown and weighed so. */
final case class MemberView(key: ListingKey, slot: Option[SlotFacts], page: Option[VenueFacts], feed: Option[ListingFeed]) {
  def venue: String = key.venue
  def pageUrl: Option[String] = key match { case ListingKey.Native(_, page, _) => Some(page); case _ => None }
  lazy val factsFromCatalogue: Boolean =
    services.identity.SourceReliability.feedStated(feed.toSeq.flatMap(_.catalogueIds), pageUrl)
  /** The year and directors the VENUE stated — its key's, else its listing row's, else its own page's — and none
   *  when its listing's facts are a feed catalogue's: those are no venue's statement to check an answer against. */
  lazy val member: ReviewMember = {
    val base = ReviewMember.of(key)
    if (factsFromCatalogue) base.copy(year = None, directors = Nil)
    else {
      val own = slot.map(_.facts).toSeq ++ page.toSeq
      base.copy(year = base.year.orElse(own.flatMap(_.year).headOption),
        directors = if (base.directors.nonEmpty) base.directors else own.map(_.directors).find(_.nonEmpty).getOrElse(Nil))
    }
  }
  /** The venue's poster: its listing row's, else its last scrape's listing (`identity_listings`), else its own film page's —
   *  never a site-wide default image ([[tools.SiteDefaultImage]]) taken for one. */
  def poster: Option[String]   =
    (slot.flatMap(_.facts.poster) ++ feed.flatMap(_.poster) ++ page.flatMap(_.poster)).find(p => !tools.SiteDefaultImage(p))
  def synopsis: Option[String] = slot.flatMap(_.facts.synopsis).orElse(page.flatMap(_.synopsis))
}
