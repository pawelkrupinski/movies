package controllers

import scala.util.Try

import models.{Country, ListingKey}
import services.review.{LabelRow, ListingExplain}

/** What the dev-only review pages read of one cluster or listing of a country ([[ReviewReads]]' sources): a card's
 *  "Why" fold-out, the listings a title is billed as, and one listing explained. */
final class ReviewListingReads(reads: ReviewReads) {

  /** The "Why" fold-out of `country`'s cluster `id` (`reviewWhy.scala.html`): each member listing with its latest
   *  decision trace, none for a listing without one. None for no such cluster (none in a country no mirror holds); a
   *  failed trace read, said. */
  def why(country: Country, id: String): Option[Try[play.twirl.api.Html]] =
    reads.cluster(country, id).map(found => Try(reads.sourceOf(country).traces(found.members.map(ListingKey.serialised))).map(traces =>
      views.html.reviewWhy(found.members.map(k => k -> traces.get(ListingKey.serialised(k))), Some(country))))

  /** The listings of `country` that `venue` (every venue when none) bills as `title` exactly, and when there are none
   *  the listings whose title contains it, to pick from ([[ListingExplain.find]], [[ListingExplain.near]]). None when
   *  no mirror holds `country`. */
  def listingsBilled(country: Country, venue: Option[String], title: String): Option[Try[(Seq[ListingKey], Seq[ListingKey])]] =
    Option.when(reads.holds(country))(Try(reads.sourceOf(country).decisions(unmatchedOnly = false)).map { decisions =>
      val keys = if (title.isEmpty) Nil else ListingExplain.find(decisions, venue, title)
      (keys, if (keys.isEmpty && title.nonEmpty) ListingExplain.near(decisions, venue, title) else Nil)
    })

  /** One listing of `country` explained ([[ListingExplain.build]]) against the labels file's `labels`. */
  def explained(country: Country, listing: ListingKey, labels: Seq[LabelRow]): ListingExplain =
    ListingExplain.build(country, listing, reads.sourceOf(country), labels)
}
