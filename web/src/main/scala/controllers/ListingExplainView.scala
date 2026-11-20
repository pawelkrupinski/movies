package controllers

import models.Country
import services.review.*

/** Everything `reviewListing.scala.html` renders: the query, each listing it names explained, or the listings near it. */
final case class ListingExplainView(countryOf: Country, venue: Option[String], title: String, explains: Seq[ListingExplain],
                                    near: Seq[models.ListingKey], notices: Seq[String]) {
  def country: String = countryOf.code
}
