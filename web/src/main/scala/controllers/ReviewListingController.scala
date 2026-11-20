package controllers

import scala.util.{Failure, Success}

import models.Country
import play.api.Mode
import play.api.mvc.*

/** One production listing explained (`/debug/review/listing`, [[services.review.ListingExplain]]): its take, where its take history is,
 *  the latest decision trace, its judgements and its cluster on the review pages -- the listings `venue` (every venue
 *  when left out) bills as `title` exactly, else those whose title contains it to pick from. Production 404s it. */
class ReviewListingController(cc: ControllerComponents, environment: Mode, reads: ReviewListingReads, labels: ReviewLabels,
                              // Why the page may be empty (no mirror, no labels file), for its banner.
                              notices: Seq[String]) extends AbstractController(cc) {

  def listing(country: String, venue: Option[String], title: String): Action[AnyContent] = Action {
    DevMode.gate(environment) {
      val wantedVenue = venue.map(_.trim).filter(_.nonEmpty)
      Country.byCode(country).flatMap(c => reads.listingsBilled(c, wantedVenue, title).map(c -> _)) match {
        case None    => NotFound(s"no mirror holds country $country")
        case Some((c, billed)) =>
          billed match {
            case Failure(e)            => InternalServerError(s"could not read identity_model_families: ${e.getMessage}")
            case Success((keys, near)) =>
              val labelsRead = labels.read()
              val explains = keys.map(reads.explained(c, _, labelsRead._1))
              val said     = Option.when(title.nonEmpty && keys.isEmpty && near.isEmpty)(s"no listing in ${c.code} is billed as “$title”")
              Ok(views.html.reviewListing(ListingExplainView(c, wantedVenue, title, explains, near, said.toSeq ++ labelsRead._2 ++ notices)))
                .withHeaders("Content-Security-Policy" -> controllers.CspFilter.WithGoogleFonts)
          }
      }
    }
  }

  /** One review card's "Why" fold-out (`/debug/review/why`), read when it is opened: each member listing's trace --
   *  the evidence for and against the film it weighed, every rule's refusal, the candidates it scored and what it
   *  searched. */
  def why(country: String, cluster: String): Action[AnyContent] = Action {
    DevMode.gate(environment) {
      Country.byCode(country).flatMap(reads.why(_, cluster)) match {
        case None                => NotFound(s"no cluster $cluster in $country")
        case Some(Success(html)) => Ok(html)
        case Some(Failure(e))    => InternalServerError(s"could not read identity_traces: ${e.getMessage}")
      }
    }
  }
}
