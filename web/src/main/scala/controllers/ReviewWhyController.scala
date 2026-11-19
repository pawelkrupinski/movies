package controllers

import models.Country
import play.api.Mode
import play.api.mvc._

import scala.util.{Failure, Success}

/** One review card's "Why" fold-out (`/debug/review/why`), read when it is opened: each member listing's trace -- the
 *  evidence for and against the film it weighed, every rule's refusal, the candidates it scored and what it searched.
 *  Production 404s it, like the rest of `/debug`. */
class ReviewWhyController(cc: ControllerComponents, environment: Mode, reads: ReviewListingReads) extends AbstractController(cc) {

  def why(country: String, cluster: String): Action[AnyContent] = Action {
    DevMode.gate(environment) {
      Country.byCode(country).filter(reads.holds).flatMap(reads.why(_, cluster)) match {
        case None                => NotFound(s"no cluster $cluster in $country")
        case Some(Success(html)) => Ok(html)
        case Some(Failure(e))    => InternalServerError(s"could not read identity_traces: ${e.getMessage}")
      }
    }
  }
}
