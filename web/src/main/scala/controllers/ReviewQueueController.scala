package controllers

import play.api.Mode
import play.api.mvc.*
import services.review.ReviewPage

/**
 * The dev-only identity review queues (`/debug/review`, `/matchable`, `/recent`): the clusters the resolver left
 * unmatched ranked by what an answer settles, those it rated matchable and still left, and those it matched lately --
 * each a card with the venues' own facts, the films it weighed, its explanation, and the answer buttons
 * ([[ReviewAnswerController]] keeps the answers). What each queue selects is [[ReviewQueues]]'. Production 404s every route here, like the rest of `/debug`.
 */
class ReviewQueueController(cc: ControllerComponents, environment: Mode, queues: ReviewQueues, pages: ReviewCardsPage)
  extends AbstractController(cc) {

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  private def render(page: ReviewPage, country: Option[String], selected: ReviewQueues.Selected, limit: Int, answered: Boolean): Result =
    pages.render(page, country, selected.clusters, limit, answered, selected.errors, selected.controls, selected.labelsRead,
      selected.dress)

  def queue(country: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly(render(ReviewPage.Queue, country, queues.queue(country), limit, answered))
  }

  def matchable(country: Option[String], min: Double, basis: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly(render(ReviewPage.Matchable, country, queues.matchable(country, min, basis), limit, answered))
  }

  def recent(country: Option[String], hours: Int, limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly(render(ReviewPage.Recent, country, queues.recent(country, hours), limit, answered))
  }
}
