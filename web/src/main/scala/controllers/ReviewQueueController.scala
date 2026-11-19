package controllers

import play.api.Mode
import play.api.mvc._
import services.review.{JudgedListings, ReviewPage, ReviewSelection}

import java.time.Clock
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.Try

/**
 * The dev-only identity review queues (`/debug/review`, `/matchable`, `/recent`): the clusters the resolver left
 * unmatched ranked by what an answer settles, those it rated matchable and still left, and those it matched lately --
 * each a card with the venues' own facts, the films it weighed, its explanation, and the answer buttons
 * ([[ReviewAnswerController]] keeps the answers). Production 404s every route here, like the rest of `/debug`.
 */
class ReviewQueueController(cc: ControllerComponents, environment: Mode, reads: ReviewReads, labels: ReviewLabels,
                            pages: ReviewCardsPage, clock: Clock) extends AbstractController(cc) {

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  def queue(country: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val labelsRead = labels.read()
      val judged     = new JudgedListings(labelsRead._1)
      // each country's clusters ranked against what it serves, the countries side by side, then merged by the same order
      val (chosen, errors) = reads.chosenOf(reads.countriesOf(country), unmatchedOnly = false)(c =>
        ReviewSelection.queue(_, reads.sourceOf(c).served(), judged))
      val ranked = chosen.sortBy { case (c, p) => (-p.score, -p.listings, c.title) }
      pages.render(ReviewPage.Queue, country, ranked.map(_._1 -> None), limit, answered, errors, ReviewView.Controls.none, labelsRead, {
        val priorities = ranked.map { case (c, p) => c.id -> p }.toMap
        card => card.withPriority(priorities.get(card.cluster.id))
      })
    }
  }

  def matchable(country: Option[String], min: Double, basis: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val (clusters, errors) = reads.clustersOf(reads.countriesOf(country), unmatchedOnly = true)
      val (matchable, controls) = ReviewView.matchable(clusters, min, basis)
      pages.render(ReviewPage.Matchable, country, matchable.map(_ -> None), limit, answered, errors, controls, labels.read(), identity)
    }
  }

  def recent(country: Option[String], hours: Int, limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val countries = reads.countriesOf(country)
      val since     = clock.instant().minusSeconds(hours.toLong * 3600)
      // the slot times read while the decisions are: neither waits on the other
      val updating  = countries.map(c => c -> Future(blocking(Try(reads.sourceOf(c).updatedSince(since))))(using ExecutionContext.global)).toMap
      // each country's clusters chosen by its own slot times, the countries side by side: no merged corpus is built
      val (chosen, errors) = reads.chosenOf(countries, unmatchedOnly = false) { c => clusters =>
        ReviewSelection.recent(clusters, Await.result(updating(c), Duration.Inf).getOrElse(Map.empty), since)
      }
      pages.render(ReviewPage.Recent, country, ReviewSelection.byConfidence(chosen).map { case (c, at) => c -> Some(at) },
        limit, answered, errors, ReviewView.Controls.ofHours(hours), labels.read(), identity)
    }
  }
}
