package controllers

import java.time.{Clock, Instant}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.Try

import services.review.{JudgedListings, LabelRow, ReviewCard, ReviewCluster, ReviewSelection}

/** The clusters each dev-only review queue selects ([[ReviewQueueController]]): the unmatched ranked by what an
 *  answer settles, those the resolver rated matchable and still left, and those it matched lately. */
final class ReviewQueues(reads: ReviewReads, labels: ReviewLabels, clock: Clock) {
  import ReviewQueues.Selected

  /** Each country's clusters ranked against what it serves, the countries side by side, then merged by the same order. */
  def queue(country: Option[String]): Selected = {
    val labelsRead = labels.read()
    val judged     = new JudgedListings(labelsRead._1)
    val (chosen, errors) = reads.chosenOf(reads.countriesOf(country), unmatchedOnly = false)(c =>
      ReviewSelection.queue(_, reads.sourceOf(c).served(), judged))
    val ranked = chosen.sortBy { case (c, p) => (-p.score, -p.listings, c.title) }
    val priorities = ranked.map { case (c, p) => c.id -> p }.toMap
    Selected(ranked.map(_._1 -> None), errors, ReviewView.Controls.none, labelsRead,
      card => card.withPriority(priorities.get(card.cluster.id)))
  }

  def matchable(country: Option[String], min: Double, basis: Option[String]): Selected = {
    val (clusters, errors) = reads.clustersOf(reads.countriesOf(country), unmatchedOnly = true)
    val (matchable, controls) = ReviewView.matchable(clusters, min, basis)
    Selected(matchable.map(_ -> None), errors, controls, labels.read(), identity)
  }

  def recent(country: Option[String], hours: Int): Selected = {
    val countries = reads.countriesOf(country)
    val since     = clock.instant().minusSeconds(hours.toLong * 3600)
    // the slot times read while the decisions are: neither waits on the other
    val updating  = countries.map(c => c -> Future(blocking(Try(reads.sourceOf(c).updatedSince(since))))(using ExecutionContext.global)).toMap
    // each country's clusters chosen by its own slot times, the countries side by side: no merged corpus is built
    val (chosen, errors) = reads.chosenOf(countries, unmatchedOnly = false) { c => clusters =>
      ReviewSelection.recent(clusters, Await.result(updating(c), Duration.Inf).getOrElse(Map.empty), since)
    }
    Selected(ReviewSelection.byConfidence(chosen).map { case (c, at) => c -> Some(at) }, errors,
      ReviewView.Controls.ofHours(hours), labels.read(), identity)
  }
}

object ReviewQueues {
  /** What a queue selected, the reads that failed, the page's controls, the labels file as read, and what the page adds
   *  to each card. */
  final case class Selected(clusters: Seq[(ReviewCluster, Option[Instant])], errors: Seq[String], controls: ReviewView.Controls,
                            labelsRead: (Seq[LabelRow], Option[String]), dress: ReviewCard => ReviewCard)
}
