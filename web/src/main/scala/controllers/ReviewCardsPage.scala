package controllers

import play.api.mvc.{Result, Results}
import services.review.{LabelRow, ReviewAnswers, ReviewCard, ReviewCluster, ReviewPage}

import java.time.Instant

/**
 * A review page of cards (`review.scala.html`): the clusters selected, the answered ones hidden unless asked for,
 * `limit` of them as cards, with the answers' progress and every notice -- the failed reads, the labels file's and
 * `notices`, why the pages may be empty or answers not kept (no mirror, answers in memory).
 */
final class ReviewCardsPage(answers: ReviewAnswers, reads: ReviewReads, notices: Seq[String]) {

  def render(page: ReviewPage, country: Option[String], selected: Seq[(ReviewCluster, Option[Instant])], limit: Int,
             showAnswered: Boolean, errors: Seq[String], controls: ReviewView.Controls, labelsRead: (Seq[LabelRow], Option[String]),
             // what the page adds to each card: the queue's rank, the Agent-judged tab's take
             dress: ReviewCard => ReviewCard): Result = {
    val index    = new ReviewAnswers.Index(answers.current())
    val (done, notDone) = selected.partition { case (c, _) => index.answerFor(c).isDefined }
    val open     = if (showAnswered) selected else notDone
    val shown    = open.take(limit)
    val (labels, labelsError) = labelsRead
    val cards    = reads.cards(shown, labels, index).map(dress)
    val order    = shown.map(_._1.id).zipWithIndex.toMap
    val view     = ReviewView(page, country.getOrElse("all"), cards.sortBy(card => order(card.cluster.id)), total = open.size,
      answeredHidden = selected.size - open.size, done.size, showAnswered, limit, controls, errors ++ labelsError ++ notices)
    Results.Ok(views.html.review(view)).withHeaders("Content-Security-Policy" -> controllers.CspFilter.WithGoogleFonts)
  }
}
