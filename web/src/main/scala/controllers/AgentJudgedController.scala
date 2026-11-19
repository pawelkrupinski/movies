package controllers

import models.Country
import play.api.Mode
import play.api.mvc._
import services.review.{DigestItem, ReviewAnswers, ReviewDigest, ReviewPage}

import java.nio.file.Files

/**
 * The dev-only Agent-judged review tab (`/debug/review/agent-judged`): every take awaiting the user's word
 * ([[ReviewDigest.pending]]) -- the agents' judgements and the candidates they flagged -- as the review cards the other
 * tabs show: the listing's own facts, the film judged (TMDB's title, asked live, when the row files none), the films
 * the model weighed, and the agent's reason, the billing's other judgements and the identity-judge-cards evidence.
 * Answers are the other pages' ([[ReviewAnswerController]]). Production 404s every route here.
 */
class AgentJudgedController(cc: ControllerComponents, environment: Mode, answers: ReviewAnswers, labels: ReviewLabels,
                            reads: ReviewReads, cards: AgentJudgedCards, pages: ReviewCardsPage) extends AbstractController(cc) {

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  def agentJudged(country: Option[String], kind: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val labelsRead = labels.read()
      val candidatesPath = ReviewDigest.candidatesPath(labels.path)
      val (candidates, candidatesError) =
        ReviewLabels.readReported(candidatesPath, Seq.empty[DigestItem])(ReviewDigest.readCandidates(candidatesPath))
      val cardsPath = ReviewDigest.judgeCardsPath(labels.path)
      val (judgeCards, cardsError) =
        if (!Files.exists(cardsPath)) ("", None) else ReviewLabels.readReported(cardsPath, "")(Files.readString(cardsPath))
      val wanted    = country.filterNot(_ == "all").flatMap(Country.byCode).map(_.code)
      val awaiting  = ReviewDigest.pending(labelsRead._1, candidates).filter(i => wanted.forall(_ == i.country))
      // what the counts count: the takes no answer stands for yet (one the labels file has not taken still hides its card),
      // each looked up by its own key alone, as its card is: another tab's answer of its listing settles another film
      val index     = new ReviewAnswers.Index(answers.current())
      val open      = awaiting.filterNot(i => index.answerFor(i.id, Nil).isDefined)
      val shownKind = kind.filter(Set(ReviewDigest.Kinds.Agent, ReviewDigest.Kinds.Candidate))
      val countryOrder = Country.all.map(_.code).zipWithIndex.toMap
      // the agent's least sure first across every country, then the countries in their order (pending's own order within)
      val items     = cards.nameFilms(ReviewDigest.ofKind(awaiting, shownKind)).sortBy(i => (i.doubt.rank, countryOrder.getOrElse(i.country, Int.MaxValue)))
      val (clusters, errors) = reads.clustersOf(items)
      pages.render(ReviewPage.Digest, country, clusters.map { case (c, _) => c -> None }, limit, answered,
        errors ++ candidatesError ++ cardsError, ReviewView.Controls.ofKind(shownKind,
          (open.count(_.agentVerdict.isDefined), open.count(_.agentVerdict.isEmpty))), labelsRead,
        cards.dressed(clusters, labelsRead._1, judgeCards))
    }
  }

  /** The Agent-judged tab's old address. */
  def digestMoved(country: Option[String]): Action[AnyContent] = Action {
    devOnly(Redirect(routes.AgentJudgedController.agentJudged(country, None, ReviewView.DefaultLimit, false)))
  }
}
