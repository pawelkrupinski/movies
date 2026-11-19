package controllers

import play.api.Mode
import play.api.mvc._
import services.review.ReviewPage

/**
 * The dev-only Agent-judged review tab (`/debug/review/agent-judged`): every take awaiting the user's word
 * (`ReviewDigest.pending`, read by [[AgentJudgedTakes]]) -- the agents' judgements and the candidates they flagged -- as the review cards the other
 * tabs show: the listing's own facts, the film judged (TMDB's title, asked live, when the row files none), the films
 * the model weighed, and the agent's reason, the billing's other judgements and the identity-judge-cards evidence.
 * Answers are the other pages' ([[ReviewAnswerController]]). Production 404s every route here.
 */
class AgentJudgedController(cc: ControllerComponents, environment: Mode, takes: AgentJudgedTakes, reads: ReviewReads,
                            cards: AgentJudgedCards, pages: ReviewCardsPage) extends AbstractController(cc) {

  private def devOnly(result: => Result): Result = DevMode.gate(environment)(result)

  def agentJudged(country: Option[String], kind: Option[String], limit: Int, answered: Boolean): Action[AnyContent] = Action {
    devOnly {
      val taken = takes.read(country, kind)
      val (clusters, errors) = reads.clustersOf(taken.items)
      pages.render(ReviewPage.Digest, country, clusters.map { case (c, _) => c -> None }, limit, answered,
        errors ++ taken.errors, ReviewView.Controls.ofKind(taken.shownKind, taken.open), taken.labelsRead,
        cards.dressed(clusters, taken.labelsRead._1, taken.judgeCards))
    }
  }

  /** The Agent-judged tab's old address. */
  def digestMoved(country: Option[String]): Action[AnyContent] = Action {
    devOnly(Redirect(routes.AgentJudgedController.agentJudged(country, None, ReviewView.DefaultLimit, false)))
  }
}
