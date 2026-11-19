package controllers

import models.Country
import services.review.{DigestItem, LabelRow, ReviewAnswers, ReviewDigest}

import java.nio.file.Files

/** What the Agent-judged tab shows ([[AgentJudgedController]]): the takes awaiting the user's word
 *  ([[ReviewDigest.pending]]) of the country and kind asked for, their films named, in the agent's least sure first
 *  across every country; the labels file as read, the identity-judge-cards evidence, the open counts and every
 *  failed read. */
final class AgentJudgedTakes(answers: ReviewAnswers, labels: ReviewLabels, cards: AgentJudgedCards) {
  import AgentJudgedTakes.Taken

  def read(country: Option[String], kind: Option[String]): Taken = {
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
    Taken(labelsRead, items, judgeCards, (candidatesError ++ cardsError).toSeq, shownKind,
      (open.count(_.agentVerdict.isDefined), open.count(_.agentVerdict.isEmpty)))
  }
}

object AgentJudgedTakes {
  /** One read of the tab: `open` the (agent-judged, candidate) takes no answer stands for yet. */
  final case class Taken(labelsRead: (Seq[LabelRow], Option[String]), items: Seq[DigestItem], judgeCards: String,
                         errors: Seq[String], shownKind: Option[String], open: (Int, Int))
}
