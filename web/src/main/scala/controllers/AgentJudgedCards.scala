package controllers

import models.Country
import services.review.{DigestItem, LabelRow, ReviewCard, ReviewCluster, ReviewDigest, TmdbPosterLookup}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}

/** The Agent-judged tab's cards: each take awaiting the user's word ([[ReviewDigest.pending]]) as the cluster of its
 *  listing, its film named as TMDB names it where the row files no title (asked live through `tmdbPosters`; none
 *  without a TMDB key), and each card dressed with the take, the billing's other judgements, the judge-cards evidence
 *  and the film judged. */
final class AgentJudgedCards(tmdbPosters: Option[TmdbPosterLookup]) {

  /** A film the row files no title for, named as TMDB names it in the country's language, the films asked side by side. */
  def nameFilms(pending: Seq[DigestItem]): Seq[DigestItem] = {
    val unnamed = pending.filter(_.filmTitle.isEmpty).flatMap(i => i.film.tmdb.map(_ -> i.country)).distinct
    val looked  = tmdbPosters.fold(Map.empty[(Int, String), String]) { lookup =>
      unnamed.map { case key @ (id, cc) =>
        key -> Future(blocking(lookup.film(id, Country.byCode(cc).fold(java.util.Locale.UK)(_.language))))(using ExecutionContext.global)
      }.flatMap { case (key, film) => Await.result(film, Duration.Inf).flatMap(_.describe).map(key -> _) }.toMap
    }
    pending.map(i => i.titled(i.film.tmdb.flatMap(id => looked.get(id -> i.country))))
  }

  /** A digest card: the take awaiting the user, the billing's other judgements, its judge-cards evidence and its film. */
  def dressed(clusters: Seq[(ReviewCluster, DigestItem)], labels: Seq[LabelRow], cards: String): ReviewCard => ReviewCard = {
    val byId = clusters.map { case (cluster, item) => cluster.id -> item }.toMap
    card => byId.get(card.cluster.id).fold(card)(item =>
      card.awaiting(item, ReviewDigest.otherJudgements(labels, item), Option(ReviewDigest.judgeCard(cards, item)).filter(_.nonEmpty)))
  }
}
