package controllers

import services.identity.ResolverDecision
import services.review._

/** Everything `review.scala.html` renders. */
final case class ReviewView(page: ReviewPage, country: String, cards: Seq[ReviewCard], total: Int, answeredHidden: Int,
                            // every listed cluster with an answer, hidden or not — the progress bar's numerator
                            answered: Int, showAnswered: Boolean, limit: Int, controls: ReviewView.Controls, notices: Seq[String]) {
  /** This page's URL with one query parameter changed. */
  def link(changes: (String, Option[String])*): String = {
    val base = Seq("country" -> Some(country), "limit" -> Option.when(limit != ReviewView.DefaultLimit)(limit.toString),
      "answered" -> Option.when(showAnswered)("true"), "min" -> controls.min.map(_.toString),
      "basis" -> controls.basis.map(_.toString), "hours" -> controls.hours.map(_.toString), "kind" -> controls.kind)
    val merged = changes.foldLeft(base.toMap)((m, c) => m + c)
    val query  = base.map(_._1).flatMap(k => merged.get(k).flatten.map(v => s"$k=${java.net.URLEncoder.encode(v, "UTF-8")}"))
    ReviewView.pathOf(page) + (if (query.isEmpty) "" else query.mkString("?", "&", ""))
  }
}
object ReviewView {
  /** How many cards a review page shows unless asked for more: the routes' `limit` default. */
  val DefaultLimit = 20

  final case class Controls(min: Option[Double] = None, basis: Option[ResolverDecision.Basis] = None,
                            byBasis: Seq[(ResolverDecision.Basis, Int)] = Nil, hours: Option[Int] = None,
                            // the Agent-judged tab's: the kind of take shown (agent / candidate), and how many of each await
                            kind: Option[String] = None, kindCounts: Option[(Int, Int)] = None)

  object Controls {
    /** No controls: the queue's. */
    val none: Controls = Controls()

    /** The recent page's: how many hours back it looks. */
    def ofHours(hours: Int): Controls = Controls(hours = Some(hours))

    /** The Agent-judged tab's: the kind of take shown, and how many of each (agent, candidate) await. */
    def ofKind(kind: Option[String], counts: (Int, Int)): Controls = Controls(kind = kind, kindCounts = Some(counts))
  }

  /** The matchable page's clusters -- those of `clusters` rated matchable at `min` or above, of the basis `basis` names
   *  (every basis for none, or a name no basis has) -- and its controls: the floor, the basis, and how many clusters
   *  each basis rates matchable. */
  def matchable(clusters: Seq[ReviewCluster], min: Double, basis: Option[String]): (Seq[ReviewCluster], Controls) = {
    val chosen = basis.flatMap(b => ResolverDecision.Basis.values.find(_.toString == b))
    (ReviewSelection.matchable(clusters, min, chosen),
      Controls(min = Some(min), basis = chosen, byBasis = ReviewSelection.matchableByBasis(clusters, min)))
  }

  def pathOf(page: ReviewPage): String = page match {
    case ReviewPage.Queue     => "/debug/review"
    case ReviewPage.Matchable => "/debug/review/matchable"
    case ReviewPage.Recent    => "/debug/review/recent"
    case ReviewPage.Digest    => "/debug/review/agent-judged"
  }
}
