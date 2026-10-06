package services.review

import models.Country
import services.identity.ResolverDecision
import services.movies.ListingKey

import java.time.Instant

/** A film the resolver weighed for a cluster: its probability where the explanation states one, and
 *  whether a member's own evidence denied it (and why). */
final case class ReviewCandidate(film: Int, probability: Option[Double], vetoed: Boolean = false, denial: Option[String] = None)

/**
 * One cluster of the identity model (`identity_model_families.decisions[]`) as the review pages read it.
 * `candidates` are the films the decision names, best first: the best rejected or vetoed candidate of
 * its pooled scoring, the members' own matches and their pooled vote, and the `candidate` / `leaning` it stored.
 */
final case class ReviewCluster(country: Country, members: Seq[ListingKey], film: Option[Int], confidence: Double,
                               basis: ResolverDecision.Basis, explanation: Seq[String], fallback: Boolean,
                               candidates: Seq[ReviewCandidate], reasons: ReviewReasons = ReviewReasons()) {
  lazy val reviewMembers: Seq[ReviewMember] = members.map(ReviewMember.of)
  lazy val id: String = ReviewClusterId.of(reviewMembers)
  def title: String = members.headOption.fold("")(_.rawTitle)
  def best: Option[ReviewCandidate] = candidates.headOption
  /** The best candidate's probability, 0 when none is stated. */
  def bestProbability: Double = best.flatMap(_.probability).getOrElse(0.0)
  def unmatched: Boolean = film.isEmpty && !fallback
  /** The film the card puts forward: the match, else the best candidate. */
  def shown: Option[Int] = film.orElse(best.map(_.film))
}

object ReviewCluster {
  // ResolverDecisions writes: "best vetoed candidate 1703622 at 62.5%, denied: <why>, (<measures>)"
  //                       and "best rejected candidate 677558 at 86.4% (<measures>)"
  private val Best = """best (vetoed|rejected) candidate (\d+) at ([\d.]+)%(?:, denied: (.*?),)? \(""".r.unanchored
  private val Own    = """own match (\d+) at ([\d.]+)%""".r.unanchored
  // "pooled evidence of 3 node(s) → 677558 at 86.4%"
  private val Pooled = """pooled evidence of \d+ node\(s\) → (\d+) at ([\d.]+)%""".r.unanchored
  private def fraction(pct: String): Option[Double] = pct.toDoubleOption.map(p => (BigDecimal(p) / 100).toDouble)

  def of(country: Country, decision: ResolverDecision): ReviewCluster = {
    val best = decision.explanation.collectFirst { case Best(kind, film, pct, denial) =>
      ReviewCandidate(film.toInt, fraction(pct), vetoed = kind == "vetoed", Option(denial)) }
    val own = decision.explanation.collect {
      case Own(film, pct)    => ReviewCandidate(film.toInt, fraction(pct))
      case Pooled(film, pct) => ReviewCandidate(film.toInt, fraction(pct))
    }
    val stored = (decision.candidate.toSeq ++ decision.leaning.toSeq).map(lean => ReviewCandidate(lean.film, None))
    val candidates = (best.toSeq ++ own.sortBy(-_.probability.getOrElse(0.0)) ++ stored)
      .filterNot(c => decision.film.contains(c.film)).distinctBy(_.film)
    ReviewCluster(country, decision.members, decision.film, decision.confidence, decision.basis, decision.explanation,
      decision.fallback.isDefined, candidates, ReviewReasons.of(decision))
  }
}

/** Which clusters each review page lists, and in what order. Pure: the pages and their tests share it. */
object ReviewSelection {

  /** Unmatched clusters with no answer yet, the most confident candidate first. */
  def queue(clusters: Seq[ReviewCluster]): Seq[ReviewCluster] =
    clusters.filter(_.unmatched).sortBy(c => (-c.bestProbability, c.title))

  /** Unmatched clusters whose best candidate the resolver itself rated at `min` or more — held back
   *  below the line (BelowThreshold), or vetoed by a guard (Vetoed) — optionally of one basis. */
  def matchable(clusters: Seq[ReviewCluster], min: Double, basis: Option[ResolverDecision.Basis]): Seq[ReviewCluster] =
    clusters.filter(c => c.unmatched && c.best.exists(_.probability.exists(_ >= min)) && basis.forall(_ == c.basis))
      .sortBy(c => (-c.bestProbability, c.title))

  /** How many clusters each basis would list at `min`. */
  def matchableByBasis(clusters: Seq[ReviewCluster], min: Double): Seq[(ResolverDecision.Basis, Int)] =
    matchable(clusters, min, None).groupBy(_.basis).view.mapValues(_.size).toSeq.sortBy(-_._2)

  /** Matched clusters a listing of which was (re)written since `since`, least confident first. The model
   *  stores no decision time: a member's `movie_slots.updatedAt` stands in for it. */
  def recent(clusters: Seq[ReviewCluster], updatedAt: Map[String, Instant], since: Instant): Seq[(ReviewCluster, Instant)] =
    clusters.filter(_.film.isDefined).flatMap { c =>
      c.members.flatMap(m => updatedAt.get(ListingKey.serialised(m))).maxOption.filterNot(_.isBefore(since)).map(c -> _)
    }.sortBy { case (c, at) => (c.confidence, -at.toEpochMilli) }
}
