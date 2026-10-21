package services.review

import services.identity.ResolverDecision
import services.movies.ListingKey

/**
 * What one answer on a cluster is worth: how many listings it settles, and how unsure the resolver is of them. The
 * queue lists the clusters an answer settles the most listings of first ([[score]]).
 *
 * @param listings    the cluster's listings no label settles yet ([[JudgedListings]]): an answer labels each of them
 * @param judged      the cluster's listings a label already settles
 * @param uncertainty the probability the resolver's verdict on the cluster is wrong: 1 − its confidence in a take, the
 *                    best candidate's probability for a no-match (the film it held back, below the cut or vetoed); at
 *                    least ½ where what prod serves a listing as disagrees with the model's decision ([[served]])
 * @param served      the films prod serves the listings as where that is not the cluster's decision (`None`: no film)
 */
final case class ReviewPriority(listings: Int, judged: Int, uncertainty: Double, served: Seq[Option[Int]], basis: ResolverDecision.Basis,
                                confidence: Double, film: Option[Int], best: Option[ReviewCandidate]) {
  /** The listings an answer settles, weighted by how likely the resolver is wrong on them: the queue's order. */
  def score: Double = listings * uncertainty
  /** Prod serves a listing of the cluster as another film, or as none, than the model decided. */
  def stagesDisagree: Boolean = served.nonEmpty

  /** Why the cluster is uncertain, in a line each, for the card. */
  def reasons: Seq[String] = {
    def pct(p: Double) = f"${p * 100}%.0f%%"
    def film(f: Option[Int]) = f.fold("no film")(id => s"tmdb:$id")
    val margin = this.film match {
      case Some(_) => Some(s"$basis take at ${pct(confidence)}")
      case None    => best.flatMap(b => b.probability.map(p => if (b.vetoed) s"best candidate vetoed at ${pct(p)}" else s"best candidate held below the cut at ${pct(p)}"))
    }
    margin.toSeq ++
      Option.when(stagesDisagree)(s"served as ${served.map(film).mkString(", ")}, the model decided ${film(this.film)}") ++
      Option.when(judged > 0)(s"$judged of ${listings + judged} listings already labelled")
  }
}

object ReviewPriority {
  /** Stage disagreement makes a cluster at least this uncertain: one of the two stages is wrong on it. */
  val Disagreeing = 0.5

  def of(cluster: ReviewCluster, served: Map[String, Option[Int]], judged: JudgedListings): ReviewPriority = {
    val (done, open) = cluster.members.partition(judged(cluster, _))
    val otherwise    = cluster.memberKeys.flatMap(served.get).filterNot(_ == cluster.film).distinct
    val margin       = if (cluster.film.isDefined) 1 - cluster.confidence else cluster.bestProbability
    ReviewPriority(open.size, done.size, if (otherwise.nonEmpty) margin.max(Disagreeing) else margin, otherwise, cluster.basis,
      cluster.confidence, cluster.film, cluster.best)
  }
}

/**
 * The listings `identity-judgements.tsv` already settles for a card: one a row judges right for some film (its film is
 * known), or wrong for the very film the card puts forward. A row names its venue, or `*` for every venue billing the
 * raw title in its country; raw titles compare as every judgement reader keys them ([[LabelsTsv.normalise]]).
 */
final class JudgedListings(labels: Seq[LabelRow]) {
  private val byListing = labels.groupBy(l => (l.country, l.venue, LabelsTsv.normalise(l.rawTitle)))

  def apply(cluster: ReviewCluster, member: ListingKey): Boolean = {
    val raw   = LabelsTsv.normalise(member.rawTitle)
    val code  = cluster.country.code
    val shown = cluster.shown.map(FilmRef.tmdb(_).render)
    (byListing.getOrElse((code, member.venue, raw), Nil) ++ byListing.getOrElse((code, "*", raw), Nil))
      .exists(row => row.right || shown.contains(row.film))
  }
}

object JudgedListings {
  val none: JudgedListings = new JudgedListings(Nil)
}
