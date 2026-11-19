package services.review

import java.time.Instant

/** Builds the cards of the clusters on screen — and reads only theirs ([[ReviewCardFacts]]). */
object ReviewCards {
  /** How many of a cast's names a card shows before folding the rest behind "+N more". */
  val CastShown = 5

  def build(source: ReviewSource, clusters: Seq[(ReviewCluster, Option[Instant])], labels: Seq[LabelRow],
            answers: ReviewAnswers.Index): Seq[ReviewCard] = {
    val facts = ReviewCardFacts.read(source, clusters)
    clusters.map { case (cluster, at) =>
      val code = cluster.country.code
      val overlay = labels.filter(l => l.country == code &&
        cluster.members.exists(m => m.rawTitle == l.rawTitle && (l.venue == "*" || l.venue == m.venue)))
      ReviewCard(cluster, cluster.members.map(facts.member), facts.films, overlay, answers.answerFor(cluster), at)
    }
  }
}
