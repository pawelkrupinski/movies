package services.review

import models.Country

/**
 * The review queue measured: per country, how many clusters it lists that an answer would settle a listing of, how many
 * listings those hold, and how many the first answers settle — each country's first `top`, and the first `answers` of
 * every country's queue merged, as the page lists them with the country "all".
 * `ReviewLabelsCli queue [top] [answers]` prints it over the local mirror.
 */
object ReviewQueueReport {

  /** One country's queue: the clusters still to answer that an answer would settle a listing of, best first. */
  final case class Queued(country: Country, ranked: Seq[(ReviewCluster, ReviewPriority)]) {
    def clusters: Int = ranked.size
    def listings: Int = ranked.map(_._2.listings).sum
    def settledBy(answers: Int): Int = ranked.take(answers).map(_._2.listings).sum
  }

  def queued(country: Country, source: ReviewSource, judged: JudgedListings, answered: ReviewAnswers.Index): Queued =
    Queued(country, ReviewSelection.queue(source.decisions(unmatchedOnly = false).map(ReviewCluster.of(country, _)), source.served(), judged)
      .filter { case (c, p) => p.score > 0 && answered.answerFor(c.id, c.reviewMembers).isEmpty })

  def render(queues: Seq[Queued], top: Int, answers: Int): String = {
    def share(part: Int, whole: Int) = if (whole == 0) "-" else f"${100.0 * part / whole}%.0f%%"
    val merged   = queues.flatMap(_.ranked).sortBy { case (c, p) => (-p.score, -p.listings, c.title) }
    val settled  = merged.take(answers).map(_._2.listings).sum
    val listings = queues.map(_.listings).sum
    val lines = queues.map { q =>
      f"${q.country.code}%-3s ${q.clusters}%6d clusters ${q.listings}%7d listings; first $top settle ${q.settledBy(top)}%6d (${share(q.settledBy(top), q.listings)})"
    }
    val perCountryTop = queues.map(q => q.settledBy(top)).sum
    (s"review queue: clusters an answer settles a listing of, unanswered, by score (listings no label covers × P(resolver wrong))" +:
      lines :+
      f"all ${queues.map(_.clusters).sum}%6d clusters $listings%7d listings; each country's first $top (${queues.map(_.ranked.take(top).size).sum} answers) " +
        f"settle $perCountryTop (${share(perCountryTop, listings)}); the merged queue's first $answers settle $settled (${share(settled, listings)})").mkString("\n")
  }
}
