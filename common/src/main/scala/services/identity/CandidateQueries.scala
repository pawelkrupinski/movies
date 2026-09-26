package services.identity

import services.movies.TitleNormalizer
import services.resolution.SearchTitles

/**
 * CANDIDATE GENERATION's question set (docs/design/identity-resolver.md §phase 2, stage a): what
 * the resolver asks the lookup source about one listing's evidence. A family's set is the union of
 * its members' — so it is a function of the family's listing SET, never of which member arrived
 * first or of an earlier answer (A1).
 *
 *  - every title SHAPE the listing published: its title, its original title, and each delimited
 *    segment of either (`SearchTitles.candidates`: the parts around " | ", " – ", ": ", a trailing
 *    bracket) — general punctuation, no curated banner list — each in the two query forms the
 *    country's normaliser writes;
 *  - each shape with the year the listing states, and without a year (a bracket year is often a
 *    re-release, a field year a production year TMDB dates differently);
 *  - every credited director's filmography.
 */
object CandidateQueries {

  def of(e: Evidence, normalizer: TitleNormalizer): Seq[CandidateQuery] = {
    val shapes = SearchTitles.candidates(e.cleanTitle, e.originalTitle)
      .flatMap(t => Seq(normalizer.apiQuery(t), normalizer.searchQuery(t)))
      .map(_.trim).filter(_.nonEmpty).distinct
    val years     = e.statedYear.toSeq.map(Option(_)) :+ Option.empty[Int]
    val titles    = for (q <- shapes; y <- years) yield CandidateQuery.Title(q, y)
    val directors = e.directors.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty).distinct.map(CandidateQuery.Director(_))
    (titles ++ directors).distinct.sorted
  }
}
