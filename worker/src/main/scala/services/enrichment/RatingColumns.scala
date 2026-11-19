package services.enrichment

import models.Source
import services.enrichment.SiteRatings.Columns

/**
 * Where each URL-keyed rating site's link, score and content slot live on a row: what the shared flow
 * ([[SiteRatings]]) writes when a site's page is found, read or denied. The site itself only finds and reads pages
 * ([[SiteRatings.Site]]); the composition root hands the flow the site's port and its columns together.
 */
object RatingColumns {

  val Metacritic: Columns[Int] = Columns[Int](
    noun      = "metascore",
    plural    = "score(s)",
    url       = _.metacriticUrl,
    score     = _.metascore,
    withUrl   = (row, url) => row.copy(metacriticUrl = Some(url)),
    withScore = (row, score) => row.copy(metascore = score),
    withPage  = (row, page) => row.copy(metascore = page.score),
    cleared   = _.copy(metacriticUrl = None, metascore = None),
    badge     = _.toString)

  val RottenTomatoes: Columns[Int] = Columns[Int](
    noun      = "Tomatometer",
    plural    = "score(s)",
    url       = _.rottenTomatoesUrl,
    score     = _.rottenTomatoes,
    withUrl   = (row, url) => row.copy(rottenTomatoesUrl = Some(url)),
    withScore = (row, score) => row.copy(rottenTomatoes = score),
    withPage  = (row, page) => row.copy(rottenTomatoes = page.score),
    cleared   = _.copy(rottenTomatoesUrl = None, rottenTomatoes = None),
    badge     = score => s"$score%")

  /** Filmweb is a full enrichment source: a page read whole also writes (or, carrying no content, drops) its slot. */
  val Filmweb: Columns[Double] = Columns[Double](
    noun      = "rating",
    plural    = "rating(s)",
    url       = _.filmwebUrl,
    score     = _.filmwebRating,
    withUrl   = (row, url) => row.copy(filmwebUrl = Some(url)),
    withScore = (row, rating) => row.copy(filmwebRating = rating),
    withPage  = (row, page) => row.copy(filmwebRating = page.score,
      data = page.slot.fold(row.data - (models.Filmweb: Source))(slot => row.data + ((models.Filmweb: Source) -> slot))),
    cleared   = row => row.copy(filmwebUrl = None, filmwebRating = None, data = row.data - (models.Filmweb: Source)),
    badge     = RatingDisplay.label)
}
