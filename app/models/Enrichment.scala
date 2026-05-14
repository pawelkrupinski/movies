package models

/**
 * Cross-cinema metadata about a film — ratings + the URLs you need to link out.
 * Looked up by (cleaned title + year) once and then shared across every
 * cinema-screening of that same film.
 *
 * `metacriticUrl` / `rottenTomatoesUrl` are populated by MetacriticClient /
 * RottenTomatoesClient at enrichment time. Each tries to resolve the canonical
 * movie page; if the canonical 404s, it stores the on-site search URL instead.
 * Pre-feature records may have None for these — `metacriticHref` /
 * `rottenTomatoesHref` fall back to generating a search URL on the fly.
 */
case class Enrichment(
  imdbId:            String,
  imdbRating:        Option[Double],
  metascore:         Option[Int],
  originalTitle:     Option[String],
  filmwebUrl:        Option[String] = None,
  filmwebRating:     Option[Double] = None,
  rottenTomatoes:    Option[Int]    = None,
  tmdbId:            Option[Int]    = None,
  metacriticUrl:     Option[String] = None,
  rottenTomatoesUrl: Option[String] = None
) {
  def imdbUrl: String = s"https://www.imdb.com/title/$imdbId/"
  def tmdbUrl: Option[String] = tmdbId.map(id => s"https://www.themoviedb.org/movie/$id")

  /** Display-time URL: validated stored URL if we have one, else an on-the-fly
   *  search URL (for legacy records that pre-date URL persistence). */
  def metacriticHref(fallbackTitle: String): String = metacriticUrl.getOrElse {
    val q = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.metacritic.com/search/$q/?category=2"
  }

  def rottenTomatoesHref(fallbackTitle: String): String = rottenTomatoesUrl.getOrElse {
    val q = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.rottentomatoes.com/search?search=$q"
  }
}
