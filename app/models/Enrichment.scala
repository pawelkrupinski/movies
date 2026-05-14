package models

/**
 * Cross-cinema metadata about a film — ratings + the URLs you need to link out.
 * Looked up by (cleaned title + year) once and then shared across every
 * cinema-screening of that same film.
 *
 * `imdbId` is optional because TMDB sometimes returns a hit for which it has
 * no IMDb cross-reference yet (very recent releases, niche films). The
 * Enrichment is still useful — TMDB id + Filmweb / Metacritic / RT URLs all
 * work without an IMDb id; only the IMDb rating + IMDb link are unavailable.
 * The daily TMDB-retry tick re-checks rows with imdbId=None so they get
 * filled when IMDb eventually indexes the film.
 */
case class Enrichment(
  imdbId:            Option[String],
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
  def imdbUrl: Option[String] = imdbId.map(id => s"https://www.imdb.com/title/$id/")
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
