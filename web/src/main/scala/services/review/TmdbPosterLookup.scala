package services.review

import play.api.libs.json.Json
import services.identity.PosterAnswers

import java.util.Locale

/**
 * A film's TMDB poster asked of TMDB live: for the dev-only review pages, when no stored read gives a candidate one — the
 * corpus does not hold the film, and its poster evidence was hashed before the paths were filed beside the hashes, or never.
 * Writes nothing: what a resolve reads (`tmdb_films`, `identity_family_answers`) is never touched, so it wakes nothing.
 * Each film's answer is kept (bounded) for the session, none for a film TMDB no longer has; a read that failed for the
 * moment (a 429, a 5xx, a timeout) is none now and not kept, so the next card asks again.
 * Never wired in production.
 */
final class TmdbPosterLookup(http: tools.HttpFetch, key: settings.TmdbApiKey) {
  import TmdbPosterLookup.Film
  private val found = tools.BoundedCache.ofSize(TmdbPosterLookup.Kept).build[(Int, String), Option[Film]]()

  /** The poster TMDB names for `tmdbId` in `language`, at the size the cards show. */
  def poster(tmdbId: Int, language: Locale): Option[String] = film(tmdbId, language).flatMap(_.poster)

  /** The film as TMDB names it in `language` — its title, year and poster — from the same read as its poster. */
  def film(tmdbId: Int, language: Locale): Option[Film] =
    try found.get((tmdbId, language.toLanguageTag), { case (id, tag) => read(id, tag) })
    catch { case scala.util.control.NonFatal(_) => None }

  private def read(tmdbId: Int, tag: String): Option[Film] =
    try {
      val movie = Json.parse(http.get(s"https://api.themoviedb.org/3/movie/$tmdbId?language=$tag&api_key=${key.value}", key.authorization))
      Some(Film((movie \ "title").asOpt[String].filter(_.nonEmpty),
        (movie \ "release_date").asOpt[String].flatMap(_.take(4).toIntOption),
        (movie \ "poster_path").asOpt[String].map(PosterAnswers.FilmPosterBase + _)))
    } catch { case e: tools.HttpStatusException if tools.HttpStatusException.isDurable(e.code) => None }
}

object TmdbPosterLookup {
  final case class Film(title: Option[String], year: Option[Int], poster: Option[String]) {
    /** "Title (year)", as `identity-judgements.tsv` files a film's title. */
    def describe: Option[String] = title.map(t => t + year.fold("")(y => s" ($y)"))
  }

  /** How many films' posters a session keeps: a few pages of cards' candidates. */
  val Kept = 5000L
}
