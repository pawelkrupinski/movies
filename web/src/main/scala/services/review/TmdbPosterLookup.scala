package services.review

import play.api.libs.json.Json
import services.identity.PosterAnswers

import java.util.Locale

/**
 * A film's TMDB poster asked of TMDB live: for the dev-only review pages, when no stored read gives a candidate one — the
 * corpus does not hold the film, and its poster evidence was hashed before the paths were filed beside the hashes, or never.
 * Writes nothing: what a resolve reads (`tmdb_films`, `identity_family_answers`) is never touched, so it wakes nothing.
 * Each film's answer is kept (bounded) for the session, none for a film TMDB no longer has; a failed read is not kept.
 * Never wired in production.
 */
final class TmdbPosterLookup(http: tools.HttpFetch, key: settings.TmdbApiKey) {
  private val found = tools.BoundedCache.ofSize(TmdbPosterLookup.Kept).build[(Int, String), Option[String]]()

  /** The poster TMDB names for `tmdbId` in `language`, at the size the cards show. */
  def poster(tmdbId: Int, language: Locale): Option[String] =
    found.get((tmdbId, language.toLanguageTag), { case (id, tag) => read(id, tag) })

  private def read(tmdbId: Int, tag: String): Option[String] =
    try (Json.parse(http.get(s"https://api.themoviedb.org/3/movie/$tmdbId?language=$tag&api_key=${key.value}", key.authorization)) \ "poster_path")
      .asOpt[String].map(PosterAnswers.FilmPosterBase + _)
    catch { case e: tools.HttpStatusException if e.code == 404 => None }
}

object TmdbPosterLookup {
  /** How many films' posters a session keeps: a few pages of cards' candidates. */
  val Kept = 5000L
}
