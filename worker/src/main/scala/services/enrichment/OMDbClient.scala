package services.enrichment

import play.api.libs.json._
import tools.{Env, HttpFetch, TextNormalization}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Feature-gated OMDb (omdbapi.com) client used to recover IDENTIFIERS, not
 * rating values: an IMDb id (by title+year search) and the canonical Rotten
 * Tomatoes URL (by imdb id). The canonical refreshers — [[ImdbRatings]] and
 * [[RottenTomatoesRatings]] — then fetch the actual scores FROM those ids/links
 * on their next tick, so OMDb never writes a rating value itself. That keeps
 * each rating's "exactly one canonical writer" invariant: OMDb is a pure
 * identifier backfill, the rating numbers stay owned by their own source.
 *
 * Two endpoints (a single GET each, key on the query string):
 *   - by title:   GET ?t=<title>&y=<year>        → `imdbID`
 *   - by imdb id: GET ?i=<imdbId>&tomatoes=true   → `tomatoURL` (the RT link)
 *
 * Feature gate: the `OMDB_API_KEY` secret. Unset → every method short-circuits
 * to `None` WITHOUT any HTTP call (the TmdbClient pattern). Wire a key (Fly
 * secret / `.env.local`) to turn the backfill on.
 */
class OMDbClient(http: HttpFetch, apiKey: => Option[String] = OMDbClient.ApiKey) {
  import OMDbClient._

  /** Recover an IMDb id by OMDb title+year search. Tries each supplied title
   *  spelling in turn (pass the English/original title first — OMDb is an
   *  English-language DB) and accepts a hit only when OMDb's returned title
   *  plausibly matches the query, so OMDb's fuzzy fallback can't bind an
   *  unrelated film. None when the key is unset (no HTTP), nothing matches, or
   *  every call fails. */
  def findImdbId(titles: Seq[String], year: Option[Int]): Option[String] =
    apiKey.flatMap { key =>
      titles.iterator.map(_.trim).filter(_.nonEmpty).distinct
        .flatMap(t => searchImdbId(t, year, key).iterator)
        .nextOption()
    }

  private def searchImdbId(title: String, year: Option[Int], key: String): Option[String] = {
    val js      = Try(Json.parse(http.get(titleUrl(title, year, key)))).getOrElse(JsNull)
    val matched = (js \ "Response").asOpt[String].contains("True")
    val id      = (js \ "imdbID").asOpt[String].filter(_.startsWith("tt"))
    val title2  = (js \ "Title").asOpt[String].getOrElse("")
    id.filter(_ => matched && titlesPlausiblyMatch(title, title2))
  }

  /** Recover the canonical Rotten Tomatoes URL for an imdb id — OMDb's
   *  `tomatoURL`, present only when OMDb holds RT data for the film. None when
   *  the key is unset, the id is blank, the call fails, or OMDb has no RT URL. */
  def rottenTomatoesUrl(imdbId: String): Option[String] =
    apiKey.flatMap { key =>
      if (imdbId.trim.isEmpty) None
      else {
        val js = Try(Json.parse(http.get(idUrl(imdbId.trim, key)))).getOrElse(JsNull)
        (js \ "tomatoURL").asOpt[String].map(_.trim)
          .filter(u => u.nonEmpty && u != "N/A" && u.startsWith("http"))
      }
    }

  private def titleUrl(title: String, year: Option[Int], key: String): String = {
    val t = URLEncoder.encode(title, StandardCharsets.UTF_8)
    val y = year.map(yy => s"&y=$yy").getOrElse("")
    s"$ApiBase?t=$t$y&apikey=$key"
  }
  private def idUrl(imdbId: String, key: String): String =
    s"$ApiBase?i=${URLEncoder.encode(imdbId, StandardCharsets.UTF_8)}&tomatoes=true&apikey=$key"
}

object OMDbClient {
  private val ApiBase = "https://www.omdbapi.com/"

  /** Feature flag: the backfill is OFF whenever this is unset. */
  val ApiKey: Option[String] = Env.get("OMDB_API_KEY")

  /** OMDb's `?t=` search applies its own fuzzy fallback, so guard the result:
   *  accept only when one diacritic-folded, alphanumeric-only title contains the
   *  other (handles "Sirat" vs "Sirât", colon-subtitle suffixes), rejecting an
   *  unrelated fuzzy hit. */
  private[enrichment] def titlesPlausiblyMatch(query: String, returned: String): Boolean = {
    val a = norm(query); val b = norm(returned)
    a.nonEmpty && b.nonEmpty && (a.contains(b) || b.contains(a))
  }
  private def norm(s: String): String =
    TextNormalization.deburr(s).toLowerCase.filter(_.isLetterOrDigit)
}
