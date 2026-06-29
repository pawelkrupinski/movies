package services.enrichment

import play.api.libs.json._
import tools.{Env, HttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Feature-gated OMDb (omdbapi.com) client that backfills the three external
 * ratings keyed by IMDb id: the IMDb rating, Rotten Tomatoes (%) and
 * Metacritic / Metascore (/100). Used as a FALLBACK only — it fills a rating
 * a film is MISSING, never overrides one another source already supplied.
 *
 * Endpoint (a single GET, no auth header — the key rides the query string):
 *   GET https://www.omdbapi.com/?i=<imdbId>&tomatoes=true&apikey=<key>
 *
 * Feature gate: the `OMDB_API_KEY` secret. When it is unset (the current
 * default) `ratings` short-circuits to `None` via `apiKey.flatMap` WITHOUT
 * making any HTTP call — exactly the TmdbClient pattern. Wire a key (Fly
 * secret / `.env.local`) to turn the backfill on.
 */
class OMDbClient(http: HttpFetch, apiKey: => Option[String] = OMDbClient.ApiKey) {
  import OMDbClient._

  /** OMDb ratings for `imdbId`, or `None` when the key is unset (no HTTP
   *  call), the id is blank, the call fails, or OMDb has no usable rating at
   *  all (every field N/A). Lazily reads `apiKey` so the missing-secret case
   *  never touches the network. */
  def ratings(imdbId: String): Option[OmdbRatings] =
    apiKey.flatMap { key =>
      if (imdbId.trim.isEmpty) None
      else Try(http.get(requestUrl(imdbId, key))).toOption.map(parse).filter(_.nonEmpty)
    }

  private def requestUrl(imdbId: String, key: String): String = {
    val id = URLEncoder.encode(imdbId.trim, StandardCharsets.UTF_8)
    s"$ApiBase?i=$id&tomatoes=true&apikey=$key"
  }

  /** Parse an OMDb response body into the three ratings. Public for
   *  fixture-driven testing — the field shapes ("7.5", "N/A", "85%",
   *  "72/100") carry enough edge cases to deserve assertions independent of
   *  HTTP. Never throws: a malformed/non-JSON body yields an all-empty result. */
  def parse(body: String): OmdbRatings = {
    val js = Try(Json.parse(body)).getOrElse(JsNull)

    val imdbRating = (js \ "imdbRating").asOpt[String].flatMap(asDouble)
    // Top-level `Metascore` is OMDb's canonical metascore; the `Ratings` array
    // carries the same number as "72/100". Prefer the top-level field; fall
    // back to the array entry when it's absent/N/A.
    val ratings = (js \ "Ratings").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
    def valueOf(source: String): Option[String] =
      ratings.collectFirst {
        case r if (r \ "Source").asOpt[String].contains(source) => (r \ "Value").asOpt[String]
      }.flatten

    val metascore      = (js \ "Metascore").asOpt[String].flatMap(leadingInt)
                           .orElse(valueOf("Metacritic").flatMap(leadingInt))
    val rottenTomatoes = valueOf("Rotten Tomatoes").flatMap(leadingInt)

    OmdbRatings(imdbRating = imdbRating, rottenTomatoes = rottenTomatoes, metascore = metascore)
  }
}

object OMDbClient {
  private val ApiBase = "https://www.omdbapi.com/"

  /** Feature flag: the backfill is OFF whenever this is unset. */
  val ApiKey: Option[String] = Env.get("OMDB_API_KEY")

  /** "7.5" → 7.5, "N/A" / "" / non-numeric → None. */
  private def asDouble(s: String): Option[Double] = s.trim.toDoubleOption

  /** Leading-integer extractor that copes with OMDb's varied number shapes:
   *  "72" → 72, "85%" → 85, "72/100" → 72, "N/A" → None. */
  private def leadingInt(s: String): Option[Int] =
    s.trim.takeWhile(_.isDigit) match {
      case ""     => None
      case digits => digits.toIntOption
    }
}

/** The three external ratings OMDb can supply for a film, each optional (OMDb
 *  returns "N/A" for any it lacks). `imdbRating` is a 0–10 score, both others
 *  are 0–100 ints (RT as a %, Metacritic as a /100 score). */
case class OmdbRatings(
  imdbRating:     Option[Double],
  rottenTomatoes: Option[Int],
  metascore:      Option[Int]
) {
  /** True when OMDb returned at least one usable rating — an all-empty result
   *  is useless as a fallback and is dropped by `OMDbClient.ratings`. */
  def nonEmpty: Boolean = imdbRating.isDefined || rottenTomatoes.isDefined || metascore.isDefined
}
