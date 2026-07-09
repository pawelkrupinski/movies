package services.enrichment

import play.api.libs.json._
import tools.{Env, HttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Feature-gated Trakt (api.trakt.tv) client that recovers IDENTIFIERS, not
 * rating values: Trakt's search results each carry an `ids` block holding the
 * IMDb id and TMDB id together, so a single call bridges one id to the other.
 * Two entry points, mirroring how a film reaches us with partial identity:
 *
 *   - [[findByImdbId]] — GET `/search/imdb/{imdbId}?type=movie`. An EXACT,
 *     id-keyed lookup: a known IMDb id → the same film's TMDB id with no fuzzy
 *     matching and nothing to corroborate. The strongest path.
 *   - [[search]] — GET `/search/movie?query=…&years=…`. Fuzzy title search
 *     returning candidates that each carry both ids; the caller
 *     ([[TraktIdResolver]]) corroborates before binding one.
 *
 * Feature gate: the `TRAKT_API_KEY` secret (Trakt's `client_id`), sent as the
 * `trakt-api-key` header Trakt requires on every request. Unset → every method
 * short-circuits (None / empty) WITHOUT any HTTP call — the OMDbClient pattern.
 */
class TraktClient(http: HttpFetch, apiKey: => Option[String] = TraktClient.ApiKey) {
  import TraktClient._

  /** The film Trakt maps to a given IMDb id, or None when the key is unset (no
   *  HTTP), the id is not a `tt…` id, Trakt has no match, or the call fails. */
  def findByImdbId(imdbId: String): Option[TraktMovie] =
    apiKey.flatMap { key =>
      val id = imdbId.trim
      if (!id.startsWith("tt")) None
      else parseResults(fetch(s"$ApiBase/search/imdb/${enc(id)}?type=movie", key)).headOption
    }

  /** Fuzzy title candidates, each carrying whatever ids Trakt holds. Empty when
   *  the key is unset (no HTTP), the title is blank, or the call fails. */
  def search(title: String, year: Option[Int]): Seq[TraktMovie] =
    apiKey.toSeq.flatMap { key =>
      val t = title.trim
      if (t.isEmpty) Seq.empty
      else parseResults(fetch(s"$ApiBase/search/movie?query=${enc(t)}${year.map(y => s"&years=$y").getOrElse("")}", key))
    }

  private def fetch(url: String, key: String): String =
    Try(http.get(url, headers(key))).getOrElse("[]")
}

object TraktClient {
  private val ApiBase = "https://api.trakt.tv"

  /** Feature flag: the client is OFF (no HTTP) whenever this is unset. */
  val ApiKey: Option[String] = Env.get("TRAKT_API_KEY")

  /** One Trakt film — title/year for corroboration, plus the cross-ids. */
  final case class TraktMovie(title: String, year: Option[Int], imdbId: Option[String], tmdbId: Option[Int])

  /** Parse a Trakt search response (a JSON array of `{type, score, movie}`
   *  entries) into the movies carrying at least one usable id. Never throws. */
  private[enrichment] def parseResults(body: String): Seq[TraktMovie] =
    Try(Json.parse(body)).toOption.flatMap(_.asOpt[JsArray]).map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(entry => (entry \ "movie").asOpt[JsValue].flatMap(movieFrom))

  private def movieFrom(m: JsValue): Option[TraktMovie] = {
    val ids   = m \ "ids"
    val tmdb  = (ids \ "tmdb").asOpt[Int]
    val imdb  = (ids \ "imdb").asOpt[String].filter(_.startsWith("tt"))
    if (tmdb.isEmpty && imdb.isEmpty) None
    else Some(TraktMovie((m \ "title").asOpt[String].getOrElse(""), (m \ "year").asOpt[Int], imdb, tmdb))
  }

  /** Trakt requires client_id auth + an API version on every request. */
  private def headers(key: String): Map[String, String] =
    Map("trakt-api-key" -> key, "trakt-api-version" -> "2")

  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
