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
 * Feature gate: the `TRAKT_API_CLIENT_ID` secret (Trakt's `client_id`), sent
 * as the `trakt-api-key` header Trakt requires on every request — for the
 * public search endpoints the header value IS the client_id (no OAuth, so the
 * paired `TRAKT_API_SECRET` isn't needed here). Unset → every method
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

  /** Feature flag: the client is OFF (no HTTP) whenever this is unset. Trakt's
   *  `client_id`, sent as the `trakt-api-key` header on every request. */
  val ApiKey: Option[String] = Env.get("TRAKT_API_CLIENT_ID")

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

  /** Identify as an application, not as Chrome. Trakt's docs list `User-Agent` as
   *  required ("we suggest using your app and version like `MyAppName/1.0.0`") and
   *  they announced enforcement in Dec 2025 alongside Cloudflare rules aimed at
   *  bot traffic — a browser UA on a JSON API endpoint is exactly that shape.
   *  `RealHttpFetch` sends a Chrome UA by default, so this override is what makes
   *  us honest (and it now REPLACES rather than appends — see `buildRequest`). */
  private[enrichment] val UserAgent = "kinowo/1.0 (+https://kinowo.fly.dev)"

  /** Trakt requires client_id auth, an API version, a JSON content type and a
   *  meaningful User-Agent on every request. A missing content type is a 412 and
   *  a rejected key is a 403, so sending all four keeps the failure modes
   *  distinguishable rather than collapsing them into one opaque 403. */
  private[enrichment] def headers(key: String): Map[String, String] =
    Map(
      "trakt-api-key"     -> key,
      "trakt-api-version" -> "2",
      "Content-Type"      -> "application/json",
      "User-Agent"        -> UserAgent
    )

  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
