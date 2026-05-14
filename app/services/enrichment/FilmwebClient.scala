package services.enrichment

import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Looks up Filmweb metadata (canonical page URL + 1–10 user rating) for a Polish
 * film title. Filmweb's public site is a SPA, but its mobile/JSON API is
 * reachable without auth at:
 *   - GET /api/v1/live/search?query=...     → list of (id, type, matchedTitle)
 *   - GET /api/v1/film/{id}/info            → { title, year, ... }
 *   - GET /api/v1/film/{id}/rating          → { rate, count, ... }
 *
 * Search has no year field, so for year-disambiguation we fetch /info for the
 * top few film-type hits and pick the closest year.
 */
class FilmwebClient(http: HttpFetch = new RealHttpFetch()) {
  import FilmwebClient._

  /** Best-effort lookup. Returns None when no film-type hit, on any parse
   *  failure, or when the rating endpoint comes back empty.
   */
  def lookup(title: String, year: Option[Int]): Option[FilmwebInfo] =
    Try {
      val hits  = search(title)
      val infos = hits.take(MaxCandidates).flatMap(id => info(id).map(id -> _))
      val picked = year match {
        case Some(y) =>
          infos
            .sortBy { case (_, i) => i.year.map(yy => math.abs(yy - y)).getOrElse(Int.MaxValue) }
            .headOption
        case None => infos.headOption
      }
      picked.flatMap { case (id, i) =>
        rating(id).map(r => FilmwebInfo(url = canonicalUrl(id, i.title, i.year), rating = Some(r)))
          .orElse(Some(FilmwebInfo(url = canonicalUrl(id, i.title, i.year), rating = None)))
      }
    }.toOption.flatten

  /** Returns film ids in the order Filmweb ranks them. */
  def search(title: String): Seq[Int] =
    parseSearch(http.get(s"$ApiBase/live/search?query=${urlEncode(title)}"))

  def info(id: Int): Option[FilmInfo] =
    Try(parseInfo(http.get(s"$ApiBase/film/$id/info"))).toOption.flatten

  def rating(id: Int): Option[Double] =
    Try(parseRating(http.get(s"$ApiBase/film/$id/rating"))).toOption.flatten

  def parseSearch(body: String): Seq[Int] =
    (Json.parse(body) \ "searchHits").asOpt[JsArray].map(_.value).getOrElse(Nil).flatMap { js =>
      val isFilm = (js \ "type").asOpt[String].contains("film")
      val id     = (js \ "id").asOpt[Int]
      if (isFilm) id else None
    }.toSeq

  def parseInfo(body: String): Option[FilmInfo] = {
    val json = Json.parse(body)
    (json \ "title").asOpt[String].map { t =>
      FilmInfo(
        title = t,
        year  = (json \ "year").asOpt[Int]
      )
    }
  }

  def parseRating(body: String): Option[Double] =
    (Json.parse(body) \ "rate").asOpt[Double]
}

object FilmwebClient {
  private val ApiBase        = "https://www.filmweb.pl/api/v1"
  private val MaxCandidates  = 3

  case class FilmInfo(title: String, year: Option[Int])

  case class FilmwebInfo(url: String, rating: Option[Double])

  /**
   * Build the canonical page URL the way Filmweb encodes it. The site replaces
   * spaces with `+` and percent-encodes everything else.
   */
  def canonicalUrl(id: Int, title: String, year: Option[Int]): String = {
    val slug = URLEncoder.encode(title, StandardCharsets.UTF_8).replace("%20", "+")
    val y    = year.map(_.toString).getOrElse("")
    s"https://www.filmweb.pl/film/$slug-$y-$id"
  }

  private def urlEncode(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)
}
