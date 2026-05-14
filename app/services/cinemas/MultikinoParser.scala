package services.cinemas

import models.{CinemaMovie, Movie, Multikino, Showtime}
import play.api.libs.json._

import java.time.LocalDateTime
import scala.util.Try

/**
 * Pure JSON → `CinemaMovie` transformation for the Multikino API response.
 * No I/O: callers fetch the body (via `HttpFetch`) and hand it here. Keeping
 * parsing separate from transport lets the parsing be unit-tested against
 * recorded fixtures without any HTTP stubbing.
 */
object MultikinoParser {
  private val BaseUrl = "https://www.multikino.pl"

  def parse(json: String): Seq[CinemaMovie] =
    (Json.parse(json) \ "result").as[JsArray].value.map(parseFilm).toSeq

  private def parseFilm(film: JsValue): CinemaMovie = {
    val title       = (film \ "filmTitle").as[String].replaceFirst("^Kino na obcasach:\\s*", "")
    val multikinoId = (film \ "filmId").asOpt[String].filter(_.nonEmpty)
    val mxcId       = (film \ "movieXchangeCode").asOpt[String].filter(_.nonEmpty)
    val sessions    = (film \ "showingGroups").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                        .flatMap(g => (g \ "sessions").as[JsArray].value)
    CinemaMovie(
      movie       = Movie(
        title          = title,
        runtimeMinutes = (film \ "runningTime").asOpt[Int],
        releaseYear    = (film \ "releaseDate").asOpt[String].flatMap(parseYear)
      ),
      cinema      = Multikino,
      posterUrl   = (film \ "posterImageSrc").asOpt[String].filter(_.nonEmpty),
      filmUrl     = absoluteUrl((film \ "filmUrl").asOpt[String]),
      synopsis    = (film \ "synopsisShort").asOpt[String].filter(_.nonEmpty),
      cast        = (film \ "cast").asOpt[String].filter(_.nonEmpty),
      director    = (film \ "director").asOpt[String].filter(_.nonEmpty),
      showtimes   = sessions.flatMap(parseSession).toSeq,
      externalIds = (multikinoId.map("mk" -> _) ++ mxcId.map("mxc" -> _)).toMap
    )
  }

  private def parseSession(session: JsValue): Option[Showtime] =
    (session \ "startTime").asOpt[String].map { startTime =>
      val attrs = (session \ "attributes").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
                    .flatMap(a => (a \ "name").asOpt[String]).toSet
      Showtime(
        dateTime   = LocalDateTime.parse(startTime),
        bookingUrl = absoluteUrl((session \ "bookingUrl").asOpt[String]),
        room       = (session \ "screenName").asOpt[String].filter(_.nonEmpty),
        format     = parseFormat(attrs)
      )
    }

  // Multikino tags Language attributes as DUBBING / NAPISY / JĘZYK ORYGINALNY.
  // Normalise to DUB / NAP tokens (other clients use the same); original-language
  // screenings get no language tag (matches Cinema City's behaviour).
  private def parseFormat(attrs: Set[String]): List[String] = List(
    attrs.find(n => n == "2D" || n == "3D"),
    if (attrs.contains("DUBBING"))     Some("DUB")
    else if (attrs.contains("NAPISY")) Some("NAP")
    else None
  ).flatten

  private def absoluteUrl(url: Option[String]): Option[String] =
    url.filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else BaseUrl + u)

  private def parseYear(date: String): Option[Int] =
    Try(java.time.LocalDate.parse(date.take(10)).getYear).toOption
}
