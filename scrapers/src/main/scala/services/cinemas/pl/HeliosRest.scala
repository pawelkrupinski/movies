package services.cinemas.pl

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZonedDateTime}

import scala.util.Try

import play.api.libs.json.*
import services.cinemas.common.ScraperParse
import services.cinemas.common.ScraperParse.jsonText

/** Helios's REST API (restapi.helios.pl) as data: what one venue's `/screening`, `/event`, `/movie/{id}` and
 *  `/screen/{id}` bodies read into. [[HeliosRestFeed]] fetches them; [[HeliosClient]] reconciles them with the NUXT
 *  repertoire page. */
object HeliosRest {

  /** One REST screening; `event` when `/event` listed it, `movieId` then the one film the event names (or empty). */
  final case class ApiScreening(
    id:       String,
    movieId:  String,
    screenId: String,
    release:  String,
    dateTime: Option[LocalDateTime],
    event:    Boolean = false
  ) {
    /** The 2D/NAP/ATMOS… tokens of `release`. */
    def format: List[String] = release.split("/").toList.filter(_.nonEmpty)
  }

  final case class ApiMovieInfo(
    title:         Option[String],
    originalTitle: Option[String],
    duration:      Option[Int],
    description:   Option[String],
    cast:          Seq[String],
    director:      Seq[String],
    year:          Option[Int],
    posterUrl:     Option[String],
    slug:          Option[String],
    countries:     Seq[String],
    genres:        Seq[String],
    trailerUrl:    Option[String],
    ageRating:     Option[String]
  )

  /** One venue's REST read: every screening by its UUID (NUXT's screening `sourceId`), each film body by its id, each
   *  screen's name by its id. */
  final case class RestData(
    screeningsById: Map[String, ApiScreening],
    movieDetails:   Map[String, ApiMovieInfo],
    screenNames:    Map[String, String]
  )

  private val OffsetDtf = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  def parseApiScreenings(body: String): Map[String, ApiScreening] =
    jsonEntries(body) { s =>
      for {
        id       <- (s \ "id").asOpt[String]
        movieId  <- (s \ "movieId").asOpt[String]
        screenId <- (s \ "screenId").asOpt[String]
      } yield id -> ApiScreening(id, movieId, screenId, release(s), warsawTime(s, "screeningTimeFrom"))
    }

  // The `/event` endpoint returns one entry per event screening (a flat array),
  // each with its `screeningId`, `screenId` and `release`. We map them
  // into the same ApiScreening shape used for room/format enrichment. Each entry also
  // names the film(s) it screens (`items[].movieId`): an event of ONE film takes that
  // film's REST body — the director, cast and year the repertoire page never states for
  // an event; a pack of several (a marathon) names none of them, `movieId` empty.
  def parseEventScreenings(body: String): Map[String, ApiScreening] =
    jsonEntries(body) { e =>
      for {
        id       <- (e \ "screeningId").asOpt[String]
        screenId <- (e \ "screenId").asOpt[String]
      } yield {
        val films = (e \ "items").asOpt[JsArray].map(_.value.toSeq).getOrElse(Nil).flatMap(i => (i \ "movieId").asOpt[String]).distinct
        id -> ApiScreening(id, movieId = films match { case Seq(only) => only; case _ => "" }, screenId, release(e),
          warsawTime(e, "timeFrom"), event = true)
      }
    }

  def parseApiMovieBody(body: String): Option[ApiMovieInfo] =
    Try(Json.parse(body)).toOption.flatMap { js =>
      (js \ "title").asOpt[String].filter(_.nonEmpty).map { title =>
        ApiMovieInfo(
          title         = Some(title),
          originalTitle = nonEmptyText(js, "originalTitle"),
          duration      = (js \ "duration").asOpt[Int],
          description   = nonEmptyText(js, "description").map(tools.TextNormalization.stripHtml),
          cast          = commaList(nonEmptyText(js, "filmCast")),
          director      = commaList(nonEmptyText(js, "director")),
          year          = (js \ "yearOfProduction").asOpt[String].flatMap(s => Try(s.take(4).toInt).toOption),
          posterUrl     = firstString(js, "posters").filter(_.startsWith("http")),
          slug          = nonEmptyText(js, "slug"),
          countries     = commaList(jsonText(js \ "country")),
          genres        = genres(js),
          trailerUrl    = trailerUrl(js),
          ageRating     = parseAgeRating(js)
        )
      }
    }

  /** A `/screen/{id}` body's room name. */
  def parseScreenName(body: String): Option[String] =
    Try((Json.parse(body) \ "name").asOpt[String]).toOption.flatten

  private def jsonEntries[A](body: String)(entry: JsValue => Option[(String, A)]): Map[String, A] =
    Try(Json.parse(body).as[JsArray]).map(_.value.flatMap(entry).toMap).getOrElse(Map.empty)

  private def release(entry: JsValue): String = (entry \ "release").asOpt[String].getOrElse("")

  private def warsawTime(entry: JsValue, field: String): Option[LocalDateTime] =
    (entry \ field).asOpt[String].flatMap(timestamp =>
      Try(ZonedDateTime.parse(timestamp, OffsetDtf).withZoneSameInstant(models.TimeZones.Poland).toLocalDateTime).toOption)

  private def nonEmptyText(js: JsValue, field: String): Option[String] = (js \ field).asOpt[String].filter(_.nonEmpty)

  private def commaList(text: Option[String]): Seq[String] = text.toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))

  private def firstString(js: JsValue, field: String): Option[String] =
    (js \ field).asOpt[JsArray].flatMap(_.value.headOption).flatMap(_.asOpt[String])

  // Helios ships `genres: [{id, name, description}]` with lowercase
  // Polish names ("science fiction", "dramat"). Title-case at the
  // write boundary so the display layer doesn't have to special-case
  // this source's casing.
  private def genres(js: JsValue): Seq[String] =
    ScraperParse.jsonObjectNames(js \ "genres").map(tools.TextNormalization.titleCaseIfAllLower)

  // Helios ships `trailers: [<youtube embed URL>]`. Take the first
  // entry; for non-embed YouTube URLs (live API has been seen to mix
  // both shapes) we re-canonicalise to the watch URL and let the
  // view layer convert back to /embed/ via TrailerEmbed.
  private def trailerUrl(js: JsValue): Option[String] =
    firstString(js, "trailers").filter(_.nonEmpty)
      .flatMap(u => models.TrailerEmbed.youTubeId(u).map(id => s"https://www.youtube.com/watch?v=$id"))

  // Helios ships `ratings: [{cinemaGroupId, symbol, value, description}]` — the
  // Polish age certificate. `symbol`/`value` are the clean short form ("15",
  // "7", "0"); `description` is the verbose "od lat 15" / "b.o.". Take the FIRST
  // rating's `symbol` (falling back to `value`) as the clean numeric label. The
  // "no restriction" marker — symbol "0" or description "b.o." (bez ograniczeń) —
  // is filtered to `None` here before `AgeRating.normalize`, which only strips
  // blanks/placeholders and can't know Helios's local convention. Empty
  // `ratings` → `None`.
  private def parseAgeRating(js: JsValue): Option[String] =
    (js \ "ratings").asOpt[JsArray].flatMap(_.value.headOption).flatMap { rating =>
      val symbol        = nonEmptyText(rating, "symbol").orElse(nonEmptyText(rating, "value"))
      val description   = (rating \ "description").asOpt[String].map(_.trim)
      val noRestriction = symbol.contains("0") || description.exists(_.equalsIgnoreCase("b.o."))
      if (noRestriction) None else symbol
    }.flatMap(services.cinemas.common.AgeRating.normalize)
}
