package services.cinemas.us

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import services.cinemas.common.{AgeRating, FilmDetail}

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Pure JSON → model transformation for Regal's `getShowtimes` API. No I/O:
 * [[RegalClient]] fetches the bodies and hands them here, so every parse is
 * unit-testable against the recorded fixtures without any HTTP stubbing.
 *
 * The response envelope is the same whichever way it is asked:
 *
 * {{{
 * { "shows":           [ { "TheatreCode": "1438", "AdvertiseShowDate": "...",
 *                          "Film": [ { "Title", "MasterMovieCode",
 *                                      "Performances": [ { "CalendarShowTime",
 *                                                          "Auditorium",
 *                                                          "PerformanceId",
 *                                                          "PerformanceAttributes" } ] } ] } ],
 *   "datesWithShows": [ "2026-08-30T00:00:00", ... ],
 *   "movies":         [ ...film detail... ] }
 * }}}
 *
 * `shows` carries ONE entry per requested theatre that has a programme on the
 * requested date. A theatre with nothing on that date is simply absent from the
 * array — see [[parseDay]] for why that is data rather than a failure.
 */
object RegalParser {

  /** Regal timestamps are `2026-08-30T11:10:00` — an ISO local date-time with no
   *  zone, already in the VENUE's own local time (the payload carries the UTC
   *  instant separately as `UtcShowTime`). We store wall-clock local times, so
   *  the calendar field is the one to read and no zone conversion is wanted. */
  private def localDateTime(raw: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(raw)).toOption

  /** The day part of one of those timestamps. */
  private def localDate(raw: String): Option[LocalDate] =
    Try(LocalDateTime.parse(raw).toLocalDate).toOption.orElse(Try(LocalDate.parse(raw.take(10))).toOption)

  private def body(json: String): JsValue =
    Try(Json.parse(json)).getOrElse(
      throw new IllegalStateException("Regal getShowtimes returned a body that is not JSON"))

  private def arrayAt(root: JsValue, field: String): Option[Seq[JsValue]] =
    (root \ field).toOption.collect { case JsArray(values) => values.toSeq }

  /** The dates the REQUESTED THEATRES have a programme on — the union across the
   *  whole batch, which is exactly what makes one chunk list serve every venue in
   *  it (they then all issue the same per-date URL and share one fetch).
   *
   *  Measured 2026-08-30: the union grows with the batch (5 codes → 79 dates,
   *  25 → 102, 100 → 105, 396 → 111), confirming it is a union and not one
   *  theatre's list.
   *
   *  An EMPTY list is expected data — a batch with nothing on at all. A body with
   *  no `datesWithShows` FIELD is a response we failed to parse, and throws: the
   *  line [[services.cinemas.common.FlicksClient]] and
   *  [[services.cinemas.uk.CineworldParser]] already draw, and the one that
   *  matters, because an index that silently returns no days would narrow every
   *  venue in the batch to an empty listing. */
  def parseDates(json: String): Seq[LocalDate] =
    arrayAt(body(json), "datesWithShows") match {
      case None => throw new IllegalStateException(
        "Regal getShowtimes response carried no datesWithShows array")
      case Some(values) =>
        values.flatMap(_.asOpt[String]).flatMap(localDate).distinct.sorted
    }

  /** ONE date's response → the films screening at `theatreCode` that day.
   *
   *  The response covers the whole batch, so this picks out the one `shows[]`
   *  entry whose `TheatreCode` matches and ignores the rest. A theatre with
   *  NOTHING on that date has no entry, and that returns EMPTY rather than
   *  throwing — an idle venue is data, not an outage. (Throwing on "no rows for
   *  me" is what left five UK venues permanently red on /uptime in July 2026.)
   *  A body with no `shows` FIELD at all is a parse failure and throws, so a
   *  broken response still fails loudly.
   *
   *  Deterministic: films ordered by `MasterMovieCode`, each film's showtimes by
   *  start time. */
  def parseDay(json: String, theatreCode: String, cinema: Cinema): Seq[CinemaMovie] = {
    val root = body(json)
    val shows = arrayAt(root, "shows").getOrElse(
      throw new IllegalStateException("Regal getShowtimes response carried no shows array"))

    val mine = shows.filter(show => (show \ "TheatreCode").asOpt[String].contains(theatreCode))

    val films = mine.flatMap(show => arrayAt(show, "Film").getOrElse(Nil))

    films.flatMap { film =>
      for {
        title <- (film \ "Title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        code  <- (film \ "MasterMovieCode").asOpt[String].map(_.trim).filter(_.nonEmpty)
      } yield {
        val showtimes = arrayAt(film, "Performances").getOrElse(Nil).flatMap(performance)
        (code, title, showtimes)
      }
    }
      // One film can appear more than once across a theatre's rows (a re-issue
      // sharing a code, a batch answering twice); union rather than pick one.
      .groupBy { case (code, _, _) => code }
      .toSeq.sortBy(_._1)
      .flatMap { case (code, group) =>
        val showtimes = group.flatMap(_._3)
          .distinctBy(s => (s.dateTime, s.room, s.format))
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(title = group.head._2),
          cinema    = cinema,
          posterUrl = None,
          // The film's stable per-chain reference AND the URL its detail is
          // fetched from — see `RegalClient.fetchFilmDetail`. Regal's public
          // film page is `/movies/<slug>-<code>` and the code alone does NOT
          // resolve it (verified 2026-08-30: `/movies/ho00021207` 404s), so
          // there is no page URL derivable from the listing alone. `filmUrl` is
          // an internal reference (it surfaces only on the dev-only /debug
          // pages), so the API URL is the honest thing to carry.
          filmUrl   = Some(RegalClient.filmDetailUrl(code)),
          synopsis  = None,
          cast      = Nil,
          director  = Nil,
          showtimes = showtimes,
          // The chain's own film id, kept so a later pass can join to it.
          externalIds = Map("regal" -> code)
        ))
      }
  }

  /** One `Performances[]` entry → a [[Showtime]]. The auditorium becomes the
   *  room; the attribute list becomes the format, minus the noise every
   *  screening carries. */
  private def performance(entry: JsValue): Option[Showtime] =
    (entry \ "CalendarShowTime").asOpt[String].flatMap(localDateTime).map { start =>
      Showtime(
        dateTime   = start,
        bookingUrl = None,
        room       = (entry \ "Auditorium").asOpt[JsValue].flatMap(auditorium),
        format     = formats(entry)
      )
    }

  /** `Auditorium` arrives as a number (`6`) on most rows and occasionally as a
   *  string; render either as the screen label. */
  private def auditorium(value: JsValue): Option[String] = value match {
    case JsNumber(n) => Some(n.toBigInt.toString)
    case JsString(s) => Some(s.trim).filter(_.nonEmpty)
    case _           => None
  }

  /** Screening attributes worth showing as a format badge.
   *
   *  `PerformanceAttributes` mixes genuine presentation formats (IMAX, RPX, 4DX,
   *  ScreenX, 3D, Dine-In) with accessibility and merchandising flags that every
   *  other screening carries too (`CC`, `DV`, `Reserved-Selected`, `No Passes`,
   *  `Laser`, `Recliner`, `Stadium`, `2D`). Badging a film "2D, CC, Reserved"
   *  on every row is noise, so only the presentation formats survive.
   *
   *  Kept as an allow-list rather than a deny-list: an attribute we have not
   *  seen is far more likely to be another operational flag than a new premium
   *  format, and a missing badge is a smaller error than a wrong one. */
  private val FormatAttributes: Set[String] =
    Set("IMAX", "IMAX 70MM", "RPX", "4DX", "SCREENX", "SCREENX 3D", "3D", "70MM", "35MM",
        "DINE-IN", "REGAL VIP", "VIP", "ATMOS", "MAGNIFY8", "HDR")

  private def formats(entry: JsValue): List[String] =
    (entry \ "PerformanceAttributes").asOpt[Seq[String]].getOrElse(Nil)
      .map(_.trim).filter(_.nonEmpty)
      .filter(attribute => FormatAttributes.contains(attribute.toUpperCase))
      .distinct.toList

  /** One film's `/api/Movies?hoCode=<code>` response → its detail fields.
   *
   *  The endpoint answers a single object (not an array). Regal fills
   *  `GraphicUrl`/`TrailerUrl` with an empty string rather than omitting them
   *  when it has none, so both are blank-filtered. `Duration` is minutes.
   *  `Rating` is the MPA label kept verbatim ("PG13", "R") the way every other
   *  source keeps its own certificate spelling. */
  def parseDetail(json: String): FilmDetail = {
    val root = body(json) match {
      case JsArray(values) => values.headOption.getOrElse(JsNull)
      case other           => other
    }
    def text(field: String): Option[String] =
      (root \ field).asOpt[String].map(_.trim).filter(_.nonEmpty)
    def list(field: String): Seq[String] =
      (root \ field).asOpt[Seq[String]].getOrElse(Nil).map(_.trim).filter(_.nonEmpty)

    FilmDetail(
      // LongDescription is the full synopsis; Description is a truncated teaser
      // of the same text, so prefer the long one and fall back.
      synopsis       = text("LongDescription").orElse(text("Description")),
      cast           = list("Actors"),
      director       = list("Directors"),
      runtimeMinutes = (root \ "Duration").asOpt[Int].filter(_ > 0),
      genres         = Seq(text("GenrePrimary"), text("GenreSecondary")).flatten.distinct,
      posterUrl      = text("GraphicUrl"),
      trailerUrl     = text("TrailerUrl"),
      ageRating      = AgeRating.normalize(text("Rating"))
    )
  }
}
