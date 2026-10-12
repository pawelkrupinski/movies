package services.cinemas.es

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.Locale
import scala.util.Try

/**
 * Pure JSON → `CinemaMovie` transformation for taquilla.com's venue endpoints
 * (see [[TaquillaClient]]). No I/O, so every rule is unit-testable against a
 * recorded fixture.
 *
 * A day's body is `objects[]` — one EVENT per film and language version shown
 * that day, each with its `sessions[]` (time, the session's own name, and the
 * chain's ticketing link) — plus `entity_map`, the films' catalogue rows
 * (original title, director, cast, runtime, certificate, synopsis). An event
 * belongs to a film by `main_entity_id`; a film shown both dubbed and in VOSE is
 * two events, so a row is the film, its showtimes the union of its events',
 * each tagged with the markers its session name carries.
 */
object TaquillaParser {

  /** Every day the venue sells, off the calendar endpoint — a day with a
   *  zero count is not one. `None` when the body is JSON but no calendar (no
   *  `objects`), and a throw when it is not JSON at all, so the client fails the
   *  scrape instead of reading an error page as an empty venue. A venue selling
   *  nothing answers an empty `[]` or `{}`: an empty listing, not a failure. */
  def days(json: String): Option[Seq[LocalDate]] = {
    val root = Json.parse(json)
    (root \ "objects").toOption.collect {
      case JsObject(days) =>
        days.collect { case (day, JsNumber(n)) if n > 0 => Try(LocalDate.parse(day)).toOption }.flatten.toSeq.sorted
      case JsArray(empty) if empty.isEmpty => Nil
    }
  }

  /** One day's events → that day's rows, one per film. Throws on a body that is
   *  not a day's listing (no `objects` array) so the day's chunk is retried. */
  def day(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    val root    = Json.parse(json)
    val events  = (root \ "objects").asOpt[Seq[JsValue]]
      .getOrElse(throw new IllegalStateException(s"taquilla.com day body has no events: ${json.take(200)}"))
    val catalog = (root \ "entity_map").asOpt[JsObject].getOrElse(Json.obj())
    events.groupBy(e => (e \ "main_entity_id").asOpt[Long]).toSeq.flatMap {
      case (None, _)          => Nil
      case (Some(id), shown)  => film(id, shown, (catalog \ id.toString \ "details" \ "movie_info").toOption, cinema)
    }.sortBy(_.movie.title)
  }

  private def film(id: Long, events: Seq[JsValue], info: Option[JsValue], cinema: Cinema): Option[CinemaMovie] = {
    val first     = events.head
    val rawTitle  = text(first, "entity_name").orElse(text(first, "event_name"))
      .getOrElse(throw new IllegalStateException(s"taquilla.com film $id has no name"))
    val title     = cleanTitle(rawTitle)
    val showtimes = events.flatMap { e =>
      val date = text(e, "datetime").flatMap(d => Try(LocalDate.parse(d.take(10))).toOption)
      (e \ "sessions").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(s => date.flatMap(session(_, s, title)))
    }.distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)

    def meta(key: String): Option[String] = info.flatMap(text(_, key)).filterNot(_ == "0")
    Option.when(showtimes.nonEmpty) {
      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = meta("duration").flatMap(_.toIntOption).filter(_ > 0),
          releaseYear    = meta("date_published").flatMap(_.take(4).toIntOption),
          countries      = list(meta("country_of_origin")),
          genres         = list(meta("genres")),
          originalTitle  = meta("original_title").filterNot(_.equalsIgnoreCase(title)),
          rawTitle       = Option.when(title != rawTitle)(rawTitle),
        ),
        cinema      = cinema,
        posterUrl   = text(first, "entity_img_url"),
        filmUrl     = text(first, "entity_name_url").map(slug => s"${TaquillaClient.BaseUrl}/entradas/$slug"),
        synopsis    = meta("synopsis"),
        cast        = list(meta("actors")),
        director    = list(meta("director")),
        showtimes   = showtimes,
        externalIds = Map("taquilla" -> id.toString),
        trailerUrl  = meta("trailer"),
        ageRating   = meta("content_rating").flatMap(ageLabel),
      )
    }
  }

  /** One session → a showtime: the event's day at the session's wall-clock time,
   *  booked at the chain's own ticketing page the session links (Taquilla's
   *  redirect when it names none). Its markers are its name's, plus the version
   *  the ticketing link asks for — entradas.com's `language=VOSE` names a VOSE
   *  session its plain name ("Vengadores: Doomsday") does not. */
  private def session(date: LocalDate, s: JsValue, title: String): Option[Showtime] =
    text(s, "time").flatMap(t => Try(LocalTime.parse(t)).toOption).map { time =>
      val sellLink = (s \ "p").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(text(_, "sell_link")).headOption
        .map(_.replace("&amp;", "&"))
      val linked   = sellLink.flatMap(LinkLanguage.findFirstMatchIn(_)).map(m => s" (${m.group(1)})").getOrElse("")
      Showtime(LocalDateTime.of(date, time), sellLink.orElse(text(s, "sminprice_url")), None,
        formatTokens(text(s, "sname").getOrElse("") + linked, title))
    }
  private val LinkLanguage = """[?&]language=([A-Za-z.]+)""".r

  /** The markers the session's name adds to the film's — "VOSE Clayface",
   *  "Vengadores: Doomsday (Infinity Vision) - PREMIUM", "CAT Cronos" — as format
   *  tokens, the screen first and the version last, the spellings SensaCine's
   *  Spanish venues use. Only the markers the film's own title lacks count, so a
   *  film whose title has "3D" in it is not a 3D screening for that. An unknown
   *  marker (Yelmo's "XE") earns nothing. */
  def formatTokens(sessionName: String, filmTitle: String): List[String] = {
    val own   = fold(filmTitle)
    val extra = Markers.filter { case (pattern, _) =>
      pattern.findFirstIn(fold(sessionName)).isDefined && pattern.findFirstIn(own).isEmpty
    }.map(_._2)
    (ScreenOrder.filter(extra.contains) ++ VersionOrder.filter(extra.contains)).distinct
  }

  private val Markers: Seq[(scala.util.matching.Regex, String)] = Seq(
    """\b3d\b""".r                                   -> "3D",
    """\bimax\b""".r                                 -> "IMAX",
    """\b4dx\b""".r                                  -> "4DX",
    """\bscreen\s?x\b""".r                           -> "SCREENX",
    """\binfinity\s+vision\b""".r                    -> "INFINITY",
    """\bpremium\b""".r                              -> "PREMIUM",
    """\batmos\b""".r                                -> "ATMOS",
    // Yelmo prefixes the version ("VOSE Clayface"); Kinépolis spells it out
    // ("INGLÉS SUBTITULADO EN ESPAÑOL (VOSE)", "SUBTÍTULOS ESPAÑOL").
    """\b(?:vose|v\.o\.s\.e\.?|subtitulad[oa]|subtitulos)(?=\W|$)""".r -> "VOSE",
    """\(vo\)|\bv\.o\.(?=\W|$)""".r                  -> "VO",
    """\b(?:catalan|catala|vdcat|v\.d\.cat\.?|cat\.?)(?=\W|$)""".r -> "CAT",
    """\beuskera\b""".r                              -> "EUS",
  )
  private val ScreenOrder  = List("3D", "IMAX", "4DX", "SCREENX", "INFINITY", "PREMIUM", "ATMOS")
  private val VersionOrder = List("VO", "VOSE", "CAT", "EUS")

  /** The film's name without the "(Cine)" Taquilla files an event-cinema relay
   *  under ("Ballet El Cascanueces (Cine)", "Origen (Cine)") — its own catalogue
   *  category, not part of any title. */
  def cleanTitle(raw: String): String = {
    val stripped = CinemaTag.replaceFirstIn(raw, "").trim
    if (stripped.isEmpty) raw.trim else stripped
  }
  private val CinemaTag = """(?i)\s*\(cine\)\s*$""".r

  /** Lower-cased, accents off: "Catalán" and "CATALÁN" are one marker. */
  private def fold(s: String): String =
    java.text.Normalizer.normalize(s.toLowerCase(Locale.ROOT), java.text.Normalizer.Form.NFD).replaceAll("\\p{M}", "")

  /** The certificate the way SensaCine prints it: "+12" for "M-12" and "NO
   *  RECOMENDADA PARA MENORES DE 12 AÑOS", "APTA" for "TP" and "APTA PARA TODOS LOS
   *  PÚBLICOS"; none while it is pending ("PDC", "EDAD PENDIENTE DE CALIFICACIÓN")
   *  or a placeholder ("-"). */
  def ageLabel(raw: String): Option[String] = {
    val folded = fold(raw.trim)
    if (folded == "pdc" || folded.contains("pendiente") || !folded.exists(_.isLetterOrDigit)) None
    else if (folded == "tp" || folded.contains("todos los publicos")) Some("APTA")
    else Years.findFirstMatchIn(folded).map(m => s"+${m.group(1)}").orElse(Some(raw.trim.toUpperCase(Locale.ROOT)))
  }
  private val Years = """(?:^m-|^\+|menores de )(\d{1,2})""".r

  private def list(raw: Option[String]): Seq[String] =
    raw.toSeq.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty))

  private def text(js: JsValue, key: String): Option[String] =
    (js \ key).toOption.collect {
      case JsString(s) => s.trim
      case JsNumber(n) => n.toString
    }.filter(_.nonEmpty)
}
