package services.cinemas.common

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import services.movies.TrailerEmbed

import java.time.LocalDateTime
import scala.util.Try

/**
 * Pure JSON → `Seq[CinemaMovie]` for the Vue Cinemas platform films endpoint
 * (`{base}/api/microservice/showings/cinemas/{id}/films`) — shared verbatim by
 * Vue UK (`myvue.com`) and CinemaxX Germany (`cinemaxx.de`), which run the same
 * backend. No I/O: [[VueCinemasPlatformClient]] fetches the body and hands it
 * here, so parsing is unit-testable against a recorded fixture.
 *
 * `baseUrl` is needed only to absolutise the relative `bookingUrl`
 * (`/book-tickets/summary/10032/HO00020282/398489` on Vue,
 * `/buchtickets/zusammenfassung/1107/...` on CinemaxX).
 */
object VueCinemasPlatformParser {

  def parse(json: String, cinema: Cinema, baseUrl: String): Seq[CinemaMovie] = {
    val root = baseUrl.stripSuffix("/")
    (Json.parse(json) \ "result").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .map(parseFilm(_, cinema, root))
  }

  private def parseFilm(film: JsValue, cinema: Cinema, root: String): CinemaMovie = {
    val title    = (film \ "filmTitle").as[String]
    val sessions = (film \ "showingGroups").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(g => (g \ "sessions").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty))
    CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = (film \ "runningTime").asOpt[Int].filter(_ > 0),
        // `releaseDate` is the LOCAL theatrical (re-)release date, not the
        // production year — a 2000 film re-run this summer comes back as
        // 2026-xx-xx. Feeding that to TMDB's year-scoped search excludes the
        // actual film and lets a same-titled modern release win, so leave the
        // year unset and let TMDB rank on title (same call MultikinoParser
        // makes against the same upstream schema).
        releaseYear    = None,
        genres         = (film \ "genres").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
                           .flatMap(g => (g \ "name").asOpt[String].orElse(g.asOpt[String]))
                           .filter(_.nonEmpty),
        // Non-empty on decorated runs ("Chicken Run (2000)" → "Chicken Run",
        // "La La Land (10th Anniversary)" → "La La Land"), which is exactly the
        // TMDB-search fallback it's there for.
        originalTitle  = (film \ "originalTitle").asOpt[String].filter(_.nonEmpty).filter(_ != title)
      ),
      cinema      = cinema,
      posterUrl   = absoluteUrl((film \ "posterImageSrc").asOpt[String], root),
      filmUrl     = absoluteUrl((film \ "filmUrl").asOpt[String], root),
      synopsis    = (film \ "synopsisShort").asOpt[String].filter(_.nonEmpty)
                      .map(tools.TextNormalization.stripHtml),
      cast        = splitPeople((film \ "cast").asOpt[String]),
      director    = splitPeople((film \ "director").asOpt[String]),
      showtimes   = sessions.flatMap(parseSession(_, root)),
      // `filmId` is the venue-facing booking id ("HO00020282"); `movieXchangeCode`
      // is the cross-brand MovieXchange uuid, the same key MultikinoParser stores
      // under "mxc" — one film, one key across both platforms.
      externalIds = ((film \ "filmId").asOpt[String].filter(_.nonEmpty).map("vue" -> _) ++
                     (film \ "movieXchangeCode").asOpt[String].filter(_.nonEmpty).map("mxc" -> _)).toMap,
      trailerUrl  = (film \ "trailers").asOpt[JsArray].flatMap(_.value.headOption)
                      .flatMap(_.asOpt[String]).filter(_.nonEmpty)
                      .flatMap(u => TrailerEmbed.youTubeId(u).map(id => s"https://www.youtube.com/watch?v=$id"))
    )
  }

  private def parseSession(session: JsValue, root: String): Option[Showtime] =
    (session \ "startTime").asOpt[String].flatMap(parseLocalDateTime).map { dateTime =>
      Showtime(
        dateTime   = dateTime,
        // Relative on both brands — absolutised against the brand's base URL.
        bookingUrl = absoluteUrl((session \ "bookingUrl").asOpt[String], root),
        room       = (session \ "screenName").asOpt[String].filter(_.nonEmpty),
        format     = formatTokens(session)
      )
    }

  /** Session `attributes[]` → [[Showtime.format]] tokens.
   *
   *  Each attribute carries an `attributeType` (`Session`, `Session_Special`,
   *  `Language`, `Movie`) plus a stable machine `value`. Only the `Session*`
   *  types describe THIS screening's presentation; `Movie` attributes ("Strobe
   *  Lighting", "Family", "Anime") are properties of the film and would be the
   *  same on every row, so they're dropped.
   *
   *  Matching is on `value`, not the display `name`, because the name is
   *  localised per brand while the value isn't — CinemaxX's "Maxximum 3D" and
   *  Vue's "3D" are both `3d`. [[FormatTokens]] order is fixed here (projection
   *  → picture/sound → language/accessibility) rather than taken from the JSON,
   *  so the same screening always yields the same list.
   *
   *  Language: German venues mark an original-with-subtitles screening either as
   *  the `Session` attribute `om-u` or via a `Language` attribute whose name
   *  says "…Untertiteln" — both map to `OmU`, the token
   *  `services.cinemas.de.WebediaShowtimesClient` already uses for Germany. A
   *  screening in the venue's default language gets no language token (matching
   *  Multikino's and Webedia's behaviour), so UK rows stay untagged. */
  private def formatTokens(session: JsValue): List[String] = {
    val attributes   = (session \ "attributes").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
    val presentation = attributes
      .filter(a => (a \ "attributeType").asOpt[String].exists(_.startsWith(SessionAttributePrefix)))
      .flatMap(a => (a \ "value").asOpt[String]).map(_.toLowerCase).toSet
    val subtitled = presentation.contains(SubtitledValue) || attributes.exists(a =>
      (a \ "attributeType").asOpt[String].contains(LanguageAttributeType) &&
        (a \ "name").asOpt[String].exists(_.toLowerCase.contains(SubtitleWord)))
    val tokens = FormatTokens.collect { case (value, token) if presentation.contains(value) => token }
    (if (subtitled) tokens :+ SubtitledToken else tokens).distinct
  }

  /** Attribute `value` → format token, in the order tokens are emitted. Data,
   *  not a chain of `if`s, so a new premium format is one line. Every entry was
   *  observed live on 2026-07-27 (Vue Islington + CinemaxX Berlin). */
  private val FormatTokens: List[(String, String)] = List(
    "epic"           -> "EPIC",     // Vue's large-format screen brand
    "3d"             -> "3D",       // Vue "3D" / CinemaxX "Maxximum 3D"
    "2d"             -> "2D",       // only CinemaxX spells the default out
    "atmos"          -> "ATMOS",
    "hdr"            -> "HDR",      // "HDR by Barco"
    "laser"          -> "LASER",
    "open-captioned" -> "OC",       // on-screen captions for deaf/HoH customers
    "audio"          -> "AD"        // audio description track
  )

  private val SessionAttributePrefix = "Session"      // covers Session + Session_Special
  private val LanguageAttributeType  = "Language"
  private val SubtitledValue         = "om-u"         // CinemaxX "Original mit Untertiteln"
  private val SubtitleWord           = "untertitel"
  private val SubtitledToken         = "OmU"

  /** Cast/director arrive as one comma-separated string on both brands. */
  private def splitPeople(raw: Option[String]): Seq[String] =
    raw.filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty).toSeq)

  private def absoluteUrl(url: Option[String], root: String): Option[String] =
    url.map(_.trim).filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else root + u)

  private def parseLocalDateTime(raw: String): Option[LocalDateTime] =
    Try(LocalDateTime.parse(raw.trim)).toOption
}
