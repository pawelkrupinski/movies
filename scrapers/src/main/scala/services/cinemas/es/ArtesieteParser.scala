package services.cinemas.es

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import services.cinemas.common.ScraperParse
import services.cinemas.es.SpanishFeedText.{certificate, field, fold}
import scala.util.Try

/**
 * Pure page → `CinemaMovie` transformation for an Artesiete venue's own page
 * (see [[ArtesieteClient]]). The page is a Vue shell whose component props carry
 * the whole programme as HTML-escaped JSON: `:fullsessionsinfo='[…]'` is every
 * session the venue sells — one object per session with the film's sheet
 * repeated on it (title, original title, runtime, certificate, cast, synopsis),
 * the room (`NombreSala`), the format (`NombreFormato`) and the start
 * (`HoraReal`, the venue's wall clock). A row is the film (`ID_Espectaculo`),
 * its showtimes its sessions.
 *
 * The same page also embeds every venue's back-office row (`:theaters`, with
 * credentials in it); nothing here reads it, and the recorded fixture keeps
 * only the sessions prop.
 */
object ArtesieteParser {

  val SessionsProp = """:fullsessionsinfo='([^']*)'""".r
  private val Stamp        = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss[.SSS]")

  /** The page's sessions prop, unescaped — `None` when the page has none (not a
   *  venue page: the client fails the scrape rather than read it as empty). */
  def sessionsJson(page: String): Option[String] =
    SessionsProp.findFirstMatchIn(page).map(m => org.jsoup.parser.Parser.unescapeEntities(m.group(1), true))

  /** The page → one row per film with a session. Throws when the page carries no
   *  sessions prop; an empty `[]` is a venue selling nothing. */
  def films(page: String, baseUrl: String, cinema: Cinema): Seq[CinemaMovie] = {
    val json = sessionsJson(page)
      .getOrElse(throw new IllegalStateException(s"Artesiete page carries no sessions: ${page.take(200)}"))
    Json.parse(json).as[Seq[JsValue]]
      .filter(s => field(s, "VisibleEnWeb").forall(_ == "1"))
      .groupBy(s => field(s, "ID_Espectaculo"))
      .toSeq.collect { case (Some(id), sessions) => film(id, sessions, baseUrl, cinema) }
      .flatten.sortBy(_.movie.title)
  }

  private def film(id: String, sessions: Seq[JsValue], baseUrl: String, cinema: Cinema): Option[CinemaMovie] = {
    val first = sessions.head
    val title = field(first, "Titulo").orElse(field(first, "NombreEspectaculo"))
      .getOrElse(throw new IllegalStateException(s"Artesiete film $id has no title"))
    val showtimes = sessions.flatMap(session(_, baseUrl)).distinctBy(s => (s.dateTime, s.room)).sortBy(s => (s.dateTime, s.room.getOrElse("")))
    Option.when(showtimes.nonEmpty)(CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = field(first, "Duracion").flatMap(_.toIntOption).filter(_ > 0),
        genres         = field(first, "NombreGenero").toSeq,
        originalTitle  = field(first, "TituloOriginal").filterNot(_.equalsIgnoreCase(title)),
      ),
      cinema      = cinema,
      posterUrl   = field(first, "Cartel").map(file => s"$baseUrl/Posters/$file"),
      filmUrl     = Some(s"$baseUrl/FilmPage/$id/${segment(title)}"),
      synopsis    = field(first, "Sinopsis"),
      cast        = field(first, "Interpretes").toSeq.flatMap(ScraperParse.commaItems),
      director    = Nil,
      showtimes   = showtimes,
      externalIds = Map("artesiete" -> id),
      trailerUrl  = field(first, "Video"),
      ageRating   = field(first, "NombreCalificacion").flatMap(certificate),
    ))
  }

  /** One session → a showtime at its real start. Its booking link is the one the
   *  page's own session button opens — `/Session/<venue>/<venue>/<film>/<title>/<pase>`
   *  — offered only for a session on sale online, as the page does. */
  private def session(s: JsValue, baseUrl: String): Option[Showtime] =
    field(s, "HoraReal").flatMap(t => Try(LocalDateTime.parse(t, Stamp)).toOption).map { start =>
      val booking = for {
        venue <- field(s, "ID_Centro")
        film  <- field(s, "ID_Espectaculo")
        name  <- field(s, "NombreEspectaculo")
        pase  <- field(s, "ID_Pase")
        if field(s, "VentaRemota").contains("1")
      } yield s"$baseUrl/Session/$venue/$venue/$film/${segment(name)}/$pase"
      Showtime(start, booking, field(s, "NombreSala"), formatTokens(field(s, "NombreFormato").getOrElse("")))
    }

  /** A title as the page's own links spell it in a path: "¿" and "?" dropped,
   *  the rest percent-encoded. */
  private def segment(title: String): String =
    URLEncoder.encode(title.replaceAll("[¿?]", ""), StandardCharsets.UTF_8).replace("+", "%20")

  /** `NombreFormato` → format tokens, screen first and version last. The chain
   *  composes it from words — "4K Laser Atmos 3D", "Digital VO", "Láser VO",
   *  "INGLES SUB ESPANOL" — so each known word maps on its own; "Digital" is the
   *  baseline and earns nothing, and an unknown word (a "Hora Silenciosa"
   *  relaxed session) is dropped. */
  def formatTokens(format: String): List[String] = {
    val words = fold(format).split("[^a-z0-9]+").filter(_.nonEmpty).toList
    val screen = words.flatMap(ScreenWords.get)
    val infinity = Option.when(words.containsSlice(List("infinity", "vision")))("INFINITY").toList
    val version =
      if (words.contains("sub")) List("VOSE")
      else if (words.contains("vose")) List("VOSE")
      else if (words.contains("vo")) List("VO")
      else Nil
    (ScreenOrder.filter((screen ++ infinity).contains) ++ version).distinct
  }

  private val ScreenWords = Map("3d" -> "3D", "4k" -> "4K", "laser" -> "LASER", "atmos" -> "ATMOS")
  private val ScreenOrder = List("3D", "4K", "LASER", "INFINITY", "ATMOS")
}
