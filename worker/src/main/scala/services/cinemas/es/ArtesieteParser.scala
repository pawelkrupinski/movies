package services.cinemas.es

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale
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
      .filter(s => text(s, "VisibleEnWeb").forall(_ == "1"))
      .groupBy(s => text(s, "ID_Espectaculo"))
      .toSeq.collect { case (Some(id), sessions) => film(id, sessions, baseUrl, cinema) }
      .flatten.sortBy(_.movie.title)
  }

  private def film(id: String, sessions: Seq[JsValue], baseUrl: String, cinema: Cinema): Option[CinemaMovie] = {
    val first = sessions.head
    val title = text(first, "Titulo").orElse(text(first, "NombreEspectaculo"))
      .getOrElse(throw new IllegalStateException(s"Artesiete film $id has no title"))
    val showtimes = sessions.flatMap(session(_, baseUrl)).distinctBy(s => (s.dateTime, s.room)).sortBy(s => (s.dateTime, s.room.getOrElse("")))
    Option.when(showtimes.nonEmpty)(CinemaMovie(
      movie = Movie(
        title          = title,
        runtimeMinutes = text(first, "Duracion").flatMap(_.toIntOption).filter(_ > 0),
        genres         = text(first, "NombreGenero").toSeq,
        originalTitle  = text(first, "TituloOriginal").filterNot(_.equalsIgnoreCase(title)),
      ),
      cinema      = cinema,
      posterUrl   = text(first, "Cartel").map(file => s"$baseUrl/Posters/$file"),
      filmUrl     = Some(s"$baseUrl/FilmPage/$id/${segment(title)}"),
      synopsis    = text(first, "Sinopsis"),
      cast        = text(first, "Interpretes").toSeq.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty)),
      director    = Nil,
      showtimes   = showtimes,
      externalIds = Map("artesiete" -> id),
      trailerUrl  = text(first, "Video"),
      ageRating   = text(first, "NombreCalificacion").flatMap(ageLabel),
    ))
  }

  /** One session → a showtime at its real start. Its booking link is the one the
   *  page's own session button opens — `/Session/<venue>/<venue>/<film>/<title>/<pase>`
   *  — offered only for a session on sale online, as the page does. */
  private def session(s: JsValue, baseUrl: String): Option[Showtime] =
    text(s, "HoraReal").flatMap(t => Try(LocalDateTime.parse(t, Stamp)).toOption).map { start =>
      val booking = for {
        venue <- text(s, "ID_Centro")
        film  <- text(s, "ID_Espectaculo")
        name  <- text(s, "NombreEspectaculo")
        pase  <- text(s, "ID_Pase")
        if text(s, "VentaRemota").contains("1")
      } yield s"$baseUrl/Session/$venue/$venue/$film/${segment(name)}/$pase"
      Showtime(start, booking, text(s, "NombreSala"), formatTokens(text(s, "NombreFormato").getOrElse("")))
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

  /** The certificate the way SensaCine prints it: "+16" for "No Recomendada para
   *  Menores de 16 años" and "Mayores de 7 años", "APTA" for "Todos los Públicos…";
   *  none while it is "Pendiente de Calificacion". */
  private def ageLabel(raw: String): Option[String] = {
    val folded = fold(raw)
    if (folded.startsWith("pendiente")) None
    else if (folded.startsWith("todos los publicos")) Some("APTA")
    else Years.findFirstMatchIn(folded).map(m => s"+${m.group(1)}").orElse(Some(raw.trim.toUpperCase(Locale.ROOT)))
  }
  private val Years = """(\d{1,2})\s*anos""".r

  private def fold(s: String): String =
    java.text.Normalizer.normalize(s.toLowerCase(Locale.ROOT), java.text.Normalizer.Form.NFD).replaceAll("\\p{M}", "")

  private def text(js: JsValue, key: String): Option[String] =
    (js \ key).toOption.collect {
      case JsString(s) => s.trim
      case JsNumber(n) => n.toString
    }.filter(_.nonEmpty)
}
