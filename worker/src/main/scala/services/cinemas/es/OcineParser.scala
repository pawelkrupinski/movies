package services.cinemas.es

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.Locale
import scala.util.Try

/**
 * Pure JSON → `CinemaMovie` transformation for an Ocine venue's own ticketing
 * server (see [[OcineClient]] for the two endpoints). No I/O: the client fetches
 * each body and hands it here, so every rule is unit-testable against a
 * recorded fixture.
 *
 * A FILM IS A GROUP OF VARIANTS. The ticketing system models every way a film
 * is shown — 2D, 3D, ATMOS, VOSE, Catalan, 4D, the Premium room — as its own
 * "sub-película" with its own id and a decorated title ("Spider-man: Brand New
 * Day (3D) (ATMOS)"), and groups them under one parent (`esGrup: true`) that
 * carries the clean title and the film-level metadata. The parent's own
 * `sessions` is null; the screenings sit on the variants. So a row is the
 * PARENT, its showtimes the union of every variant's, each tagged with the
 * formats of the variant it came from. A film with a single way of being shown
 * (a live-event relay) is not grouped (`esGrup: false`) and carries its
 * screenings on itself.
 */
object OcineParser {

  /** Every film id the venue's cartelera names — the running programme
   *  (`pelicules`), the upcoming releases (`estrenes`) and the advance sales
   *  (`anticipades`) — deduplicated, in first-seen order. All three, because an
   *  advance-sale film is exactly the tail a scrape must not lose (see
   *  `ScrapeHorizon`), and a listed release costs one request to find out it has
   *  nothing on sale yet.
   *
   *  `None` when the body is not the cartelera at all — no `pelicules` array —
   *  so the client can fail the scrape rather than read an error page as "the
   *  venue shows nothing". An empty array is a genuinely empty venue. */
  def filmIds(json: String): Option[Seq[String]] = {
    val root = Try(Json.parse(json)).getOrElse(JsNull)
    (root \ "pelicules").asOpt[Seq[JsValue]].map { running =>
      val others = Seq("estrenes", "anticipades").flatMap(k => (root \ k).asOpt[Seq[JsValue]].getOrElse(Nil))
      (running ++ others).flatMap(p => (p \ "id").asOpt[Long]).distinct.map(_.toString)
    }
  }

  /** One film's detail body → its row at this venue, or nothing when it has no
   *  screening on sale (an announced release). Throws on a body that is not a
   *  film at all (no `id`/`titol`) — a fetch that answered with the wrong thing
   *  must fail this film's chunk so it is retried, not silently drop the film.
   *
   *  `notAfter` is the sanity bound of `ScrapeHorizon` — a guard against a
   *  garbage far date, never a coverage window. */
  def film(json: String, baseUrl: String, cinema: Cinema, notAfter: LocalDate): Option[CinemaMovie] = {
    val root     = Json.parse(json)
    val id       = (root \ "id").asOpt[Long].getOrElse(throw new IllegalStateException(s"Ocine film body has no id: ${json.take(200)}"))
    val rawTitle = text(root, "titol").getOrElse(throw new IllegalStateException(s"Ocine film $id has no title"))
    val title    = cleanTitle(rawTitle)

    val variants = root +: (root \ "subPelicules").asOpt[Seq[JsValue]].getOrElse(Nil)
    val showtimes = variants
      .flatMap { v =>
        val format = formatTokens((v \ "propietatsDesc").asOpt[Seq[String]].getOrElse(Nil))
        (v \ "sessions").asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(session(_, baseUrl, format))
      }
      .filterNot(_.dateTime.toLocalDate.isAfter(notAfter))
      .distinctBy(s => (s.dateTime, s.bookingUrl))
      .sortBy(s => (s.dateTime, s.room.getOrElse("")))

    Option.when(showtimes.nonEmpty) {
      val movie = Movie(
        title          = title,
        runtimeMinutes = text(root, "durada").flatMap(_.toIntOption).filter(_ > 0),
        genres         = text(root, "genereComercial").toSeq,
        originalTitle  = text(root, "titolOriginal").filterNot(_.equalsIgnoreCase(title)),
        rawTitle       = Option.when(title != rawTitle)(rawTitle),
      )
      CinemaMovie(
        movie       = movie,
        cinema      = cinema,
        // The payload's `poster` is the JPEG itself, base64-inlined (~30KB per
        // film), not a URL — there is nothing linkable to store. TMDB supplies
        // the poster downstream.
        posterUrl   = None,
        filmUrl     = Some(s"$baseUrl/#/DetallPelicula/$id"),
        synopsis    = text(root, "sinopsis"),
        cast        = Seq("actor1", "actor2", "actriu1", "actriu2").flatMap(text(root, _)),
        director    = text(root, "director").toSeq.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty)),
        showtimes   = showtimes,
        externalIds = Map("ocine" -> id.toString),
        trailerUrl  = text(root, "videoExtern"),
        ageRating   = text(root, "classificacio").map(ageLabel),
      )
    }
  }

  /** One session → a showtime. `data` + `hora` are the venue's own wall clock.
   *  The booking link is the one the site's own cartelera navigates to when a
   *  time is picked: the ticketing app's root, told which screening (`plan`, the
   *  session's `planificacio`) it was entered for. */
  private def session(s: JsValue, baseUrl: String, format: List[String]): Option[Showtime] =
    for {
      date <- (s \ "data").asOpt[String].flatMap(d => Try(LocalDate.parse(d)).toOption)
      time <- (s \ "hora").asOpt[String].flatMap(h => Try(LocalTime.parse(h)).toOption)
    } yield Showtime(
      LocalDateTime.of(date, time),
      (s \ "planificacio").asOpt[Long].map(plan => s"$baseUrl/?plan=$plan&extern=1&idioma=es"),
      text(s, "nomSala"),
      format,
    )

  /** The site's `propietatsDesc` vocabulary → format tokens, screen first and the
   *  language version last, the order the Webedia clients use. The baseline
   *  every session carries ("Estándar", "Digital") says nothing and is dropped;
   *  an unmapped value is kept, upper-cased, because a NEW room type or format
   *  must not vanish from the row — it is the screening's only distinguishing
   *  mark. The version tokens are SensaCine's own, so a Spanish visitor reads
   *  the same badge whichever source served the venue. The unmarked version is
   *  the Castilian one (dubbed or native), which earns no badge. */
  def formatTokens(descriptions: Seq[String]): List[String] = {
    val known = descriptions.flatMap(d => Tokens.get(normalise(d)).map(d -> _))
    val screen = ScreenOrder.filter(t => known.exists(_._2 == t))
    val unknown = descriptions
      .filterNot(d => Baseline.contains(normalise(d)) || Tokens.contains(normalise(d)))
      .map(_.trim.toUpperCase(Locale.ROOT)).filter(_.nonEmpty)
    val version = VersionOrder.filter(t => known.exists(_._2 == t))
    (screen ++ unknown ++ version).distinct
  }

  private val Baseline: Set[String] = Set("estándar", "estandar", "digital", "analógica", "analogica")

  private val Tokens: Map[String, String] = Map(
    "2d"               -> "2D",
    "3d"               -> "3D",
    "sala 4d"          -> "4D",
    "screen x"         -> "SCREENX",
    "infinity vision"  -> "INFINITY",
    "sala ice"         -> "ICE",
    "sala premium"     -> "PREMIUM",
    "urban"            -> "URBAN",
    "sala kids"        -> "KIDS",
    "atmos"            -> "ATMOS",
    "versión original" -> "VO",
    "versión vose"     -> "VOSE",
    "versión catalán"  -> "CAT",
    "versión euskera"  -> "EUS",
  )
  private val ScreenOrder  = List("2D", "3D", "4D", "SCREENX", "INFINITY", "ICE", "PREMIUM", "URBAN", "KIDS", "ATMOS")
  private val VersionOrder = List("VO", "VOSE", "CAT", "EUS")

  private def normalise(s: String): String = s.trim.toLowerCase(Locale.ROOT)

  /** The film's title without the variant markers the box office appends to
   *  name a way of showing it — "(Català)", "(VOSE)", "(3D)", "(ATMOS)", "(4D)",
   *  "(IV)" — which the session's format tokens already carry. A variant's
   *  title always has them; a film's usually doesn't, but one shown ONLY in
   *  Catalan is titled like its sole variant ("Detectiu conan: L'àngel caigut de
   *  la carretera (Català)"), and the marker would otherwise reach the title
   *  search. Only a trailing run of known markers goes: a parenthesis that is
   *  part of the real title is never one of them. */
  def cleanTitle(raw: String): String = {
    val stripped = TrailingMarker.replaceFirstIn(raw, "").trim
    if (stripped.isEmpty) raw.trim else stripped
  }

  private val TrailingMarker =
    """(?iu)(?:\s*[(\[](?:català|catalán|catala|vose|vos|vo|v\.o\.s\.e\.|euskera|2d|3d|4d|4dx|atmos|iv|screen\s*x|kids|urban|ice|premium|sala\s+[\p{L}\d ]+)[)\]])+\s*$""".r

  /** "12 años" → "+12", the way SensaCine and a Spanish listing print it;
   *  a word ("Apta") is kept, upper-cased to SensaCine's spelling. */
  private def ageLabel(raw: String): String =
    AgeYears.findFirstMatchIn(raw).map(m => s"+${m.group(1)}").getOrElse(raw.toUpperCase(Locale.ROOT))

  private val AgeYears = """^(\d+)\s*(?:años|anys)?$""".r

  private def text(js: JsValue, key: String): Option[String] =
    (js \ key).asOpt[String].map(_.trim).filter(_.nonEmpty)
}
