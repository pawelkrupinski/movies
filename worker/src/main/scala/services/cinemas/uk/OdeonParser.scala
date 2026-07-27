package services.cinemas.uk

import models._
import play.api.libs.json._

import java.time.{LocalDate, OffsetDateTime}
import scala.util.Try

/**
 * Pure parsers for Odeon's Vista `WSVistaWebClient/ocapi/v1` JSON — kept out of
 * [[OdeonClient]] so a spec can feed recorded payloads straight in (no HTTP, no
 * auth). Three shapes, one per endpoint the client touches:
 *
 *   - `sites`                                   → [[parseSites]]      (roster self-check)
 *   - `film-screening-dates?siteIds=<id>`       → [[parseScreeningDates]] (the horizon)
 *   - `showtimes/by-business-date/<date>?siteIds=<id>` → [[parseShowtimes]] (a day's films)
 *
 * The showtimes payload is self-contained: `showtimes[]` reference films,
 * screens and attributes by id, resolved against the `relatedData` blocks in the
 * same response — so one call yields fully-formed [[CinemaMovie]]s.
 */
object OdeonParser {

  /** The public, human-facing Odeon site (Cloudflare-protected, so the client
   *  never fetches it — used only to mint the booking and film URLs the ocapi
   *  ids point at, exactly as the www booking flow builds them). */
  val Domain = "https://www.odeon.co.uk"

  /** One venue in the `sites` roster — id + display name. The roster carries no
   *  public slug, so it's used to self-check which `siteId`s exist (against
   *  `ODEON-VENUE-MAP.tsv`), not to build URLs. */
  case class OdeonSite(id: String, name: String)

  def parseSites(json: String): Seq[OdeonSite] =
    (Json.parse(json) \ "sites").asOpt[JsArray].map(_.value).getOrElse(Seq.empty).flatMap { s =>
      for {
        id   <- (s \ "id").asOpt[String]
        name <- (s \ "name" \ "text").asOpt[String]
      } yield OdeonSite(id, name)
    }.toSeq

  /** The business dates a venue has a programme on — the client's horizon. Each
   *  `filmScreeningDates[]` entry is one date; deduped + sorted. */
  def parseScreeningDates(json: String): Seq[LocalDate] =
    (Json.parse(json) \ "filmScreeningDates").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
      .flatMap(d => (d \ "businessDate").asOpt[String])
      .flatMap(s => Try(LocalDate.parse(s)).toOption)
      .distinct.sorted.toSeq

  /** A day's showtimes → one [[CinemaMovie]] per film, showtimes deduped by
   *  (time, booking, room, format) and time-ordered. Films/screens/attributes
   *  are resolved out of `relatedData`. */
  def parseShowtimes(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    val root        = Json.parse(json)
    val relatedData = root \ "relatedData"

    def relatedById(block: String): Map[String, JsValue] =
      (relatedData \ block).asOpt[JsArray].map(_.value).getOrElse(Seq.empty)
        .flatMap(v => (v \ "id").asOpt[String].map(_ -> v)).toMap

    val films      = relatedById("films")
    val screens    = relatedById("screens")
    val attributes = relatedById("attributes")

    def attributeName(id: String): Option[String] =
      attributes.get(id).flatMap(a => (a \ "name" \ "text").asOpt[String])

    val showtimes = (root \ "showtimes").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)

    showtimes.flatMap { st =>
      for {
        filmId  <- (st \ "filmId").asOpt[String]
        film    <- films.get(filmId)
        title   <- (film \ "title" \ "text").asOpt[String].map(_.trim).filter(_.nonEmpty)
        startsAt <- (st \ "schedule" \ "startsAt").asOpt[String]
        when    <- Try(OffsetDateTime.parse(startsAt).toLocalDateTime).toOption
      } yield {
        val id     = (st \ "id").asOpt[String]
        val siteId = (st \ "siteId").asOpt[String].getOrElse("")
        val room   = (st \ "screenId").asOpt[String].flatMap(screens.get)
          .flatMap(s => (s \ "name" \ "text").asOpt[String]).filter(_.nonEmpty)

        // Format/accessibility tags: `requires3dGlasses` plus the recognised
        // showtime attributes. Pricing/promo attributes (Saver, £5 web) are
        // intentionally dropped by `formatToken` so they don't leak into the
        // user-facing pills.
        val threeD  = if ((st \ "requires3dGlasses").asOpt[Boolean].getOrElse(false)) List("3D") else Nil
        val attrs   = (st \ "attributeIds").asOpt[Seq[String]].getOrElse(Seq.empty)
          .flatMap(attributeName).flatMap(formatToken)
        val format  = (threeD ++ attrs).distinct

        val slug     = slugify(title)
        // Release year deliberately NOT taken from `film.releaseDate`: Odeon
        // dates a re-release to its ORIGINAL year, so trusting it would misfile
        // classics. Left to EmbeddedYear / TMDB enrichment downstream.
        RawOdeonShowtime(
          filmId    = filmId,
          title     = title,
          runtime   = (film \ "runtimeInMinutes").asOpt[Int].filter(_ > 0),
          trailer   = (film \ "trailerUrl").asOpt[String].filter(_.nonEmpty),
          filmUrl   = Some(s"$Domain/films/$slug/$filmId/?siteId=$siteId"),
          showtime  = Showtime(
            dateTime   = when,
            bookingUrl = id.map(i => s"$Domain/ticketing/seat-picker/?showtimeId=$i"),
            room       = room,
            format     = format
          )
        )
      }
    }.toSeq.groupBy(_.filmId).toSeq.sortBy(_._1).flatMap { case (_, group) =>
      val head = group.head
      val shows = group.map(_.showtime)
        .distinctBy(s => (s.dateTime, s.bookingUrl, s.room, s.format))
        .sortBy(_.dateTime)
      if (shows.isEmpty) None
      else Some(CinemaMovie(
        movie       = Movie(title = head.title, runtimeMinutes = head.runtime),
        cinema      = cinema,
        posterUrl   = None,
        filmUrl     = head.filmUrl,
        synopsis    = None,
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = shows,
        externalIds = Map("odeon" -> head.filmId),
        trailerUrl  = head.trailer
      ))
    }
  }

  private case class RawOdeonShowtime(
    filmId:   String,
    title:    String,
    runtime:  Option[Int],
    trailer:  Option[String],
    filmUrl:  Option[String],
    showtime: Showtime
  )

  /** Map a Vista showtime-attribute display name to a user-facing format /
   *  accessibility token, or `None` for attributes that aren't a screen format
   *  or an access feature (pricing tiers like "Saver"/"£5 web", promos,
   *  content warnings). A curated data table rather than an allow-everything
   *  passthrough: the attribute list mixes format, accessibility, pricing and
   *  promo tags undifferentiated by the API, and only the first two belong on a
   *  showtime pill. Substring-matched so a decorated variant ("IMAX with Laser",
   *  "Dolby Atmos") still resolves. */
  def formatToken(rawName: String): Option[String] = {
    val n = rawName.toLowerCase.trim
    if (n.isEmpty) None
    else if (n.contains("imax")) Some("IMAX")
    else if (n.contains("screenx")) Some("ScreenX")
    else if (n.contains("4dx")) Some("4DX")
    else if (n.contains("isense")) Some("iSense")
    else if (n.contains("dolby")) Some("Dolby")
    else if (n.contains("d-box") || n.contains("dbox")) Some("D-BOX")
    else if (n.contains("70mm")) Some("70mm")
    else if (n.contains("35mm")) Some("35mm")
    else if (n.contains("recliner")) Some("Recliner")
    else if (n.contains("gallery")) Some("The Gallery")
    else if (n.contains("silver")) Some("Silver Cinema")
    else if (n.contains("audio described")) Some("Audio Described")
    else if (n.contains("open captioned") || n.contains("subtitled") || n.endsWith("(sub)")) Some("Subtitled")
    else if (n.contains("closed captioned")) Some("Closed Captions")
    else if (n.contains("relaxed") || n.contains("autism")) Some("Relaxed")
    else if (n.contains("kids club") || n.contains("baby club") || n.contains("odeon kids")) Some("Kids")
    else if (n.contains("wheelchair")) Some("Wheelchair Accessible")
    else None
  }

  /** Lower-case, `&`→`and`, non-alphanumerics collapsed to single hyphens —
   *  the `slugify(title, {strict:true})` shape Odeon's own film-page URLs use. */
  private def slugify(title: String): String =
    title.toLowerCase
      .replace("&", " and ")
      .replaceAll("[^a-z0-9]+", "-")
      .replaceAll("^-+|-+$", "")
}
