package services.cinemas.uk

import java.util.Locale

import models._
import play.api.libs.json._
import services.cinemas.common.AgeRating

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
   *  `docs/venue-maps/ODEON-VENUE-MAP.tsv`), not to build URLs. */
  case class OdeonSite(id: String, name: String)

  def parseSites(json: String): Seq[OdeonSite] =
    arrayAt(Json.parse(json) \ "sites").flatMap { s =>
      for {
        id   <- (s \ "id").asOpt[String]
        name <- (s \ "name" \ "text").asOpt[String]
      } yield OdeonSite(id, name)
    }.toSeq

  /** The business dates a venue has a programme on — the client's horizon. Each
   *  `filmScreeningDates[]` entry is one date; deduped + sorted. */
  def parseScreeningDates(json: String): Seq[LocalDate] =
    arrayAt(Json.parse(json) \ "filmScreeningDates")
      .flatMap(d => (d \ "businessDate").asOpt[String])
      .flatMap(s => Try(LocalDate.parse(s)).toOption)
      .distinct.sorted.toSeq

  /** A day's showtimes → one [[CinemaMovie]] per film, showtimes deduped by
   *  (time, booking, room, format) and time-ordered. Films/screens/attributes
   *  are resolved out of `relatedData`. */
  def parseShowtimes(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    val root    = Json.parse(json)
    val related = new RelatedData(root \ "relatedData")
    arrayAt(root \ "showtimes").flatMap(showtimeRow(_, related)).toSeq
      .groupBy(_.filmId).toSeq.sortBy(_._1)
      .map { case (_, group) => cinemaMovie(group, related, cinema) }
  }

  /** One `showtimes[]` entry, kept when it names a known film with a title and a
   *  parseable start. */
  private def showtimeRow(st: JsValue, related: RelatedData): Option[RawOdeonShowtime] =
    for {
      filmId   <- (st \ "filmId").asOpt[String]
      film     <- related.films.get(filmId)
      title    <- (film \ "title" \ "text").asOpt[String].map(_.trim).filter(_.nonEmpty)
      startsAt <- (st \ "schedule" \ "startsAt").asOpt[String]
      when     <- Try(OffsetDateTime.parse(startsAt).toLocalDateTime).toOption
    } yield {
      val siteId = (st \ "siteId").asOpt[String].getOrElse("")
      // Release year deliberately NOT taken from `film.releaseDate`: Odeon
      // dates a re-release to its ORIGINAL year, so trusting it would misfile
      // classics. Left to EmbeddedYear / TMDB enrichment downstream.
      RawOdeonShowtime(
        filmId   = filmId,
        film     = film,
        title    = title,
        filmUrl  = Some(s"$Domain/films/${slugify(title)}/$filmId/?siteId=$siteId"),
        showtime = Showtime(
          dateTime   = when,
          bookingUrl = (st \ "id").asOpt[String].map(i => s"$Domain/ticketing/seat-picker/?showtimeId=$i"),
          room       = (st \ "screenId").asOpt[String].flatMap(related.screenName),
          format     = showtimeFormat(st, related)
        )
      )
    }

  /** Format/accessibility tags: `requires3dGlasses` plus the recognised
   *  showtime attributes. Pricing/promo attributes (Saver, £5 web) are
   *  intentionally dropped by `formatToken` so they don't leak into the
   *  user-facing pills. */
  private def showtimeFormat(st: JsValue, related: RelatedData): List[String] = {
    val threeD = if ((st \ "requires3dGlasses").asOpt[Boolean].getOrElse(false)) List("3D") else Nil
    val attrs  = (st \ "attributeIds").asOpt[Seq[String]].getOrElse(Seq.empty)
      .flatMap(related.attributeName).flatMap(formatToken)
    (threeD ++ attrs).distinct
  }

  /** One film's rows (never empty — `groupBy` makes no empty group) → its
   *  [[CinemaMovie]]. */
  private def cinemaMovie(group: Seq[RawOdeonShowtime], related: RelatedData, cinema: Cinema): CinemaMovie = {
    val head = group.head
    val film = head.film
    CinemaMovie(
      movie       = Movie(
        title          = head.title,
        runtimeMinutes = (film \ "runtimeInMinutes").asOpt[Int].filter(_ > 0),
        genres         = related.filmGenres(film)
      ),
      cinema      = cinema,
      posterUrl   = None,
      filmUrl     = head.filmUrl,
      synopsis    = filmSynopsis(film),
      cast        = related.peopleWithRole(film, "Actor"),
      director    = related.peopleWithRole(film, "Director"),
      showtimes   = group.map(_.showtime)
        .distinctBy(s => (s.dateTime, s.bookingUrl, s.room, s.format))
        .sortBy(_.dateTime),
      externalIds = Map("odeon" -> head.filmId),
      trailerUrl  = (film \ "trailerUrl").asOpt[String].filter(_.nonEmpty),
      ageRating   = related.filmAgeRating(film)
    )
  }

  // Prefer the full synopsis; fall back to the short one.
  private def filmSynopsis(film: JsValue): Option[String] =
    (film \ "synopsis" \ "text").asOpt[String]
      .orElse((film \ "shortSynopsis" \ "text").asOpt[String])
      .map(_.trim).filter(_.nonEmpty)

  private def arrayAt(at: JsLookupResult): Seq[JsValue] =
    at.asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)

  /** The showtimes payload's `relatedData` blocks, indexed by id, and the
   *  lookups that resolve a showtime's or film's references against them. */
  private final class RelatedData(relatedData: JsLookupResult) {
    private def byId(block: String): Map[String, JsValue] =
      arrayAt(relatedData \ block).flatMap(v => (v \ "id").asOpt[String].map(_ -> v)).toMap

    val films: Map[String, JsValue]   = byId("films")
    private val screens               = byId("screens")
    private val attributes            = byId("attributes")
    private val castAndCrew           = byId("castAndCrew")
    private val genres                = byId("genres")
    private val censorRatings         = byId("censorRatings")

    def screenName(id: String): Option[String] =
      screens.get(id).flatMap(s => (s \ "name" \ "text").asOpt[String]).filter(_.nonEmpty)

    def attributeName(id: String): Option[String] =
      attributes.get(id).flatMap(a => (a \ "name" \ "text").asOpt[String])

    // `relatedData.castAndCrew[].name` splits the person across
    // given/middle/family (some parts blank or space-padded); join the
    // non-blank, trimmed parts into one display name.
    private def personName(memberId: String): Option[String] =
      castAndCrew.get(memberId).map { p =>
        Seq("givenName", "middleName", "familyName")
          .flatMap(k => (p \ "name" \ k).asOpt[String]).map(_.trim).filter(_.nonEmpty)
          .mkString(" ")
      }.filter(_.nonEmpty)

    // A film's `castAndCrew[]` are `{castAndCrewMemberId, roles[]}` refs;
    // resolve each id against `relatedData.castAndCrew` and keep those whose
    // roles include `role`, preserving the source order.
    def peopleWithRole(film: JsValue, role: String): Seq[String] =
      arrayAt(film \ "castAndCrew")
        .filter(c => (c \ "roles").asOpt[Seq[String]].getOrElse(Seq.empty).contains(role))
        .flatMap(c => (c \ "castAndCrewMemberId").asOpt[String])
        .flatMap(personName)

    // A film's `genreIds[]` resolved against `relatedData.genres[].name.text`.
    def filmGenres(film: JsValue): Seq[String] =
      (film \ "genreIds").asOpt[Seq[String]].getOrElse(Seq.empty)
        .flatMap(genres.get)
        .flatMap(g => (g \ "name" \ "text").asOpt[String]).map(_.trim).filter(_.nonEmpty)

    // A film's `censorRatingId` resolved against
    // `relatedData.censorRatings[].classification.text` — the BBFC label
    // ("PG", "U", "15"). AgeRating trims and drops any unclassified placeholder.
    def filmAgeRating(film: JsValue): Option[String] =
      (film \ "censorRatingId").asOpt[String].flatMap(censorRatings.get)
        .flatMap(c => (c \ "classification" \ "text").asOpt[String])
        .flatMap(AgeRating.normalize)
  }

  private case class RawOdeonShowtime(
    filmId:   String,
    film:     JsValue,
    title:    String,
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
    val n = rawName.toLowerCase(Locale.ROOT).trim
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
    title.toLowerCase(Locale.ROOT)
      .replace("&", " and ")
      .replaceAll("[^a-z0-9]+", "-")
      .replaceAll("^-+|-+$", "")
}
