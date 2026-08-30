package services.cinemas.us

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Pure JSON → `CinemaMovie` transformation for Alamo Drafthouse's own schedule
 * API (see [[AlamoDrafthouseClient]] for the endpoint and why one call covers a
 * venue's whole horizon). No I/O: the client fetches the body and hands it here,
 * so every rule below is unit-testable against a recorded fixture.
 *
 * The join this file exists to do mirrors the Webedia one in
 * [[services.cinemas.common.GatsbyBoxOfficeParser]]: `data.sessions[]` carries
 * the screenings and names its film only by a `presentationSlug`, while
 * `data.presentations[]` is the only place that slug resolves to a title,
 * poster and certificate. A session whose presentation is absent is DROPPED —
 * a film we could only name by its slug is unenrichable and unshowable, so a
 * row for it would be worse than no row. (Verified 2026-08-30 against Lakeline:
 * the payload resolved 300/300 of its sessions, so the drop is a guard, not a
 * routine path.)
 *
 * A PRESENTATION IS NOT A FILM. Alamo models each way a film is presented as its
 * own presentation — the regular run, an advance screening, a live-Q&A night, a
 * dress-up screening — and every one of them carries the SAME `show`, with the
 * same title. Lakeline's snapshot had 51 presentations over 48 shows for exactly
 * that reason. Grouping by presentation would therefore emit two rows with an
 * identical title at one cinema, so rows are keyed by `show.slug`, which is the
 * chain's own film-level id: one row per film, its variants' showtimes unioned.
 */
object AlamoDrafthouseParser {

  /** The film a presentation is OF — the film side of the join, identified by
   *  `show.slug` rather than by the presentation's own slug (see the class note).
   *  Alamo's VENUE payload carries no runtime, synopsis, cast or director (those
   *  live on the per-show detail endpoint, which is a separate fetch per film);
   *  TMDB supplies them downstream, so this carries only what the payload has. */
  case class Show(
    slug:          String,
    title:         String,
    posterUrl:     Option[String],
    certification: Option[String]
  )

  /** Join a venue's sessions against its presentations into one row per film.
   *
   *  `venueId` selects this venue's sessions. The endpoint is venue-scoped, but
   *  it answers with the whole MARKET's `market[].cinemas[]` roster alongside,
   *  so filtering by the id resolved from our own slug is a cheap guard against
   *  ever reading a sibling venue's programme into this one's listing.
   *
   *  `notAfter` is the sanity bound on how far out a screening may sit — see
   *  [[services.cinemas.common.ScrapeHorizon]]. It is NOT a coverage window: the
   *  payload is whatever the venue advertises and we keep all of it, this only
   *  stops a garbage far-future date landing in the read model.
   */
  def parse(json: String, venueSlug: String, cinema: Cinema, notAfter: LocalDate): Seq[CinemaMovie] = {
    val data          = (Try(Json.parse(json)).getOrElse(JsNull) \ "data")
    val presentations = parsePresentations(data)
    val formats       = parseFormatTitles(data)
    val venueId       = cinemaIdFor(data, venueSlug)

    val sessions = (data \ "sessions").asOpt[Seq[JsValue]].getOrElse(Nil)
      .filter(s => venueId.forall(id => (s \ "cinemaId").asOpt[String].contains(id)))
      .filterNot(isHidden)

    sessions
      .flatMap(s => (s \ "presentationSlug").asOpt[String].flatMap(presentations.get).map(_ -> s))
      .groupBy(_._1.slug)
      .toSeq
      .flatMap { case (_, group) =>
        val showtimes = group.map(_._2)
          .flatMap(s => parseSession(s, formats))
          .filterNot(_.dateTime.toLocalDate.isAfter(notAfter))
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        // The show is the same object behind every one of its presentations, so
        // any of them names it; take the first that carries each optional field.
        val shows = group.map(_._1)
        Option.when(showtimes.nonEmpty)(
          toCinemaMovie(
            shows.head.copy(
              posterUrl     = shows.flatMap(_.posterUrl).headOption,
              certification = shows.flatMap(_.certification).headOption
            ),
            showtimes, cinema))
      }
      .sortBy(_.movie.title)
  }

  /** `data.presentations[]` keyed by the PRESENTATION slug the sessions carry,
   *  valued by the SHOW that presentation is of — the indirection that lets a
   *  film's variants collapse into one row. Pure + public so a spec can assert
   *  the film side independently of any session. */
  def parsePresentations(data: JsLookupResult): Map[String, Show] =
    (data \ "presentations").asOpt[Seq[JsValue]].getOrElse(Nil)
      .filterNot(p => (p \ "isHidden").asOpt[Boolean].getOrElse(false))
      .flatMap { p =>
        for {
          slug     <- (p \ "slug").asOpt[String].map(_.trim).filter(_.nonEmpty)
          showSlug <- (p \ "show" \ "slug").asOpt[String].map(_.trim).filter(_.nonEmpty)
          title    <- (p \ "show" \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        } yield slug -> Show(
          slug          = showSlug,
          title         = title,
          posterUrl     = (p \ "show" \ "posterImages").asOpt[Seq[JsValue]].getOrElse(Nil)
            .headOption.flatMap(i => (i \ "uri").asOpt[String]).map(_.trim).filter(_.nonEmpty),
          certification = certificate((p \ "show" \ "certification").asOpt[String])
        )
      }.toMap

  /** The MPAA certificate, when the field really holds one.
   *
   *  Alamo's `show.certification` is free text and the live corpus proves it:
   *  across all 23 markets it also held "(Standard)", "Focus" and "Ages" —
   *  fragments of an age-POLICY name that had leaked into the field. Those are
   *  not ratings, and storing one would put "Focus" on a film's age-rating chip,
   *  so only the real MPAA vocabulary is admitted. */
  private def certificate(raw: Option[String]): Option[String] =
    raw.map(_.trim.toUpperCase).filter(MpaaRatings.contains)

  private val MpaaRatings = Set("G", "PG", "PG-13", "R", "NC-17", "NR")

  /** `data.formats[]` slug → its display title, so a screening's `formatSlug`
   *  can be recognised even where this venue's payload is the only place the
   *  vendor spells it. */
  private def parseFormatTitles(data: JsLookupResult): Map[String, String] =
    (data \ "formats").asOpt[Seq[JsValue]].getOrElse(Nil)
      .flatMap { f =>
        for {
          slug  <- (f \ "slug").asOpt[String].map(_.trim).filter(_.nonEmpty)
          title <- (f \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        } yield slug -> title
      }.toMap

  /** This venue's numeric Alamo cinema id, resolved from the slug we asked for
   *  against the market roster the response carries. `None` when the roster
   *  doesn't name it — then no filter is applied, because the endpoint is
   *  already venue-scoped and dropping every session would turn a shape change
   *  into a silently empty venue. */
  private def cinemaIdFor(data: JsLookupResult, venueSlug: String): Option[String] =
    (data \ "market").asOpt[Seq[JsValue]].getOrElse(Nil)
      .flatMap(m => (m \ "cinemas").asOpt[Seq[JsValue]].getOrElse(Nil))
      .find(c => (c \ "slug").asOpt[String].contains(venueSlug))
      .flatMap(c => (c \ "id").asOpt[String])

  /** A session the public can't book: the payload's own `isHidden` flag, or the
   *  vendor's "Hidden" attribute, whose `sessionAttributes[]` entry spells it
   *  "Invite Only". Both mean the screening is not on public sale. */
  private def isHidden(session: JsValue): Boolean =
    (session \ "isHidden").asOpt[Boolean].getOrElse(false) ||
      attributeSlugs(session).exists(_.equalsIgnoreCase("Hidden"))

  private def attributeSlugs(session: JsValue): Seq[String] =
    (session \ "sessionAttributeSlugs").asOpt[Seq[String]].getOrElse(Nil)

  private def parseSession(session: JsValue, formats: Map[String, String]): Option[Showtime] =
    // `showTimeClt` is the cinema's own local wall-clock ("clt" = cinema local
    // time) with no zone offset, which is exactly what we store — the sibling
    // `showTimeUtc` would need the venue's zone applied back to say the same
    // thing.
    (session \ "showTimeClt").asOpt[String]
      .flatMap(s => Try(LocalDateTime.parse(s.trim)).toOption)
      .map { dateTime =>
        Showtime(
          dateTime   = dateTime,
          bookingUrl = bookingUrl(session),
          room       = (session \ "screenNumber").asOpt[String].map(_.trim).filter(_.nonEmpty),
          format     = formatTokens(
            (session \ "formatSlug").asOpt[String].toSeq ++ attributeSlugs(session),
            formats
          )
        )
      }

  /** The public booking deep-link for one session. Alamo's ticketing route is
   *  `/ticketing/<cinemaId>/<sessionId>` — both ids are on the session itself,
   *  so the link is derived rather than read from a field the payload doesn't
   *  carry. */
  private def bookingUrl(session: JsValue): Option[String] =
    for {
      cinemaId  <- (session \ "cinemaId").asOpt[String].map(_.trim).filter(_.nonEmpty)
      sessionId <- (session \ "sessionId").asOpt[String].map(_.trim).filter(_.nonEmpty)
    } yield s"${AlamoDrafthouseClient.BaseUrl}/ticketing/$cinemaId/$sessionId"

  /** Screening tokens for one session's format slug + attribute slugs.
   *
   *  Dimension first (so a row reads "2D"/"3D" the way every other client's
   *  does), then the premium formats, then the language token — the same order
   *  [[services.cinemas.common.GatsbyBoxOfficeParser.formatTokens]] uses, so a
   *  US chain row and a UK one read alike.
   *
   *  The vocabulary is small and closed: `data.formats[]` held exactly eight
   *  slugs across all 23 markets on 2026-08-30 and `sessionAttributes[]`
   *  sixteen. Only the ones that describe HOW the film is projected or heard
   *  are tokens — the audience-policy attributes (All Ages, Baby Day, Kid
   *  Friendly, Sensory Friendly, the QR menu markers) are not a screening
   *  FORMAT, the same line the Webedia parser draws.
   *
   *  Public so a spec can pin a combination the recorded venue happens not to
   *  hold. `formats` is the response's own slug→title map, used only to tell a
   *  format slug we don't know from an attribute we don't know. */
  def formatTokens(slugs: Seq[String], formats: Map[String, String]): List[String] = {
    val lower = slugs.map(_.trim.toLowerCase).filter(_.nonEmpty).toSet

    val dimension =
      if (lower.contains("3d-digital")) List("3D")
      else if (lower.exists(Set("2d-digital", "digital", "open-caption")))
        // `open-caption` occupies the format slot but says nothing about the
        // dimension; every observed open-caption screening is a flat digital one.
        List("2D")
      else Nil

    val premium = PremiumTokens.collect { case (slug, token) if lower.contains(slug) => token }

    // Open caption puts the dialogue on the screen — the same thing the UK
    // brands' `Accessibility.OpenCaption` tag means, so it earns the same token.
    val language = if (lower.contains("open-caption")) List("SUB") else Nil

    // A format slug the response declares but this map doesn't know yet: keep
    // the vendor's own display title rather than dropping the screening's only
    // distinguishing mark. Attributes are NOT treated this way — most of them
    // are audience policy, not format.
    val unmapped = slugs.map(_.trim).filter(s => formats.contains(s))
      .filterNot(s => KnownFormatSlugs.contains(s.toLowerCase))
      .flatMap(formats.get).map(_.toUpperCase)

    (dimension ++ premium ++ language ++ unmapped).distinct
  }

  /** Format slugs and session attributes that map to a screening token. Both
   *  namespaces are folded into one lookup because Alamo spells the celluloid
   *  gauges in BOTH ("35mm" as a format, "35MM"/"35mm" as an attribute) and
   *  either one alone means the same thing. */
  private val PremiumTokens: List[(String, String)] = List(
    "70mm"         -> "70MM",
    "35mm"         -> "35MM",
    "16mm"         -> "16MM",
    "hdr"          -> "HDR",
    "the-big-show" -> "PLF",
    "infinity"     -> "INFINITY",
    "atmos"        -> "ATMOS"
  )

  /** Every format slug the tokeniser above already accounts for, so `unmapped`
   *  only ever fires for a genuinely NEW one the vendor introduces. */
  private val KnownFormatSlugs: Set[String] =
    Set("2d-digital", "3d-digital", "open-caption") ++ PremiumTokens.map(_._1)

  private def toCinemaMovie(show: Show, showtimes: Seq[Showtime], cinema: Cinema): CinemaMovie =
    CinemaMovie(
      movie       = Movie(title = show.title),
      cinema      = cinema,
      posterUrl   = show.posterUrl,
      filmUrl     = Some(s"${AlamoDrafthouseClient.BaseUrl}/show/${show.slug}"),
      synopsis    = None,   // not carried by the venue payload; TMDB supplies it
      cast        = Seq.empty,
      director    = Seq.empty,
      showtimes   = showtimes,
      // Alamo's own show slug — stable across venues AND across a film's
      // presentation variants, so a merge can recognise the same film at two
      // Drafthouses.
      externalIds = Map("alamo" -> show.slug),
      ageRating   = show.certification
    )
}
