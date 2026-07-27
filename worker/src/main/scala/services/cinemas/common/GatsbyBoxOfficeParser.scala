package services.cinemas.common

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import services.movies.TrailerEmbed

import java.time.LocalDateTime
import scala.util.Try

/**
 * Pure JSON → `CinemaMovie` transformation for the Webedia "box office" Gatsby
 * platform (see [[GatsbyBoxOfficeClient]] for the endpoints and why one client
 * serves both UK brands). No I/O: the client fetches the two bodies and hands
 * them here, so every parsing rule below is unit-testable against recorded
 * fixtures with no HTTP stubbing.
 *
 * The join this file exists to do: the schedule endpoint keys screenings by a
 * bare numeric `movieId` and carries NO film metadata at all — not even a
 * title. The chain-wide Gatsby static query (`allMovie`) is the only place that
 * id resolves to a title/poster/genres, so `parse` takes both bodies and joins
 * them. A scheduled id absent from the catalogue is DROPPED: a film we can only
 * name "1000048157" is unenrichable and unshowable, so a row for it would be
 * worse than no row. (Verified 2026-07-27: the catalogue covered 65/65 of
 * Bluewater's scheduled ids, so the drop is a guard, not a routine path.)
 */
object GatsbyBoxOfficeParser {

  /** The catalogue fields the platform's Gatsby query actually selects. The
   *  schema also declares `runtime`, `release`, `synopsis`, `direction`,
   *  `casting` and `certificate`, but those are `null` for EVERY node on both
   *  UK brands (verified 2026-07-27: 113/113 Showcase and 137/137 Everyman) —
   *  the site's static query simply doesn't populate them. Extracting them
   *  would be extracting nulls, so this carries only what the payload has;
   *  TMDB supplies runtime/synopsis/cast/director downstream anyway. */
  case class CatalogueFilm(
    title:         String,
    originalTitle: Option[String],
    posterUrl:     Option[String],
    path:          Option[String],
    genres:        Seq[String],
    trailerUrl:    Option[String]
  )

  /** Join the venue's schedule against the chain's film catalogue.
   *
   *  `theaterId` selects this venue's branch of the schedule response (the
   *  endpoint takes a list of theatres and keys the reply by id, even though we
   *  only ever ask for one). `baseUrl` turns the catalogue's site-relative
   *  `path` into the absolute film-page URL. */
  def parse(
    scheduleJson:  String,
    catalogueJson: String,
    theaterId:     String,
    cinema:        Cinema,
    baseUrl:       String
  ): Seq[CinemaMovie] = {
    val catalogue = parseCatalogue(catalogueJson)
    val schedule  = (Try(Json.parse(scheduleJson)).getOrElse(JsNull) \ theaterId \ "schedule")
      .asOpt[JsObject].map(_.fields.toSeq).getOrElse(Seq.empty)

    schedule.flatMap { case (movieId, byDate) =>
      val showtimes = parseShowtimes(byDate)
      catalogue.get(movieId)
        .filter(_ => showtimes.nonEmpty)
        .map(film => toCinemaMovie(movieId, film, showtimes, cinema, baseUrl))
    }.sortBy(_.movie.title)
  }

  /** `data.allMovie.nodes[]` keyed by the same numeric id the schedule uses.
   *  Pure + public so a spec can assert the catalogue independently of any
   *  schedule. */
  def parseCatalogue(json: String): Map[String, CatalogueFilm] =
    (Try(Json.parse(json)).getOrElse(JsNull) \ "data" \ "allMovie" \ "nodes")
      .asOpt[Seq[JsValue]].getOrElse(Nil)
      .flatMap { n =>
        for {
          id    <- (n \ "id").asOpt[String].map(_.trim).filter(_.nonEmpty)
          title <- (n \ "title").asOpt[String].map(_.trim).filter(_.nonEmpty)
        } yield id -> CatalogueFilm(
          title = title,
          // Both brands echo the same string here for English-language films;
          // carry it only when it genuinely differs, matching the Webedia
          // sibling's rule so a TMDB search doesn't get a duplicate hint.
          originalTitle = (n \ "originalTitle").asOpt[String].map(_.trim)
            .filter(_.nonEmpty).filter(_ != title),
          posterUrl  = (n \ "poster").asOpt[String].map(_.trim).filter(_.nonEmpty),
          path       = (n \ "path").asOpt[String].map(_.trim).filter(_.nonEmpty),
          genres     = parseGenres((n \ "genres").asOpt[String]),
          // `trailer.youtube` is an ARRAY of watch URLs (populated on ~70% of
          // nodes); `HD`/`SD` sit alongside it and are always null here. Commit
          // only when the first entry parses as an embeddable YouTube id, as
          // MultikinoParser does.
          trailerUrl = (n \ "trailer" \ "youtube").asOpt[Seq[String]].getOrElse(Nil)
            .headOption.flatMap(TrailerEmbed.youTubeId)
            .map(id => s"https://www.youtube.com/watch?v=$id")
        )
      }.toMap

  /** "ADVENTURE, SCIENCE_FICTION" → List("Adventure", "Science fiction"). The
   *  platform ships a comma-separated SCREAMING_SNAKE enum; leave it verbatim
   *  and the genre chips read as shouting placeholders. */
  private def parseGenres(raw: Option[String]): Seq[String] =
    raw.getOrElse("").split(",").toSeq
      .map(_.trim.replace('_', ' ').toLowerCase)
      .filter(_.nonEmpty)
      .map(g => g.substring(0, 1).toUpperCase + g.substring(1))
      .distinct

  /** One film's `{date: [session, …]}` block flattened into screenings.
   *
   *  `isExpired` sessions are dropped — the platform keeps a day's already-
   *  started screenings in the payload with the flag set, and a booking link
   *  that no longer sells. Deduped by (time, booking) because the same
   *  screening can repeat across the tag-suffixed session ids, and time-ordered
   *  so the row is stable between scrapes. */
  private def parseShowtimes(byDate: JsValue): Seq[Showtime] =
    byDate.asOpt[JsObject].map(_.fields.toSeq).getOrElse(Seq.empty)
      .sortBy(_._1)
      .flatMap { case (_, sessions) =>
        sessions.asOpt[Seq[JsValue]].getOrElse(Nil).flatMap(parseSession)
      }
      .distinctBy(s => (s.dateTime, s.bookingUrl))
      .sortBy(_.dateTime)

  private def parseSession(session: JsValue): Option[Showtime] =
    if ((session \ "isExpired").asOpt[Boolean].getOrElse(false)) None
    else
      // `startsAt` is the venue's wall-clock with no zone offset — the theatre's
      // own timeZone is what we passed into the query, so it parses straight to
      // a LocalDateTime.
      (session \ "startsAt").asOpt[String].flatMap(s => Try(LocalDateTime.parse(s.trim)).toOption)
        .map { dateTime =>
          val tags = (session \ "tags").asOpt[Seq[String]].getOrElse(Nil)
          Showtime(
            dateTime   = dateTime,
            bookingUrl = WebediaBoxOffice.bookingUrl(session),
            room       = (session \ "screen" \ "name").asOpt[String].map(_.trim).filter(_.nonEmpty),
            format     = formatTokens(tags)
          )
        }

  /** Screening tokens for one session's dotted `tags[]`.
   *
   *  Dimension first (so a row reads "2D"/"3D" the way Multikino's and Cinema
   *  City's do), then the vendor-shared premium-format tokens, then the
   *  language version. A plain digital screening yields just "2D".
   *
   *  Language lives here rather than in [[WebediaBoxOffice]] because the token
   *  is per-market: Germany wants OmU/OV, the UK wants SUB/DUB.
   *
   *  Public so a spec can pin a tag combination the live snapshot doesn't hold. */
  def formatTokens(tags: Seq[String]): List[String] = {
    val lower = tags.map(_.toLowerCase)
    def has(needle: String) = lower.exists(_.contains(needle))

    // Both brands tag stereoscopic screenings twice — the projection format
    // (`Format.Projection.3d`) and the auditorium system (RealD3D). Either
    // alone means 3D; `Format.Projection.Digital` is the flat default.
    val dimension =
      if (has("format.projection.3d") || has("auditorium.experience.reald3d")) List("3D")
      else if (has("format.projection.digital"))                               List("2D")
      else                                                                     Nil

    // Subtitled / closed / open caption all mean "you can read the dialogue";
    // the accessibility-only tags (audio description, sensory-friendly) are not
    // a screening FORMAT and stay out of this list.
    val language =
      if (has("accessibility.subtitled") || has("accessibility.closedcaption") ||
          has("accessibility.opencaption")) List("SUB")
      else if (has("accessibility.dubbed") || has("localization.dubbed")) List("DUB")
      else Nil

    (dimension ++ WebediaBoxOffice.premiumTokens(tags) ++ language).distinct
  }

  private def toCinemaMovie(
    movieId:   String,
    film:      CatalogueFilm,
    showtimes: Seq[Showtime],
    cinema:    Cinema,
    baseUrl:   String
  ): CinemaMovie =
    CinemaMovie(
      movie = Movie(
        title         = film.title,
        genres        = film.genres,
        originalTitle = film.originalTitle
      ),
      cinema      = cinema,
      posterUrl   = film.posterUrl,
      filmUrl     = film.path.map(p => s"$baseUrl$p"),
      synopsis    = None,   // never populated by the platform's static query
      cast        = Seq.empty,
      director    = Seq.empty,
      showtimes   = showtimes,
      // The platform's own film id — stable across venues within a brand, so a
      // merge can recognise the same film at two Showcase venues.
      externalIds = Map("boxoffice" -> movieId),
      trailerUrl  = film.trailerUrl
    )
}

/**
 * The parts of Webedia's box-office response shape that are the SAME wherever
 * the vendor's platform is deployed — the `data.ticketing[]` booking-link block
 * and the `Format.Projection.*` / `Auditorium.Experience.*` tag namespace.
 *
 * These are shared because the repo already reaches this vendor twice: the
 * Gatsby "box office" sites here (Showcase / Everyman, UK) and the classic
 * website-JSON sites behind [[services.cinemas.de.WebediaShowtimesClient]]
 * (Filmstarts, DE). Both serve the identical `ticketing[].urls[]` shape with
 * the identical `relay.mvtx.us` trailing-junk quirk, and both tag screenings
 * from the identical dotted vocabulary.
 *
 * `WebediaShowtimesClient` still carries its own inline copies of both rules
 * and should be migrated onto this object (at which point this belongs in its
 * own file next to it) — that migration is deliberately out of scope for the
 * change that introduced this file.
 */
object WebediaBoxOffice {

  /** The booking deep-link for one screening, off its `data.ticketing[]`.
   *
   *  Prefer the `default` provider: it is the brand's own clean ticketing URL
   *  (`purchase.everymancinema.com/launch/ticketing/<uuid>`). The `relay`
   *  provider is a `relay.mvtx.us` redirector whose `code=` parameter carries
   *  unencoded spaces and semicolons ("…&code=Gallery; Recliner; ReservedSeating"),
   *  so it only serves as the fallback — and gets cleaned either way. */
  def bookingUrl(session: JsValue): Option[String] = {
    val providers = (session \ "data" \ "ticketing").asOpt[Seq[JsValue]].getOrElse(Nil)
    def urlsOf(p: JsValue) = (p \ "urls").asOpt[Seq[String]].getOrElse(Nil)
    val preferred = providers.find(p => (p \ "provider").asOpt[String].contains("default"))
    preferred.toSeq.flatMap(urlsOf).headOption
      .orElse(providers.flatMap(urlsOf).headOption)
      .map(cleanBookingUrl)
      .filter(_.nonEmpty)
  }

  /** Relay booking URLs arrive with a render/attribute marker glued on past a
   *  space ("…&code=2D; SSR", "…&code=Gallery; Recliner"); take the URL up to
   *  the first whitespace and drop a stray trailing ";". */
  def cleanBookingUrl(url: String): String =
    url.trim.split("\\s+").headOption.getOrElse(url).stripSuffix(";")

  /** Premium/projection tokens from the vendor's dotted tag vocabulary, minus
   *  the plain-digital and stereoscopic tags (which every deployment folds into
   *  its own dimension token) and minus anything market-specific.
   *
   *  Confirmed present on the UK brands 2026-07-27 (`Format.Projection.Laser`,
   *  `Auditorium.Experience.PLF` at Showcase; `Format.Projection.35mm`,
   *  `Auditorium.Experience.DolbyAtmos` at Everyman; `Format.Projection.HFR` in
   *  both vocabularies). `Format.Projection.Imax` is in the vendor's shared
   *  797-entry taxonomy and is already mapped by the German sibling, so it is
   *  carried here too even though neither UK brand runs an IMAX screen. */
  def premiumTokens(tags: Seq[String]): List[String] = {
    val lower = tags.map(_.toLowerCase)
    List(
      "format.projection.imax"           -> "IMAX",
      "format.projection.laser"          -> "LASER",
      "format.projection.hfr"            -> "HFR",
      "format.projection.35mm"           -> "35MM",
      "auditorium.experience.plf"        -> "PLF",
      "auditorium.experience.dolbyatmos" -> "ATMOS"
    ).collect { case (needle, token) if lower.exists(_.contains(needle)) => token }
  }
}
