package services.cinemas.uk

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import services.movies.TrailerEmbed

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Pure JSON → model transformation for Cineworld's `quickbook` data API
 * (the same Regal-built endpoint family Cinema City Poland serves off, chain
 * code 10108 instead of 10103). No I/O: [[CineworldClient]] fetches the bodies
 * and hands them here, so every parse is unit-testable against the recorded
 * fixtures without any HTTP stubbing.
 */
object CineworldParser {

  /** One venue off the chain roster (`cinemas/with-event/until/<date>`). The
   *  checked-in `CINEWORLD-VENUE-MAP.tsv` — which pairs each API venue id with
   *  the `Cinema` case object it feeds — is built from this list, so the reader
   *  lives in code and is pinned by the spec: a roster shape change (a renamed
   *  venue, a new site, a dropped one) then surfaces as a test failure instead
   *  of a silently stale map. */
  case class CineworldVenue(id: String, displayName: String, link: Option[String])

  /** The chain's venues. Throws when the response carries no `body.cinemas`
   *  array — that isn't "no venues", it's a response we failed to parse. */
  def parseVenues(json: String): Seq[CineworldVenue] =
    array(json, "cinemas").flatMap { venue =>
      for {
        id   <- (venue \ "id").asOpt[String].filter(_.nonEmpty)
        name <- (venue \ "displayName").asOpt[String].filter(_.nonEmpty)
      } yield CineworldVenue(id, name, (venue \ "link").asOpt[String].filter(_.nonEmpty))
    }

  /** The dates a venue has a programme on, off `dates/in-cinema/<id>/until/…`.
   *  Sparse — Cineworld lists day-by-day for the near term and then only the
   *  individual far-out days advance sales are open on, so this is the exact
   *  set of days worth a `film-events` call.
   *
   *  An EMPTY list is expected data (a venue with nothing on), not a failure:
   *  the planner records an empty scrape and `MovieCache.recordCinemaScrape`
   *  bails on it, so the venue keeps its last-known listing. A body with no
   *  `dates` array at all is a parse failure and throws — the line
   *  `WebediaShowtimesClient` and [[FlicksClient]] already draw. */
  def parseDates(json: String): Seq[LocalDate] =
    array(json, "dates").flatMap(_.asOpt[String])
      .flatMap(s => Try(LocalDate.parse(s)).toOption)
      .distinct.sorted

  /** ONE day's `film-events` response → that day's films, joining `events[]` to
   *  `films[]` on `filmId`. Deterministic: films come out ordered by their
   *  Cineworld id and each film's showtimes by start time. Throws on a body we
   *  can't parse (only that day's chunk task reschedules). */
  def parseDay(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    val body   = parseBody(json)
    val films  = optionalArray(body, "films")
    val events = optionalArray(body, "events")

    val info: Map[String, FilmInfo] = films.iterator.flatMap { film =>
      (film \ "id").asOpt[String].filter(_.nonEmpty).zip((film \ "name").asOpt[String].filter(_.nonEmpty))
        .map { case (id, name) =>
          id -> FilmInfo(
            name           = name,
            runtimeMinutes = (film \ "length").asOpt[Int].filter(_ > 0),
            posterUrl      = (film \ "posterLink").asOpt[String].filter(_.nonEmpty),
            filmUrl        = (film \ "link").asOpt[String].filter(_.nonEmpty),
            // `videoLink` is a bare YouTube share URL ("https://youtu.be/…") on
            // roughly half the catalogue; commit it only when it parses as a
            // YouTube id we can embed, normalised to the watch form the view
            // layer expects (same shape MultikinoParser stores).
            trailerUrl     = (film \ "videoLink").asOpt[String].filter(_.nonEmpty)
                               .flatMap(TrailerEmbed.youTubeId)
                               .map(id => s"https://www.youtube.com/watch?v=$id"),
            genres         = attributesOf(film).flatMap(GenreLabel.get).distinct.toList)
        }
    }.toMap

    events.iterator.flatMap { event =>
      for {
        filmId   <- (event \ "filmId").asOpt[String].filter(_.nonEmpty)
        dateTime <- (event \ "eventDateTime").asOpt[String].flatMap(s => Try(LocalDateTime.parse(s)).toOption)
      } yield filmId -> Showtime(
        dateTime   = dateTime,
        bookingUrl = (event \ "bookingLink").asOpt[String].filter(_.nonEmpty),
        room       = (event \ "auditorium").asOpt[String].filter(_.nonEmpty).map(screenName),
        format     = formatOf(attributesOf(event)))
    }.toSeq
      .groupBy(_._1).toSeq.sortBy(_._1)
      .flatMap { case (filmId, slots) =>
        info.get(filmId).map { film =>
          CinemaMovie(
            movie       = Movie(
              title          = film.name,
              runtimeMinutes = film.runtimeMinutes,
              // `releaseYear` is the UK *theatrical release* year, not the film's
              // production year: this feed stamps 2026 on "Scorsese Season: Raging
              // Bull (1980)" and on "£2 Family Films : Chicken Run (2026
              // Re-Release)" alike. Using it would poison TMDB resolution exactly
              // the way it does for Multikino — a year-scoped search excludes the
              // real film and the fallback picks whoever shares the title. Leave
              // it None; `MovieService.settle`'s `EmbeddedYear` backfill recovers
              // the year from the titles that actually carry one.
              releaseYear    = None,
              genres         = film.genres,
              rawTitle       = Some(film.name)),
            cinema      = cinema,
            posterUrl   = film.posterUrl,
            filmUrl     = film.filmUrl,
            // Synopsis / cast / director aren't in this feed and Cineworld's film
            // pages are client-rendered, so TMDB supplies them downstream.
            synopsis    = None,
            cast        = Seq.empty,
            director    = Seq.empty,
            showtimes   = slots.map(_._2).sortBy(_.dateTime),
            externalIds = Map("cineworld" -> filmId),
            trailerUrl  = film.trailerUrl)
        }
      }
  }

  private case class FilmInfo(
    name:           String,
    runtimeMinutes: Option[Int],
    posterUrl:      Option[String],
    filmUrl:        Option[String],
    trailerUrl:     Option[String],
    genres:         Seq[String]
  )

  /** `attributeIds` is one kitchen-sink list per film AND per event, mixing
   *  screen format (`2d`, `imax`, `4dx`), genre (`action`), BBFC rating (`15`),
   *  language (`tamil`), accessibility (`audio-described`) and seating
   *  (`recliner`, `reserved-selected`). Callers pick the slice they want. */
  private def attributesOf(node: JsValue): Seq[String] =
    (node \ "attributeIds").asOpt[Seq[String]].getOrElse(Seq.empty).map(_.toLowerCase)

  /** The screen-feature tokens we badge a showtime with, in the app-wide
   *  vocabulary `FormatTags` fixes (2D / 3D / IMAX / 4DX / NAP …) so Cineworld
   *  agrees with every other source, plus the film's spoken language on the
   *  foreign-language screenings that carry one. Deliberately narrow: `laser`,
   *  `recliner`, `reserved-selected` and `audio-described` ride the majority of
   *  screenings here (audio description alone is on ~75% of them), so badging
   *  them would be noise rather than a distinguishing mark. */
  private def formatOf(attrs: Seq[String]): List[String] = {
    val set = attrs.toSet
    List(
      PremiumFormats.collectFirst { case (id, token) if set.contains(id) => token },
      if (set.contains("3d")) Some("3D") else if (set.contains("2d")) Some("2D") else None,
      if (set.contains("subbed")) Some("NAP") else None
    ).flatten ++ attrs.flatMap(LanguageLabel.get).distinct
  }

  /** Premium presentation formats, most distinctive first — a 4DX screening is
   *  also flagged `3d`, so only the leading match becomes the premium badge and
   *  the dimension token follows it separately. */
  private val PremiumFormats: List[(String, String)] =
    List("imax" -> "IMAX", "4dx" -> "4DX", "screenx" -> "SCREENX")

  /** Spoken-language attribute ids Cineworld tags a foreign-language screening
   *  with → the token we badge it with, so a Tamil or Telugu showing reads
   *  apart from the English default. Whitelisted like [[GenreLabel]] for the
   *  same reason: `attributeIds` has no namespace to separate a language from a
   *  rating, genre or seating id, so an unrecognised token must drop rather
   *  than leak a non-language attribute onto the badge. */
  private val LanguageLabel: Map[String, String] = Map(
    "hindi"  -> "HINDI",
    "tamil"  -> "TAMIL",
    "telugu" -> "TELUGU"
  )

  /** Cineworld's `auditorium` is the bare screen number ("12"); the venue signs
   *  and the booking flow both call it "Screen 12". Anything non-numeric (a
   *  named premium screen) passes through as-is. */
  private def screenName(auditorium: String): String =
    if (auditorium.forall(_.isDigit)) s"Screen $auditorium" else auditorium

  /** Genre tokens as they appear in `attributeIds` → the label we display.
   *  Whitelisted rather than passed through, because the same list carries
   *  ratings, languages and seating that are NOT genres; an unrecognised token
   *  drops rather than leaking "reserved-selected" onto a card. */
  private val GenreLabel: Map[String, String] = Map(
    "action"      -> "Action",
    "adventure"   -> "Adventure",
    "animation"   -> "Animation",
    "anime"       -> "Anime",
    "biography"   -> "Biography",
    "comedy"      -> "Comedy",
    "concert"     -> "Concert",
    "crime"       -> "Crime",
    "documentary" -> "Documentary",
    "drama"       -> "Drama",
    "family"      -> "Family",
    "fantasy"     -> "Fantasy",
    "history"     -> "History",
    "horror"      -> "Horror",
    "music"       -> "Music",
    "musical"     -> "Musical",
    "mystery"     -> "Mystery",
    "opera"       -> "Opera",
    "romance"     -> "Romance",
    "sci-fi"      -> "Sci-Fi",
    "scifi"       -> "Sci-Fi",
    "sport"       -> "Sport",
    "thriller"    -> "Thriller",
    "war"         -> "War",
    "western"     -> "Western"
  )

  /** Every quickbook response wraps its payload in a `body` object. Its absence
   *  means we're not looking at the API we think we are (an error page, a
   *  Cloudflare interstitial, a shape change) — a failure, so it throws. */
  private def parseBody(json: String): JsValue =
    (Json.parse(json) \ "body").asOpt[JsObject].getOrElse(
      throw new IllegalStateException("Cineworld response carried no `body` object"))

  /** A REQUIRED array under `body` — missing means an unparseable response. */
  private def array(json: String, field: String): Seq[JsValue] =
    (parseBody(json) \ field).asOpt[JsArray].map(_.value.toSeq).getOrElse(
      throw new IllegalStateException(s"Cineworld response carried no `body.$field` array"))

  /** An array under an already-parsed body that may legitimately be absent —
   *  a day with no programme comes back with empty (or no) `films`/`events`. */
  private def optionalArray(body: JsValue, field: String): Seq[JsValue] =
    (body \ field).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
}
