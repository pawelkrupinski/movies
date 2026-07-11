package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

class CinemaCityClient(http: HttpFetch, detailHttp: Option[HttpFetch] = None) {

  private val BaseApiUrl = CinemaCityClient.BaseApiUrl
  // Per-film detail-page fetch path. Defaults to `http`; the composition root
  // injects ONE CachingDetailFetch shared across every Cinema City location, so a
  // film's detail page is fetched once per chain per TTL instead of once per
  // location per pass. The live day-listing fetch stays on `http`.
  private val detailFetch: HttpFetch = detailHttp.getOrElse(http)
  private val FarFuture  = "2027-01-01"

  // Returns BARE movies (showtimes + listing fields + the per-film detail-page
  // URL as filmUrl); the per-venue CinemaCityScraper enqueues an EnrichDetails
  // task that calls `fetchFilmDetail` for countries/genres/synopsis/cast/
  // director/trailer.
  def fetch(cinemaId: String, cinema: Cinema): Seq[CinemaMovie] =
    reduce(dates(cinemaId).flatMap(d => fetchDay(cinemaId, cinema, d)))

  /** The cinema's available screening dates = the chunk keys. Best-effort: an
   *  empty/failed dates call yields no chunks (an empty scrape keeps the venue's
   *  slots), matching the old behaviour. */
  def dates(cinemaId: String): Seq[LocalDate] = {
    val datesUrl = s"$BaseApiUrl/dates/in-cinema/$cinemaId/until/$FarFuture?attr=&lang=pl_PL"
    Try((Json.parse(http.get(datesUrl)) \ "body" \ "dates").as[Seq[String]]
      .flatMap(d => Try(LocalDate.parse(d)).toOption)).getOrElse(Seq.empty)
  }

  /** One day's film-events response → that day's films (bare, with that day's
   *  showtimes). A throw reschedules just this day's chunk task. */
  def fetchDay(cinemaId: String, cinema: Cinema, date: LocalDate): Seq[CinemaMovie] =
    parseDay(http.get(s"$BaseApiUrl/film-events/in-cinema/$cinemaId/at-date/$date?attr=&lang=pl_PL"), cinema)

  /** Merge per-day films by Cinema City film id (externalIds "cc"), concatenating
   *  + sorting their showtimes — the cross-day fold the old whole-venue scrape did
   *  (deterministic by id; first day's film metadata wins, as before). */
  def reduce(movies: Seq[CinemaMovie]): Seq[CinemaMovie] =
    movies.groupBy(m => m.externalIds.getOrElse("cc", m.movie.title)).toSeq.sortBy(_._1)
      .map { case (_, group) => group.head.copy(showtimes = group.flatMap(_.showtimes).sortBy(_.dateTime)) }

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's film-page URL (Cinema City's `link`). Goes through the shared
   *  `detailFetch` so a film's page is fetched once per chain per TTL. None on
   *  fetch failure so the task stays stale and is retried. */
  def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailFetch.get(ref)).toOption.map { html =>
      val detail = CinemaCityClient.parseDetails(html)
      FilmDetail(
        synopsis   = detail.synopsis,
        cast       = detail.cast,
        director   = detail.director,
        countries  = detail.countries,
        genres     = detail.genres,
        trailerUrl = detail.trailerUrl
      )
    }

  private case class FilmInfo(
    name:           String,
    posterLink:     Option[String],
    filmLink:       Option[String],
    runtimeMinutes: Option[Int],
    releaseYear:    Option[Int]
  )

  /** Parse ONE day's `film-events` response into that day's bare movies: title/
   *  runtime/year/poster + the film-page link as filmUrl (the detail ref).
   *  Countries/genres/synopsis/cast/director/trailer are NOT in this JSON — they
   *  come from the per-film page via `fetchFilmDetail` (deferred). A malformed
   *  body yields no films (matching the old per-day `Try` swallow). */
  private def parseDay(raw: String, cinema: Cinema): Seq[CinemaMovie] = Try {
    val body   = Json.parse(raw) \ "body"
    val films  = (body \ "films").as[JsArray].value
    val events = (body \ "events").as[JsArray].value

    val info: Map[String, FilmInfo] = films.iterator.map { film =>
      (film \ "id").as[String] -> FilmInfo(
        name           = (film \ "name").as[String],
        posterLink     = (film \ "posterLink").asOpt[String].filter(_.nonEmpty),
        filmLink       = (film \ "link").asOpt[String].filter(_.nonEmpty),
        runtimeMinutes = (film \ "length").asOpt[Int],
        // releaseYear comes through as a String in the API (`"releaseYear":"2026"`);
        // asOpt[Int] silently fails on a String, so try String first.
        releaseYear    = (film \ "releaseYear").asOpt[String].flatMap(s => Try(s.toInt).toOption)
          .orElse((film \ "releaseYear").asOpt[Int]))
    }.toMap

    val slotsByFilm = events.iterator.flatMap { event =>
      val filmId     = (event \ "filmId").as[String]
      val bookingUrl = (event \ "bookingLink").asOpt[String].filter(_.nonEmpty)
      val room       = (event \ "auditorium").asOpt[String].filter(_.nonEmpty).map(CinemaCityClient.normalizeAuditorium)
      val attrs      = (event \ "attributeIds").asOpt[Seq[String]].getOrElse(Seq.empty).toSet
      // attrs is a kitchen-sink list (genre, 2d/3d, dubbed/subbed, IMAX, seating, age…);
      // pick out the screen-feature tokens we display.
      val format = List(
        if (attrs.contains("imax")) Some("IMAX") else None,
        if (attrs.contains("3d")) Some("3D") else if (attrs.contains("2d")) Some("2D") else None,
        if (attrs.contains("dubbed")) Some("DUB") else if (attrs.contains("subbed")) Some("NAP") else None
      ).flatten
      Try(LocalDateTime.parse((event \ "eventDateTime").as[String])).toOption
        .map(dt => filmId -> Showtime(dt, bookingUrl, room, format))
    }.toSeq.groupBy(_._1)

    slotsByFilm.toSeq.flatMap { case (filmId, slots) =>
      info.get(filmId).map { fi =>
        CinemaMovie(
          movie       = Movie(CinemaCityClient.cleanTitle(fi.name), fi.runtimeMinutes, fi.releaseYear, rawTitle = Some(fi.name)),
          cinema      = cinema,
          posterUrl   = fi.posterLink,
          filmUrl     = fi.filmLink,
          synopsis    = None,
          cast        = Seq.empty,
          director    = Seq.empty,
          showtimes   = slots.map(_._2).sortBy(_.dateTime),
          externalIds = Map("cc" -> filmId),
          trailerUrl  = None)
      }
    }
  }.getOrElse(Seq.empty)
}

object CinemaCityClient {

  /** The quickbook data-API base. Public so the 1:N `CinemaCityScraper`
   *  wrapper can derive its `scrapeHosts` from the same string the client
   *  fetches with. */
  val BaseApiUrl = "https://www.cinema-city.pl/pl/data-api-service/v1/quickbook/10103"

  /** Strip event/cycle decoration so a decorated screening collapses onto the
   *  same Movie — and enriches off the same clean title — as the regular run:
   *  "Ladies Night - X" (ladies' night), "X - powrót do kin" (re-release),
   *  "Kolekcja Mamoru Hosody: X" (anime retrospective).
   *
   *  The strip now lives in the editable rule set under the "cinema-city" key
   *  (see TitleRules); this delegates so the behaviour stays unit-testable
   *  here and documents the chain's rule key. Production cleaning happens
   *  centrally in `MovieCache.recordCinemaScrape`.
   *
   *  On top of the editable strips we also peel the trailing screen-format /
   *  version tag Cinema City bakes into `film.name` ("Afrykanska Przygoda 3D
   *  IMAX", "500 mil (lektor)") — the same tokens already ride each
   *  `Showtime.format`, so dropping them lets the format-decorated entry collapse
   *  onto the plain film and enrich off one title. A foreign-language dub
   *  ("Odyseja ukraiński dubbing") is a genuinely separate Ukrainian-language
   *  screening, NOT a format variant: stripping only the trailing "dubbing" would
   *  leave a dangling "ukraiński", so that variant is left whole. */
  def cleanTitle(name: String): String = {
    val decorated = services.movies.TitleNormalizer.cinemaClean("cinema-city", name)
    if (LanguageDubSuffixRe.findFirstIn(decorated).isDefined) decorated
    else ScraperParse.stripFormatTags(decorated)
  }

  // A trailing foreign-language dub marker ("… ukraiński dubbing") — left whole
  // so it stays a distinct card rather than being half-stripped to a dangling
  // language word. Anchored to the end so an interior word never matches.
  private val LanguageDubSuffixRe =
    """(?i)\b(?:ukraiński|ukrainian)\s+(?:dubbing|napisy|lektor)\s*$""".r

  /** Per-film metadata parsed from the public film page. Countries used to
   *  live in a `<p>Produkcja: …</p>` line; cast/director/synopsis come from
   *  the page's embedded `var filmDetails = {…}` JSON blob, which carries
   *  the same shape the in-page React component renders from. */
  case class Details(
    synopsis:   Option[String],
    cast:       Seq[String],
    director:   Seq[String],
    countries:  Seq[String],
    genres:     Seq[String],
    trailerUrl: Option[String]
  )

  object Details {
    val empty: Details = Details(None, Seq.empty, Seq.empty, Seq.empty, Seq.empty, None)
  }

  // Kinepolis' `auditorium` field returns names truncated to 5 chars without a
  // space — `Sala1`..`Sala9` for single-digit rooms, `Sal10`..`Sal18` for the
  // two-digit ones. Plaza's `auditorium` already comes through clean
  // (`Sala 1`, `Sala IMAX`), so non-matching input passes through unchanged.
  private val KinepolisShortRe = """^Sala?(\d+)$""".r

  def normalizeAuditorium(s: String): String = s match {
    case KinepolisShortRe(n) => s"Sala $n"
    case other               => other
  }

  // Embedded JS object: `var filmDetails = { ... };`. Single-line, JSON-valued.
  // The `(?s)` flag lets `.*?` span newlines if a future page wraps the
  // value; today it's a single line.
  private val FilmDetailsRe = """(?s)var\s+filmDetails\s*=\s*(\{.*?\})\s*;""".r

  // Fallback production-country line — `var filmDetails.releaseCountry` is
  // a single string ("USA"), while the page also renders a longer
  // `<p>Produkcja: Belgia, Francja, USA 2026</p>` line that's more reliable
  // for co-productions. We prefer that line; if it's missing we fall back
  // to `releaseCountry`.
  private val ProductionLineRe = """<p>Produkcja:\s*([^<]+?)\s*</p>""".r
  private val TrailingYearRe   = """\s+\d{4}\s*$""".r

  /** Parse the film-details page. Public for the spec. */
  def parseDetails(html: String): Details = {
    val js  = FilmDetailsRe.findFirstMatchIn(html).flatMap(m => Try(Json.parse(m.group(1))).toOption)
    val castRaw   = js.flatMap(j => (j \ "cast").asOpt[String]).filter(_.nonEmpty)
    val cast      = castRaw.map(s => tools.TextNormalization.dropTrailingPartialNameIfLong(s))
                      .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val director  = js.flatMap(j => (j \ "directors").asOpt[String]).filter(_.nonEmpty)
                      .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val synopsis  = js.flatMap(j => (j \ "synopsis").asOpt[String]).filter(_.nonEmpty)
      .map(tools.TextNormalization.stripHtml)
    val countries = parseCountries(html)
      .orElse(js.flatMap(j => (j \ "releaseCountry").asOpt[String]).filter(_.nonEmpty)
        .map(s => s.split(",").map(_.trim).filter(_.nonEmpty).toSeq))
      .getOrElse(Seq.empty)
    // `filmDetails.categoriesAttributes` is a list of English lowercase
    // genre tokens (`"comedy"`, `"drama"`, `"sci-fi"`). Map the common
    // ones onto the Polish labels TMDB/Filmweb use so all three sources
    // converge on the same spelling; unknown tokens get the empty list
    // (skip rather than guess at translation).
    val genres = js.flatMap(j => (j \ "categoriesAttributes").asOpt[JsArray])
      .map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(_.asOpt[String]).filter(_.nonEmpty)
      .flatMap(token => CategoryToPolish.get(token.toLowerCase))
      .distinct
    // Trailer: `filmDetails.videoLink` is a single YouTube watch URL when the
    // film has a trailer. `mediaList[].url` for `type=="Video"` is the same
    // URL surfaced under the media-gallery shape; we read it as a fallback
    // for films where `videoLink` is empty but a Video entry exists.
    val videoLink = js.flatMap(j => (j \ "videoLink").asOpt[String]).filter(_.nonEmpty)
    val mediaVideo = js.flatMap { j =>
      (j \ "mediaList").asOpt[JsArray].flatMap(_.value
        .find(m => (m \ "type").asOpt[String].contains("Video"))
        .flatMap(m => (m \ "url").asOpt[String])
        .filter(_.nonEmpty))
    }
    val trailerUrl = videoLink.orElse(mediaVideo)
      .flatMap(u => services.movies.TrailerEmbed.youTubeId(u)
        .map(id => s"https://www.youtube.com/watch?v=$id"))
    Details(synopsis = synopsis, cast = cast, director = director, countries = countries, genres = genres, trailerUrl = trailerUrl)
  }

  /** Cinema City's `categoriesAttributes` tokens → the Polish labels TMDB
   *  and Filmweb use. Covers the common cases; unknown tokens drop rather
   *  than getting passed through verbatim (English in a Polish-language UI
   *  would look like a bug). Extend as new tokens appear on production
   *  films — `WriteCinemaCity` re-records the fixture against live data. */
  private val CategoryToPolish: Map[String, String] = Map(
    "action"      -> "Akcja",
    "adventure"   -> "Przygodowy",
    "animation"   -> "Animacja",
    "anime"       -> "Anime",
    "biography"   -> "Biograficzny",
    "comedy"      -> "Komedia",
    "concert"     -> "Koncert",
    "crime"       -> "Kryminał",
    "documentary" -> "Dokumentalny",
    "drama"       -> "Dramat",
    "family"      -> "Familijny",
    "fantasy"     -> "Fantasy",
    "history"     -> "Historyczny",
    "horror"      -> "Horror",
    "musical"     -> "Musical",
    "music"       -> "Muzyczny",
    "mystery"     -> "Tajemnica",
    "opera"       -> "Opera",
    "romance"     -> "Romans",
    "sci-fi"      -> "Sci-Fi",
    "scifi"       -> "Sci-Fi",
    "sport"       -> "Sportowy",
    "thriller"    -> "Thriller",
    "war"         -> "Wojenny",
    "western"     -> "Western"
  )

  /** Extract production-country list from the `<p>Produkcja: …</p>` line.
   *  Returns None when the line is absent so the caller can decide whether
   *  to fall back to `filmDetails.releaseCountry`. */
  private[cinemas] def parseCountries(html: String): Option[Seq[String]] =
    ProductionLineRe.findFirstMatchIn(html).map { m =>
      TrailingYearRe.replaceFirstIn(m.group(1), "").trim
    }.filter(_.nonEmpty).map(_.split(",").map(_.trim).filter(_.nonEmpty).toSeq)
}

