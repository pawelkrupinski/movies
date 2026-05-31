package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Showtime}
import play.api.libs.json._
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

class CinemaCityClient(http: HttpFetch) {

  private val BaseApiUrl = "https://www.cinema-city.pl/pl/data-api-service/v1/quickbook/10103"
  private val FarFuture  = "2027-01-01"

  def fetch(cinemaId: String, cinema: Cinema): Seq[CinemaMovie] = {
    val datesUrl = s"$BaseApiUrl/dates/in-cinema/$cinemaId/until/$FarFuture?attr=&lang=pl_PL"
    val dates: Seq[LocalDate] = Try {
      (Json.parse(http.get(datesUrl)) \ "body" \ "dates")
        .as[Seq[String]]
        .flatMap(d => Try(LocalDate.parse(d)).toOption)
    }.getOrElse(Seq.empty)

    val pendingEvents = dates.map { date =>
      val url = s"$BaseApiUrl/film-events/in-cinema/$cinemaId/at-date/$date?attr=&lang=pl_PL"
      date -> http.getAsync(url)
    }

    case class FilmInfo(
      name:           String,
      posterLink:     Option[String],
      filmLink:       Option[String],
      runtimeMinutes: Option[Int],
      releaseYear:    Option[Int],
      countries:      Seq[String]
    )

    val allFilms  = collection.mutable.Map[String, FilmInfo]()
    val allEvents = collection.mutable.ListBuffer[(String, LocalDateTime, Option[String], Option[String], List[String])]()

    for ((_, future) <- pendingEvents) {
      Try {
        val body   = Json.parse(future.join()) \ "body"
        val films  = (body \ "films").as[JsArray].value
        val events = (body \ "events").as[JsArray].value

        for (film <- films) {
          val id = (film \ "id").as[String]
          if (!allFilms.contains(id)) {
            allFilms(id) = FilmInfo(
              name           = (film \ "name").as[String],
              posterLink     = (film \ "posterLink").asOpt[String].filter(_.nonEmpty),
              filmLink       = (film \ "link").asOpt[String].filter(_.nonEmpty),
              runtimeMinutes = (film \ "length").asOpt[Int],
              // releaseYear comes through as a String in the API
              // (`"releaseYear":"2026"`) — asOpt[Int] silently fails on a
              // String and the field stayed None for every film.
              releaseYear    = (film \ "releaseYear").asOpt[String].flatMap(s => Try(s.toInt).toOption)
                .orElse((film \ "releaseYear").asOpt[Int]),
              countries      = Seq.empty   // filled in below from the per-film details page
            )
          }
        }

        for (event <- events) {
          val filmId      = (event \ "filmId").as[String]
          val dateTimeStr = (event \ "eventDateTime").as[String]
          val bookingUrl  = (event \ "bookingLink").asOpt[String].filter(_.nonEmpty)
          val room        = (event \ "auditorium").asOpt[String].filter(_.nonEmpty)
                            .map(CinemaCityClient.normalizeAuditorium)
          val attrs       = (event \ "attributeIds").asOpt[Seq[String]].getOrElse(Seq.empty).toSet
          // attrs comes in as a kitchen-sink list (genre, 2d/3d, dubbed/subbed/original-lang-*, IMAX,
          // seating type, age rating…). Pick out the screen-feature tokens we want to display.
          val format      = List(
            if (attrs.contains("imax"))  Some("IMAX") else None,
            if (attrs.contains("3d"))    Some("3D")
            else if (attrs.contains("2d")) Some("2D")
            else None,
            if (attrs.contains("dubbed")) Some("DUB")
            else if (attrs.contains("subbed")) Some("NAP")
            else None
          ).flatten
          Try(LocalDateTime.parse(dateTimeStr)).foreach { dateTime =>
            allEvents += ((filmId, dateTime, bookingUrl, room, format))
          }
        }
      }
    }

    // Per-film details page fetch — neither country nor cast/director/synopsis
    // are in the film-events JSON, but the public film page embeds a
    // `var filmDetails = { ... }` JSON blob with all of them plus the
    // production-country line. Fetched in parallel; a failed fetch or
    // missing field leaves the row's content fields empty (the rest of
    // the row stays usable).
    val detailsByFilmId: Map[String, CinemaCityClient.Details] = {
      val pending = allFilms.toSeq.flatMap { case (id, info) =>
        info.filmLink.map { link => id -> http.getAsync(link) }
      }
      pending.map { case (id, fut) =>
        id -> Try(fut.join()).toOption.map(CinemaCityClient.parseDetails).getOrElse(CinemaCityClient.Details.empty)
      }.toMap
    }
    allFilms.foreach { case (id, info) =>
      detailsByFilmId.get(id).filter(_.countries.nonEmpty)
        .foreach(d => allFilms.update(id, info.copy(countries = d.countries)))
    }

    allEvents
      .groupBy(_._1)
      .toSeq
      .flatMap { case (filmId, slots) =>
        allFilms.get(filmId).map { info =>
          val details = detailsByFilmId.getOrElse(filmId, CinemaCityClient.Details.empty)
          CinemaMovie(
            movie       = Movie(
              CinemaCityClient.cleanTitle(info.name),
              info.runtimeMinutes,
              info.releaseYear,
              countries = info.countries,
              genres    = details.genres
            ),
            cinema      = cinema,
            posterUrl   = info.posterLink,
            filmUrl     = info.filmLink,
            synopsis    = details.synopsis,
            cast        = details.cast,
            director    = details.director,
            showtimes   = slots.toSeq.map { case (_, dateTime, bookingUrl, room, format) =>
              Showtime(dateTime, bookingUrl, room, format)
            }.sortBy(_.dateTime),
            externalIds = Map("cc" -> filmId),
            trailerUrl  = details.trailerUrl
          )
        }
      }
  }
}

object CinemaCityClient {

  /** Strip event/cycle decoration so a decorated screening collapses onto the
   *  same Movie — and enriches off the same clean title — as the regular run:
   *  "Ladies Night - X" (ladies' night), "X - powrót do kin" (re-release),
   *  "Kolekcja Mamoru Hosody: X" (anime retrospective). Without it the
   *  decorated string is its own row that TMDB/Filmweb can't resolve. Public
   *  so the strip is unit-testable directly. */
  def cleanTitle(name: String): String =
    name
      .stripPrefix("Ladies Night - ")
      .stripSuffix(" - powrót do kin")
      .replaceFirst("^Kolekcja\\s+Mamoru\\s+Hosody:\\s*", "")

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

