package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.util.Try

class KinoPalacoweClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoPalacowe
  private val BaseUrl = "https://kinopalacowe.pl"
  private val ApiBase = s"$BaseUrl/public/api/calendar/?widgetHash=widget_17943"

  // Each film page carries a single one-liner like
  //   "reż. David Fincher, Niemcy, USA 1999, 139'"
  // or with co-directors / year glued to country / typographic apostrophe:
  //   "reż. Grzegorz Wacławek, Marta Szymańska, Polska 2025, 88'"
  //   "reż. Mascha Schilinski, Niemcy2025, 149'"
  //   "reż. Luis Buñuel, Francja, Włochy 1967, 101’"
  //   "reż. Mamoru Hosoda, Japonia 2009. 114'"   ← period instead of comma
  // The 19xx/20xx year anchor + trailing `\d+'` pin down the end so a 4-digit
  // CSRF-token suffix elsewhere on the page can't masquerade as the year/runtime.
  private val MetaPat = """reż\.\s+([^<]+?)\s*(19\d{2}|20\d{2})\s*[,.]\s*(\d+)['’]""".r.unanchored

  // Trailer link — Pałacowe embeds the YouTube watch URL in the film page's
  // gallery: `<a class="gallery__movie ..." href="https://www.youtube.com/watch?v=…">`.
  // Other unrelated YouTube channel links also appear on the page (banner,
  // footer); the gallery class is what distinguishes the trailer's anchor.
  private val TrailerPat = """<a[^>]*class="gallery__movie[^"]*"[^>]*href="(https?://(?:www\.)?(?:youtube\.com|youtu\.be|player\.vimeo\.com|vimeo\.com)/[^"]+)"""".r

  // Some film pages — notably the "Kino bez barier" accessible screenings —
  // tack a genre onto the metadata blob, in two shapes:
  //   "… Kraje produkcji: USA. Gatunek: krótkometrażowy.<br>"  (colon)
  //   "… Kraj produkcji: Francja. Gatunek animowany.<br>"       (adjective, no colon)
  // Captured up to the first `.`/`<` that closes the sentence; comma-split so a
  // future multi-genre list still parses. Pałacowe's values are lowercase, so
  // they're title-cased to match the genres TMDB / Filmweb / Apollo contribute.
  private val GenrePat = """(?i)Gatunek:?\s+([^.<|]+)""".r

  private case class FilmMeta(
    director:    Seq[String],
    countries:   Seq[String],
    releaseYear: Option[Int],
    runtime:     Option[Int],
    genres:      Seq[String],
    trailerUrl:  Option[String]
  )

  private val EmptyMeta = FilmMeta(Seq.empty, Seq.empty, None, None, Seq.empty, None)

  private def fetchFilmMeta(filmUrl: String): FilmMeta =
    Try(http.get(filmUrl)).toOption.flatMap(parseFilmMeta).getOrElse(EmptyMeta)

  private def parseFilmMeta(html: String): Option[FilmMeta] = {
    val trailer = parseTrailer(html)
    val genres  = parseGenres(html)
    val metaFromLine =
    // Strip HTML first \u2014 the meta line is sometimes split across <span> tags
    //   "re\u017c. <span>Carla Sim\u00f3n, </span><span>Hiszpania 2025, 114'</span>"
    // which makes `[^<]+?` in MetaPat fail to cross the tag boundary. Also
    // normalise U+00A0 (non-breaking space) which Java's `\s` doesn't match.
    MetaPat.findFirstMatchIn(Jsoup.parse(html).text().replace("\u00a0", " ")).map { m =>
      val parts = m.group(1).trim.stripSuffix(",").trim
        .split(",").map(_.trim).filter(_.nonEmpty).toList
      val (countries, others) = parts.partition(CountryNames.isPolish)
      val (directorParts, countryParts) =
        if (countries.nonEmpty) (others, countries)
        // No known country matched — fall back to the simple "last chunk is the
        // country" rule so we still produce something for an exotic origin.
        else (parts.dropRight(1), parts.takeRight(1))
      FilmMeta(
        director    = directorParts.filter(_.nonEmpty),
        countries   = countryParts,
        releaseYear = Try(m.group(2).toInt).toOption,
        runtime     = Try(m.group(3).toInt).toOption.filter(n => n >= 30 && n <= 300),
        genres      = genres,
        trailerUrl  = trailer
      )
    }
    // The reż./year/runtime line is the primary signal, but a film page can
    // still carry a trailer and/or a genre without it (some pages truncate the
    // meta block). Surface a meta carrying whichever of those parsed so they
    // reach the cache anyway.
    metaFromLine.orElse(
      if (trailer.isDefined || genres.nonEmpty)
        Some(EmptyMeta.copy(genres = genres, trailerUrl = trailer))
      else None
    )
  }

  /** Genres from the `Gatunek[:] <list>` marker in the metadata blob, when
   *  present. Returns the empty list for the many pages that omit it. */
  def parseGenres(html: String): Seq[String] =
    GenrePat.findFirstMatchIn(html).map { m =>
      m.group(1).trim.stripSuffix(",").trim
        .split(",").map(_.trim).filter(_.nonEmpty)
        .map(tools.TextNormalization.titleCaseIfAllLower).toSeq
    }.getOrElse(Seq.empty)

  /** Pałacowe trailer URL parsed from the film page's gallery anchor.
   *  Returns the canonical `youtube.com/watch?v=ID` form when the captured
   *  URL parses as a YouTube video; vimeo URLs (a few pages use them) are
   *  passed through unchanged for the view layer's TrailerEmbed to handle. */
  def parseTrailer(html: String): Option[String] =
    TrailerPat.findFirstMatchIn(html).map(_.group(1)).flatMap(ScraperParse.canonicalTrailer)

  def fetch(): Seq[CinemaMovie] = {
    val entries = fetchAllEntries()

    val metaByUrl: Map[String, FilmMeta] =
      ParallelDetailFetch("kino-palacowe-meta", entries.flatMap(_.filmUrl).distinct, 2.minutes)(fetchFilmMeta)

    entries
      .groupBy(_.movieTitle)
      .toSeq
      .map { case (title, group) =>
        val sorted = group.sortBy(_.dateTime)
        val first  = sorted.head
        val meta   = first.filmUrl.flatMap(metaByUrl.get).getOrElse(EmptyMeta)
        CinemaMovie(
          movie     = Movie(
            title          = title,
            runtimeMinutes = meta.runtime,
            releaseYear    = meta.releaseYear,
            countries      = meta.countries,
            genres         = meta.genres
          ),
          cinema    = KinoPalacowe,
          posterUrl = first.posterUrl,
          filmUrl   = first.filmUrl,
          synopsis   = first.synopsis,
          cast       = Seq.empty,
          director   = meta.director,
          showtimes  = sorted.map(entry => Showtime(entry.dateTime, entry.bookingUrl, entry.room)),
          trailerUrl = meta.trailerUrl
        )
      }
  }

  private def fetchAllEntries(): Seq[ScreeningEntry] = {
    var entries = Seq.empty[ScreeningEntry]
    var page    = 1
    var hasNext = true
    while (hasNext) {
      val (pageEntries, next) = fetchPage(page)
      entries = entries ++ pageEntries
      hasNext = next
      page   += 1
    }
    entries
  }

  private def fetchPage(page: Int): (Seq[ScreeningEntry], Boolean) =
    parseJson(http.get(s"$ApiBase&page=$page"))

  private case class ScreeningEntry(
    movieTitle:     String,
    dateTime:       LocalDateTime,
    posterUrl:      Option[String],
    filmUrl:        Option[String],
    bookingUrl:     Option[String],
    room:           Option[String] = None,
    runtimeMinutes: Option[Int]    = None,
    synopsis:       Option[String] = None
  )

  private def parseJson(json: String): (Seq[ScreeningEntry], Boolean) = {
    val root    = Json.parse(json)
    val hasNext = (root \ "next").asOpt[Boolean].getOrElse(false)
    val days    = (root \ "results").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)

    val entries = days.flatMap { day =>
      (day \ "subsections").asOpt[JsArray].map(_.value).getOrElse(Seq.empty).flatMap { sub =>
        (sub \ "entries").asOpt[JsArray].map(_.value).getOrElse(Seq.empty).flatMap(parseEntry)
      }
    }

    (entries.toSeq, hasNext)
  }

  private def parseEntry(entry: JsValue): Option[ScreeningEntry] = {
    val filmUrl    = (entry \ "url").asOpt[String].filter(_.contains("/filmy/"))
    val ticketType = (entry \ "ticket_type").asOpt[Int].getOrElse(0)

    if (filmUrl.isEmpty || ticketType != 2) None
    else {
      val rawTitle  = (entry \ "title").asOpt[String].getOrElse("")
      val title     = KinoPalacoweClient.cleanTitle(rawTitle.split(" \\| ").head.trim)
      val startDate = (entry \ "start_date").asOpt[String]
      val startTime = (entry \ "start_time").asOpt[String]

      for {
        dateStr <- startDate
        timeStr <- startTime
        if title.nonEmpty
      } yield {
        val dateTime  = LocalDateTime.parse(s"${dateStr}T${timeStr}")
        val photoPath = (entry \ "photo" \ "sizes" \ "lg").asOpt[String]
                          .filter(_.nonEmpty)
                          .orElse((entry \ "photo" \ "image").asOpt[String].filter(_.nonEmpty))
        val room = (entry \ "category").asOpt[String].filter(_.nonEmpty)
        val runtime = (entry \ "duration").asOpt[Int]
                        .orElse((entry \ "runtime").asOpt[Int])
                        .orElse((entry \ "length").asOpt[Int])
        ScreeningEntry(
          movieTitle     = title,
          dateTime       = dateTime,
          posterUrl      = photoPath.map(path => if (path.startsWith("http")) path else s"$BaseUrl$path"),
          filmUrl        = filmUrl,
          bookingUrl     = (entry \ "ticket_url").asOpt[String].filter(_.nonEmpty),
          room           = room,
          runtimeMinutes = runtime,
          synopsis       = (entry \ "lead").asOpt[String].map(tools.TextNormalization.stripHtml).filter(_.nonEmpty)
        )
      }
    }
  }
}

object KinoPalacoweClient {

  /** Strip cycle decoration so a decorated screening collapses onto the same
   *  canonical row — and enriches off the same clean title — as the regular
   *  run: "Poranek dla dzieci: X" (kids' matinee), "DKF Zamek: X" (film-club),
   *  "WAJDA: re-wizje. X" (the Andrzej Wajda retrospective, which Kino Apollo
   *  lists under its own `Cykl „Wajda: re-wizje"` prefix — strip the Pałacowe
   *  shape here so both cinemas' screenings land on one row, not two). Public
   *  so the strip is unit-testable directly. */
  def cleanTitle(title: String): String =
    title
      .stripPrefix("Poranek dla dzieci: ")
      .stripPrefix("DKF Zamek: ")
      .stripPrefix("WAJDA: re-wizje. ")
}

