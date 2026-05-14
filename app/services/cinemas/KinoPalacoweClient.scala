package services.cinemas

import models.{CinemaMovie, KinoPalacowe, Movie, Showtime}
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.util.Try

class KinoPalacoweClient(http: HttpFetch = new RealHttpFetch()) {

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

  // Polish country names. When at least one chunk between the director and the
  // year matches this set, we use it to partition co-director names (which
  // don't match) from countries. Without this, "reż. A, B, Polska 2025"
  // couldn't tell whether B is a co-director or the second country.
  private val CountryNames: Set[String] = Set(
    "Polska", "USA", "Niemcy", "Francja", "Włochy", "Hiszpania", "Belgia", "Maroko",
    "Łotwa", "Węgry", "Dania", "Norwegia", "Czechy", "Republika Czeska", "Irlandia",
    "Wielka Brytania", "Inne", "Holandia", "Szwecja", "Finlandia", "Szwajcaria",
    "Austria", "Portugalia", "Rosja", "Turcja", "Ukraina", "Białoruś", "Litwa",
    "Estonia", "Słowacja", "Rumunia", "Bułgaria", "Grecja", "Indie", "Pakistan",
    "Japonia", "Chiny", "Korea Południowa", "Korea Północna", "Iran", "Izrael",
    "Brazylia", "Argentyna", "Australia", "Kanada", "Meksyk", "Nowa Zelandia",
    "Egipt", "RPA", "Korea"
  )

  private case class FilmMeta(
    director:    Option[String],
    country:     Option[String],
    releaseYear: Option[Int],
    runtime:     Option[Int]
  )

  private val EmptyMeta = FilmMeta(None, None, None, None)

  private def fetchFilmMeta(filmUrl: String): FilmMeta =
    Try(http.get(filmUrl)).toOption.flatMap(parseFilmMeta).getOrElse(EmptyMeta)

  private def parseFilmMeta(html: String): Option[FilmMeta] =
    // Strip HTML first \u2014 the meta line is sometimes split across <span> tags
    //   "re\u017c. <span>Carla Sim\u00f3n, </span><span>Hiszpania 2025, 114'</span>"
    // which makes `[^<]+?` in MetaPat fail to cross the tag boundary. Also
    // normalise U+00A0 (non-breaking space) which Java's `\s` doesn't match.
    MetaPat.findFirstMatchIn(Jsoup.parse(html).text().replace("\u00a0", " ")).map { m =>
      val parts = m.group(1).trim.stripSuffix(",").trim
        .split(",").map(_.trim).filter(_.nonEmpty).toList
      val (countries, others) = parts.partition(CountryNames.contains)
      val (directorParts, countryParts) =
        if (countries.nonEmpty) (others, countries)
        // No known country matched — fall back to the simple "last chunk is the
        // country" rule so we still produce something for an exotic origin.
        else (parts.dropRight(1), parts.takeRight(1))
      FilmMeta(
        director    = Some(directorParts.mkString(", ")).filter(_.nonEmpty),
        country     = Some(countryParts.mkString(", ")).filter(_.nonEmpty),
        releaseYear = Try(m.group(2).toInt).toOption,
        runtime     = Try(m.group(3).toInt).toOption.filter(n => n >= 30 && n <= 300)
      )
    }

  def fetch(): Seq[CinemaMovie] = {
    val entries = fetchAllEntries()

    val pool = Executors.newFixedThreadPool(8)
    val metaByUrl: Map[String, FilmMeta] =
      try {
        entries.flatMap(_.filmUrl).distinct
          .map(url => url -> pool.submit[FilmMeta](() => fetchFilmMeta(url)))
          .map { case (url, f) => url -> f.get() }
          .toMap
      } finally pool.shutdown()

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
            country        = meta.country
          ),
          cinema    = KinoPalacowe,
          posterUrl = first.posterUrl,
          filmUrl   = first.filmUrl,
          synopsis  = first.synopsis,
          cast      = None,
          director  = meta.director,
          showtimes = sorted.map(entry => Showtime(entry.dateTime, entry.bookingUrl, entry.room))
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
      val title     = rawTitle.split(" \\| ").head.trim
        .stripPrefix("Poranek dla dzieci: ")
        .stripPrefix("DKF Zamek: ")
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
          synopsis       = (entry \ "lead").asOpt[String].map(_.trim).filter(_.nonEmpty)
        )
      }
    }
  }
}

object KinoPalacoweClient {
  def fetch(): Seq[CinemaMovie] = new KinoPalacoweClient().fetch()
}
