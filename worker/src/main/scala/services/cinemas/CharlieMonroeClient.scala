package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDateTime, ZonedDateTime}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

class CharlieMonroeClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = CharlieMonroe
  private val PageUrl = "https://kinomalta.pl/seanse"

  // Per-film detail page carries production countries in a
  //   `<span class="wpmoly … meta label">Kraj:&nbsp;</span><span … meta value">
  //    <a …>Country</a><a …>Other</a></span>`
  // block (one `<a>` per country for co-productions). The listing doesn't
  // expose it, so we fetch each film page in parallel and gracefully fall
  // back to an empty list on any failure.
  private val CountryAnchorPat =
    """(?s)class="wpmoly[^"]*meta label[^"]*">Kraj:.*?class="wpmoly[^"]*meta value[^"]*">(.*?)</span>""".r
  private val AnchorTextPat    = """<a[^>]*>([^<]+)</a>""".r

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PageUrl)

  def fetch(): Seq[CinemaMovie] = {
    val movies = parseHtml(http.get(PageUrl))
    val urls   = movies.flatMap(_.filmUrl).distinct
    if (urls.isEmpty) movies
    else {
      val countries = fetchCountries(urls)
      movies.map { m =>
        val cs = m.filmUrl.flatMap(countries.get).getOrElse(Seq.empty)
        if (cs.isEmpty) m else m.copy(movie = m.movie.copy(countries = cs))
      }
    }
  }

  private def fetchCountries(urls: Seq[String]): Map[String, Seq[String]] =
    ParallelDetailFetch("charlie-monroe-countries", urls, 1.minute)(fetchCountriesFor)

  private def fetchCountriesFor(detailUrl: String): Seq[String] =
    Try(http.get(detailUrl)).toOption.map(parseCountries).getOrElse(Seq.empty)

  def parseCountries(html: String): Seq[String] =
    CountryAnchorPat.findFirstMatchIn(html).toSeq.flatMap { m =>
      AnchorTextPat.findAllMatchIn(m.group(1)).map(_.group(1).trim).filter(_.nonEmpty).toSeq
    }

  /** Genres from a listing card's `div.meta` line, which reads
   *  "Czarna komedia, Thriller / 139 min" — the comma-separated genre list
   *  precedes the ` / ` runtime. The runtime segment is filtered out so a
   *  card carrying only "NN min" yields no spurious genre. */
  def parseMetaGenres(metaText: String): Seq[String] =
    metaText.split("/").headOption.toSeq.flatMap(
      _.split(",").map(_.trim)
        .filter(_.nonEmpty)
        .filterNot(_.matches("(?i).*\\d+\\s*min.*"))
        .map(tools.TextNormalization.titleCaseIfAllLower)
    )

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html)

    val screenings = doc.select("script[type=application/ld+json]").asScala
      .flatMap(el => Try(Json.parse(el.data())).toOption)
      .flatMap(parseScreeningEvent)
      .toSeq

    val roomMap = collection.mutable.Map[(String, String, String), String]()
    val detailsByTitle = doc.select("article.movie-card").asScala.flatMap { article =>
      Option(article.selectFirst("h2.title")).map(_.text().trim).map { title =>
        article.select(".showtimes-row").asScala.foreach { row =>
          val hall = Option(row.selectFirst(".hall-label")).map(_.text().trim).filter(_.nonEmpty)
          hall.foreach { hallName =>
            row.select("button.btn-showtime").asScala.foreach { btn =>
              val time = Option(btn.selectFirst("span.time")).map(_.text().trim)
              val date = Option(btn.selectFirst("span.price")).flatMap(el => normalizeDateKey(el.text()))
              for (t <- time; d <- date)
                roomMap((title.toLowerCase, d, t)) = hallName
            }
          }
        }
        val filmUrl   = Option(article.selectFirst("a[href*=/movies/]")).map(_.attr("href"))
        val synopsis  = Option(article.selectFirst("p.desc")).map(_.text().replaceAll("\\.{3,}$", "").trim).filter(_.nonEmpty)
        val posterUrl = Option(article.selectFirst("img[data-src]")).map(_.attr("data-src"))
        val genres    = Option(article.selectFirst("div.meta")).map(_.text()).map(parseMetaGenres).getOrElse(Seq.empty)
        title -> ArticleDetails(filmUrl, synopsis, posterUrl, genres)
      }
    }.toMap

    screenings
      .groupBy(_.movie.title)
      .toSeq
      .map { case (title, events) =>
        val sorted  = events.sortBy(_.dateTime)
        val details = detailsByTitle.getOrElse(title, ArticleDetails(None, None, None, Seq.empty))
        CinemaMovie(
          movie     = sorted.head.movie.copy(genres = details.genres),
          cinema    = CharlieMonroe,
          posterUrl = details.posterUrl.orElse(sorted.flatMap(_.posterUrl).headOption),
          filmUrl   = details.filmUrl,
          synopsis  = details.synopsis,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = sorted.map { event =>
            val dKey    = dateKey(event.dateTime.getYear, event.dateTime.getMonthValue, event.dateTime.getDayOfMonth)
            val timeKey = "%02d:%02d".format(event.dateTime.getHour, event.dateTime.getMinute)
            val room    = roomMap.get((title.toLowerCase, dKey, timeKey))
            Showtime(event.dateTime, event.bookingUrl, room)
          }
        )
      }
  }

  /** The listing renders the showtime date in `span.price` without
   *  zero-padding the day ("2.06.2026"), so normalise it to the same
   *  zero-padded "DD.MM.YYYY" the JSON-LD-derived lookup key uses — else
   *  any day (or month) < 10 fails to match and the room is dropped. */
  private def normalizeDateKey(raw: String): Option[String] =
    raw.trim.split('.') match {
      case Array(d, m, y) => Try(dateKey(y.toInt, m.toInt, d.toInt)).toOption
      case _              => None
    }

  private def dateKey(year: Int, month: Int, day: Int): String =
    "%02d.%02d.%d".format(day, month, year)

  private case class ArticleDetails(filmUrl: Option[String], synopsis: Option[String], posterUrl: Option[String], genres: Seq[String])

  private case class ScreeningEntry(
    movie:      Movie,
    dateTime:   LocalDateTime,
    posterUrl:  Option[String],
    bookingUrl: Option[String]
  )

  private def parseDuration(iso: String): Option[Int] =
    """PT(?:(\d+)H)?(?:(\d+)M)?""".r.findFirstMatchIn(iso).map { m =>
      Option(m.group(1)).map(_.toInt * 60).getOrElse(0) +
      Option(m.group(2)).map(_.toInt).getOrElse(0)
    }.filter(_ > 0)

  private def parseScreeningEvent(json: JsValue): Option[ScreeningEntry] =
    Try {
      val eventType = (json \ "@type").asOpt[String]
        .orElse((json \ "@type").asOpt[JsArray].flatMap(_.value.headOption.flatMap(_.asOpt[String])))

      eventType.filter(_ == "ScreeningEvent").flatMap { _ =>
        for {
          title     <- (json \ "workPresented" \ "name").asOpt[String]
          startDate <- (json \ "startDate").asOpt[String]
        } yield {
          val runtimeMinutes = (json \ "workPresented" \ "duration").asOpt[String].flatMap(parseDuration)
          val releaseYear    = (json \ "workPresented" \ "dateCreated").asOpt[Int]
                                 .orElse((json \ "workPresented" \ "copyrightYear").asOpt[Int])
          ScreeningEntry(
            movie      = Movie(title, runtimeMinutes, releaseYear),
            dateTime   = ZonedDateTime.parse(startDate).toLocalDateTime,
            posterUrl  = (json \ "workPresented" \ "image").asOpt[String].filter(_.nonEmpty),
            bookingUrl = (json \ "offers" \ "url").asOpt[String].filter(_.nonEmpty)
          )
        }
      }
    }.toOption.flatten
}

