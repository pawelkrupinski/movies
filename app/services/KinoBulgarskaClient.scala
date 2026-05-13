package clients

import models.{CinemaMovie, KinoBulgarska, Movie, Showtime}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, RealHttpFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

class KinoBulgarskaClient(http: HttpFetch = new RealHttpFetch()) {

  private val PageUrl = "http://kinobulgarska19.pl/repertuar"

  private val PolishMonths = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4,
    "maja" -> 5, "czerwca" -> 6, "lipca" -> 7, "sierpnia" -> 8,
    "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

  private val PremiereSuffixPat = """(?i)\s+–\s+poznańska premiera$""".r
  private val DatePat           = """(\d+)\s+(\w+)""".r

  private def normalizeTitle(raw: String): String = {
    val stripped = PremiereSuffixPat.replaceFirstIn(raw, "")
    if (stripped.isEmpty) stripped else stripped.head.toUpper + stripped.tail.toLowerCase
  }

  private def parsePolishDate(text: String): Option[LocalDate] =
    DatePat.findFirstMatchIn(text).flatMap { m =>
      PolishMonths.get(m.group(2)).map { month =>
        val day       = m.group(1).toInt
        val today     = LocalDate.now()
        val candidate = LocalDate.of(today.getYear, month, day)
        if (candidate.isBefore(today.minusDays(1))) candidate.plusYears(1) else candidate
      }
    }

  private val RuntimePat = """(\d+)\s*min""".r

  // movie-meta is formatted "reż. <DIRECTORS…>, <COUNTRIES…>, YEAR r., DURATION min."
  // First comma-separated chunk is the director, all chunks between director
  // and the year are production countries.
  private def extractMeta(section: Element): (Option[String], Option[String], Option[Int], Option[Int]) =
    Option(section.selectFirst("p.movie-meta")).map { meta =>
      val text = meta.text()
      if (!text.startsWith("reż. ")) (None, None, None, None)
      else {
        val afterRez = text.stripPrefix("reż. ")
        val parts    = afterRez.split(",\\s*").map(_.trim)
        val yearIdx  = parts.indexWhere(_.matches("\\d{4} r\\."))
        val director = if (yearIdx >= 1) Some(parts(0)).filter(_.nonEmpty) else None
        val country  = if (yearIdx > 1) Some(parts.slice(1, yearIdx).mkString(", ")).filter(_.nonEmpty) else None
        val year     = if (yearIdx >= 0) Try(parts(yearIdx).replaceAll("[^0-9]", "").toInt).toOption else None
        val runtime  = RuntimePat.findFirstMatchIn(text).flatMap(m => Try(m.group(1).toInt).toOption)
        (director, country, year, runtime)
      }
    }.getOrElse((None, None, None, None))

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc            = Jsoup.parse(html)
    val filmByUrl      = collection.mutable.Map[String, (String, Option[String], Option[String], Option[String], Option[String], Option[Int], Option[Int])]()
    val showtimesByUrl = collection.mutable.Map[String, collection.mutable.ListBuffer[Showtime]]()

    doc.select("article").asScala.foreach { article =>
      val dateOpt = Option(article.selectFirst("h3")).flatMap(h3 => parsePolishDate(h3.text()))

      article.select("section.clearfix").asScala.foreach { section =>
        for (link <- Option(section.selectFirst("h4 a[href]")); date <- dateOpt) {
          val filmUrl = link.attr("href")
          val title   = normalizeTitle(link.text())

          val timeOpt = Option(section.selectFirst(".start-info.clock")).flatMap { clockEl =>
            val text = clockEl.text().replaceAll("\\s+", "")
            Try {
              val parts = text.split(":")
              LocalDateTime.of(date, LocalTime.of(parts(0).toInt, parts(1).toInt))
            }.toOption
          }

          val room = Option(section.selectFirst(".show-type-badge a"))
            .map(_.text().trim)
            .filter(_.nonEmpty)

          if (!filmByUrl.contains(filmUrl)) {
            val posterUrl = Option(section.selectFirst("img[src]")).map { img =>
              img.attr("src").replaceAll("-\\d+x\\d+(\\.jpg)", "$1")
            }
            val synopsis                             = Option(section.selectFirst("p:not(.movie-meta)")).map(_.text()).filter(_.nonEmpty)
            val (director, country, year, runtime)   = extractMeta(section)
            filmByUrl(filmUrl) = (title, posterUrl, synopsis, director, country, year, runtime)
          }

          timeOpt.foreach { dateTime =>
            showtimesByUrl.getOrElseUpdate(filmUrl, collection.mutable.ListBuffer()) += Showtime(dateTime, None, room)
          }
        }
      }
    }

    filmByUrl.toSeq.flatMap { case (filmUrl, (title, posterUrl, synopsis, director, country, year, runtime)) =>
      showtimesByUrl.get(filmUrl).map { slots =>
        CinemaMovie(
          movie     = Movie(title, runtimeMinutes = runtime, releaseYear = year, country = country),
          cinema    = KinoBulgarska,
          posterUrl = posterUrl,
          filmUrl   = Some(filmUrl),
          synopsis  = synopsis,
          cast      = None,
          director  = director,
          showtimes = slots.toSeq.sortBy(_.dateTime)
        )
      }
    }
  }
}

object KinoBulgarskaClient {
  def fetch(): Seq[CinemaMovie] = new KinoBulgarskaClient().fetch()
}
