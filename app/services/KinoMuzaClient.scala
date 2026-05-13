package clients

import models.{CinemaMovie, KinoMuza, Movie, Showtime}
import org.jsoup.Jsoup
import tools.{HttpFetch, RealHttpFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

class KinoMuzaClient(http: HttpFetch = new RealHttpFetch()) {

  private val RepertoireUrl = "https://www.kinomuza.pl/repertuar/"

  private def parseDate(ddMM: String): Option[LocalDate] =
    Try {
      val parts     = ddMM.trim.split("\\.")
      val today     = LocalDate.now()
      val candidate = LocalDate.of(today.getYear, parts(1).toInt, parts(0).toInt)
      if (candidate.isBefore(today.minusDays(1))) candidate.plusYears(1) else candidate
    }.toOption

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl))

  private val RuntimePat = """(\d+)’""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc      = Jsoup.parse(html)
    val previews = doc.select("#movies .preview").asScala

    previews.flatMap { preview =>
      val title    = Option(preview.selectFirst(".preview-title")).map(_.text().trim).filter(_.nonEmpty)
      val filmUrl  = Option(preview.selectFirst("a[href*=/movie/]")).map(_.attr("href"))
      val posterUrl = Option(preview.selectFirst("img[data-src]")).map(_.attr("data-src"))
      val infoHtml  = Option(preview.selectFirst(".f1-bold p")).map(_.html()).getOrElse("")
      val infoText  = Option(preview.selectFirst(".f1-bold p")).map(_.text()).getOrElse("")
      // Info block is <br>-separated: line 0 = "reż. <names>", line 1 = country/-ies,
      // then year and runtime (also picked up by the regexes from the flat text).
      val infoLines = infoHtml.split("(?i)<br\\s*/?>").toSeq
                              .map(line => Jsoup.parse(line).text().trim)
                              .filter(_.nonEmpty)
      val director  = infoLines.headOption
        .map(_.replaceFirst("(?i)^\\s*reż\\.\\s*", "").trim)
        .filter(_.nonEmpty)
      // Line 1, if present, is the country list. Skip it if it instead carries a
      // year/runtime token (some entries omit the country line entirely).
      val country   = infoLines.lift(1)
        .filterNot(_.matches(".*\\b(?:19|20)\\d{2}\\b.*"))
        .filterNot(_.matches(".*\\d+\\s*[\\u2019'].*"))
        .filter(_.nonEmpty)
      val runtimeMinutes = RuntimePat.findFirstMatchIn(infoText).flatMap(m => Try(m.group(1).toInt).toOption)
      val releaseYear    = YearPat.findAllMatchIn(infoText).flatMap(m => Try(m.group(1).toInt).toOption).toSeq.headOption

      title.map { t =>
        val showtimes = preview.select(".table-row").asScala.flatMap { row =>
          val dateOpt = Option(row.selectFirst(".day")).flatMap(el => parseDate(el.text()))
          val items   = row.select(".ticket-list-item").asScala

          dateOpt.toSeq.flatMap { date =>
            items.flatMap { item =>
              val timeText   = Option(item.selectFirst(".ticket-hour")).map(_.text().trim)
              val bookingUrl = Option(item.selectFirst("a.ticket-buy")).map(_.attr("href"))
              val room       = Option(item.selectFirst("span.text-gold"))
                                 .map(_.parent().text().trim)
                                 .filter(_.nonEmpty)
              timeText.flatMap { t =>
                Try {
                  val parts = t.split(":")
                  Showtime(LocalDateTime.of(date, LocalTime.of(parts(0).toInt, parts(1).toInt)),
                           bookingUrl, room)
                }.toOption
              }
            }
          }
        }.toSeq.distinctBy(_.dateTime)

        CinemaMovie(
          movie     = Movie(t, runtimeMinutes, releaseYear, country = country),
          cinema    = KinoMuza,
          posterUrl = posterUrl,
          filmUrl   = filmUrl,
          synopsis  = None,
          cast      = None,
          director  = director,
          showtimes = showtimes.sortBy(_.dateTime)
        )
      }
    }.toSeq.filter(_.showtimes.nonEmpty)
  }
}

object KinoMuzaClient {
  def fetch(): Seq[CinemaMovie] = new KinoMuzaClient().fetch()
}
