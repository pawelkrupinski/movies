package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{DaemonExecutors, HttpFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import scala.util.Try

class KinoMuzaClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoMuza
  private val RepertoireUrl = "https://www.kinomuza.pl/repertuar/"

  private def parseDate(ddMM: String): Option[LocalDate] =
    Try {
      val parts     = ddMM.trim.split("\\.")
      val today     = LocalDate.now()
      val candidate = LocalDate.of(today.getYear, parts(1).toInt, parts(0).toInt)
      if (candidate.isBefore(today.minusDays(1))) candidate.plusYears(1) else candidate
    }.toOption

  def fetch(): Seq[CinemaMovie] = {
    val listing = parseHtml(http.get(RepertoireUrl))
    val urls    = listing.flatMap(_.filmUrl).distinct
    if (urls.isEmpty) listing
    else {
      val synopses = fetchSynopses(urls)
      listing.map { cm =>
        cm.copy(synopsis = cm.filmUrl.flatMap(synopses.get).flatten.orElse(cm.synopsis))
      }
    }
  }

  // Detail-page fetch — concurrent across films. Two workers, not the 5
  // CLAUDE.md mentions for undocumented services: Muza is a small Poznań
  // cinema, not a production API, and 5 simultaneous requests at scrape
  // start trips their burst limiter — subsequent requests stall past our
  // 30-s timeout and every synopsis comes back None for that tick. Two
  // workers still finishes ~50 detail pages well inside the 5-minute
  // scrape window while staying gentle. Failed fetches map to None
  // silently so a flaky detail page doesn't drop the whole row from the
  // repertoire.
  private def fetchSynopses(urls: Seq[String]): Map[String, Option[String]] = {
    // Cap concurrency at 2 — Muza's upstream burst-limits and 5 parallel GETs
    // starting at the scrape-tick boundary stalls every request past our
    // 30-s timeout. `boundedEC` parks the extra virtual threads cheaply.
    val ec = DaemonExecutors.boundedEC("kino-muza-details", maxConcurrent = 2)
    try {
      val futures = urls.map(url => Future(url -> Try(http.get(url)).toOption.flatMap(parseSynopsis))(ec))
      Await.result(Future.sequence(futures)(implicitly, ec), 5.minutes).toMap
    } finally ec.shutdown()
  }

  // Muza's detail pages render the synopsis in the first `paragraph`-classed
  // column of the film header — `div.col-lg-7.paragraph`. A second
  // `.paragraph` block appears further down for "Treści wrażliwe" (sensitive
  // content); scoping to the col-lg-7 variant keeps that out. Multiple
  // `<p>` children get joined with a blank line so original paragraph
  // boundaries survive the homepage card formatter.
  def parseSynopsis(html: String): Option[String] = {
    val doc = Jsoup.parse(html)
    val paragraphs = doc.select("div.col-lg-7.paragraph > p").asScala
      .map(_.text().trim)
      .filter(_.nonEmpty)
      .toSeq
    if (paragraphs.isEmpty) None else Some(paragraphs.mkString("\n\n"))
  }

  private val RuntimePat = """(\d+)’""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  // Strip the "| najlepsze z najgorszych" series tag Muza appends to films
  // featured in that recurring programme — same film, regular title in the
  // rest of the corpus.
  private val SeriesSuffix = """(?i)\s*\|\s*najlepsze\s+z\s+najgorszych\s*$""".r

  private def cleanTitle(raw: String): String =
    SeriesSuffix.replaceFirstIn(raw, "").trim

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc      = Jsoup.parse(html)
    val previews = doc.select("#movies .preview").asScala

    previews.flatMap { preview =>
      val title    = Option(preview.selectFirst(".preview-title")).map(_.text().trim).map(cleanTitle).filter(_.nonEmpty)
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
      // Split on "," for co-productions ("Polska, Niemcy" → two entries).
      val countries = infoLines.lift(1)
        .filterNot(_.matches(".*\\b(?:19|20)\\d{2}\\b.*"))
        .filterNot(_.matches(".*\\d+\\s*[\\u2019'].*"))
        .filter(_.nonEmpty)
        .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      val runtimeMinutes = RuntimePat.findFirstMatchIn(infoText).flatMap(m => Try(m.group(1).toInt).toOption)
      val releaseYear    = YearPat.findAllMatchIn(infoText).flatMap(m => Try(m.group(1).toInt).toOption).toSeq.headOption

      title.map { title =>
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
          movie     = Movie(title, runtimeMinutes, releaseYear, countries = countries),
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

