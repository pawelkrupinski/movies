package clients

import models.{CinemaMovie, Movie, Rialto, Showtime}
import org.jsoup.Jsoup
import tools.{HttpFetch, RealHttpFetch}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

class RialtoClient(http: HttpFetch = new RealHttpFetch()) {

  private val RepertoireUrl = "https://www.kinorialto.poznan.pl/repertuar/"
  private val BaseUrl       = "https://www.kinorialto.poznan.pl"

  private def stripCyclePrefix(title: String): String = {
    val colonIdx = title.indexOf(": ")
    if (colonIdx > 0 && colonIdx < 30) {
      val prefix = title.substring(0, colonIdx)
      if (prefix != prefix.toUpperCase) title.substring(colonIdx + 2) else title
    } else title
  }

  // Rialto presents most titles in upper case; lower-case them so that they
  // merge case-insensitively with the same films from other cinemas.
  private def normalizeCase(title: String): String =
    if (title.isEmpty) title
    else title.head.toUpper + title.tail.toLowerCase

  private val DateTimePat = """- (\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}) -""".r
  private val RuntimePat  = """(\d+)\s*min""".r
  private val YearPat     = """\b((?:19|20)\d{2})\b""".r

  private case class FilmEntry(
    title:          String,
    eventUrl:       String,
    posterUrl:      Option[String],
    synopsis:       Option[String],
    director:       Option[String],
    runtimeMinutes: Option[Int]     = None,
    releaseYear:    Option[Int]     = None
  )

  def fetch(): Seq[CinemaMovie] = {
    val filmEntries = parseRepertoire(http.get(RepertoireUrl))

    val pendingPages = filmEntries.map { entry =>
      entry -> http.getAsync(entry.eventUrl)
    }
    val showtimesByUrl: Map[String, Seq[Showtime]] = pendingPages.flatMap { case (entry, future) =>
      Try(parseEventPage(future.join())).toOption.map(entry.eventUrl -> _)
    }.toMap

    filmEntries
      .groupBy(_.title.toUpperCase)
      .values
      .flatMap { group =>
        val primary      = group.head
        val allShowtimes = group.flatMap(e => showtimesByUrl.getOrElse(e.eventUrl, Seq.empty))
                                .sortBy(_.dateTime)
        if (allShowtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(primary.title, primary.runtimeMinutes, primary.releaseYear),
          cinema    = Rialto,
          posterUrl = primary.posterUrl,
          filmUrl   = Some(primary.eventUrl),
          synopsis  = primary.synopsis,
          cast      = None,
          director  = primary.director,
          showtimes = allShowtimes
        ))
      }
      .toSeq
  }

  private def parseRepertoire(html: String): Seq[FilmEntry] =
    html.split("""<hr class="table-seperator"/>""").toSeq.flatMap { blockHtml =>
      val block = Jsoup.parseBodyFragment(blockHtml)

      Option(block.selectFirst("""a[title^="Film:"]""")).map { link =>
        val rawEventUrl = link.attr("href")
        val eventUrl    = if (rawEventUrl.startsWith("http")) rawEventUrl else BaseUrl + rawEventUrl

        val rawTitle = Option(block.selectFirst("div.title")).map(_.text().trim).getOrElse("")
        val title    = normalizeCase(stripCyclePrefix(rawTitle))

        // Restrict to the actual movie image container — otherwise the first block
        // (which still includes the page header) picks up the Facebook-login icon.
        val posterUrl = Option(block.selectFirst(".list-item-image img[src], .image img[src]"))
                          .map(_.attr("src"))

        val (synopsis, director, runtime, year) = Option(block.selectFirst("span.text")) match {
          case None => (None, None, None, None)
          case Some(span) =>
            val lines    = span.html().split("(?i)<br\\s*/?>").map(l => Jsoup.parseBodyFragment(l).body().text().trim)
            val dir      = lines.find(_.startsWith("Reż. ")).map(_.stripPrefix("Reż. ").trim)
            val emptyIdx = lines.indexWhere(_.isEmpty)
            val synLines = if (emptyIdx >= 0) lines.drop(emptyIdx + 1) else Array.empty[String]
            val synText  = synLines.filter(_.nonEmpty).mkString(" ").trim
            val fullText = lines.mkString(" ")
            val rt       = RuntimePat.findFirstMatchIn(fullText).flatMap(m => Try(m.group(1).toInt).toOption)
            val yr       = YearPat.findAllMatchIn(fullText).flatMap(m => Try(m.group(1).toInt).toOption).toSeq.headOption
            (Option(synText).filter(_.nonEmpty), dir, rt, yr)
        }

        FilmEntry(title, eventUrl, posterUrl, synopsis, director, runtime, year)
      }
    }

  private def parseEventPage(html: String): Seq[Showtime] = {
    val doc = Jsoup.parse(html)
    doc.select("a.b24-button.show[href][title]").asScala.flatMap { a =>
      val href  = a.attr("href")
      val title = a.attr("title")
      val url   = if (href.startsWith("http")) href else BaseUrl + href
      DateTimePat.findFirstMatchIn(title).flatMap { dt =>
        Try(LocalDateTime.parse(s"${dt.group(1)}T${dt.group(2)}")).toOption
          .map(dateTime => Showtime(dateTime, Some(url)))
      }
    }.toSeq.distinctBy(_.dateTime)
  }
}

object RialtoClient {
  def fetch(): Seq[CinemaMovie] = new RialtoClient().fetch()
}
