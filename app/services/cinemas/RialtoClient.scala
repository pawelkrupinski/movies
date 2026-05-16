package services.cinemas

import models.{Cinema, CinemaMovie, Movie, Rialto, Showtime}
import org.jsoup.Jsoup
import tools.{HttpFetch, RealHttpFetch}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

class RialtoClient(http: HttpFetch = new RealHttpFetch()) extends CinemaScraper {

  val cinema: Cinema = Rialto
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
  // merge case-insensitively with the same films from other cinemas. Sentence-
  // case rather than just-first-letter so multi-sentence titles read naturally
  // ("Mavka. Prawdziwy mit", not "Mavka. prawdziwy mit").
  //
  // After `. ` (period + space) we capitalize the next letter only when the
  // preceding token looks like a sentence-ending word — a 4+ letter run, or a
  // digit. That keeps "Mavka. Prawdziwy" and "skarpetek 3. Ale kosmos" right
  // while leaving Polish abbreviations untouched ("ang. napisami", "reż.
  // Jana", "ul. Świętego Marcina") — those abbreviations are 2–3 letters.
  private def normalizeCase(title: String): String = {
    if (title.isEmpty) return title
    val chars = title.toLowerCase.toCharArray
    chars(0) = chars(0).toUpper
    var i = 0
    while (i + 2 < chars.length) {
      if (chars(i) == '.' && chars(i + 1) == ' ' && precedingTokenEndsSentence(chars, i))
        chars(i + 2) = chars(i + 2).toUpper
      i += 1
    }
    new String(chars)
  }

  /** True if the character run ending at index `dotIdx - 1` looks like a
   *  sentence-ending token: a digit (sequel/chapter number) or a 4+ letter
   *  word. Counts contiguous letters/digits backwards from `dotIdx - 1`. */
  private def precedingTokenEndsSentence(chars: Array[Char], dotIdx: Int): Boolean = {
    if (dotIdx == 0) return false
    val prev = chars(dotIdx - 1)
    if (prev.isDigit) return true
    if (!prev.isLetter) return false
    var letters = 0
    var j = dotIdx - 1
    while (j >= 0 && chars(j).isLetter) { letters += 1; j -= 1 }
    letters >= 4
  }

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
    releaseYear:    Option[Int]     = None,
    country:        Option[String]  = None
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
          movie     = Movie(primary.title, primary.runtimeMinutes, primary.releaseYear, country = primary.country),
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

        val (synopsis, director, runtime, year, country) = Option(block.selectFirst("span.text")) match {
          case None => (None, None, None, None, None)
          case Some(span) =>
            val lines    = span.html().split("(?i)<br\\s*/?>").map(l => Jsoup.parseBodyFragment(l).body().text().trim)
            val dir      = lines.find(_.startsWith("Reż. ")).map(_.stripPrefix("Reż. ").trim)
            val emptyIdx = lines.indexWhere(_.isEmpty)
            val synLines = if (emptyIdx >= 0) lines.drop(emptyIdx + 1) else Array.empty[String]
            val synText  = synLines.filter(_.nonEmpty).mkString(" ").trim
            val fullText = lines.mkString(" ")
            val rt       = RuntimePat.findFirstMatchIn(fullText).flatMap(m => Try(m.group(1).toInt).toOption)
            val yr       = YearPat.findAllMatchIn(fullText).flatMap(m => Try(m.group(1).toInt).toOption).toSeq.headOption
            // The metadata line on Rialto looks like "Ukraina 2026, 90 minut" or
            // "Polska, Niemcy, 2026, 124 min" — everything before the first
            // 4-digit year is the production country (or comma-separated
            // countries). Strip the optional trailing comma left from the
            // "..Niemcy, 2026" form.
            val countryPat = """(?s)^(.+?)\s*,?\s+(?:19|20)\d{2}\b""".r
            val ctry = lines.find(l => YearPat.findFirstMatchIn(l).isDefined && !l.toLowerCase.startsWith("reż"))
              .flatMap(l => countryPat.findFirstMatchIn(l).map(_.group(1).trim))
              .map(_.stripSuffix(","))
              .map(_.trim)
              .filter(_.nonEmpty)
            (Option(synText).filter(_.nonEmpty), dir, rt, yr, ctry)
        }

        FilmEntry(title, eventUrl, posterUrl, synopsis, director, runtime, year, country)
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

