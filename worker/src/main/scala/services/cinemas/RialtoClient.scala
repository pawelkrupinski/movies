package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

class RialtoClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = Rialto
  private val RepertoireUrl = "https://www.kinorialto.poznan.pl/repertuar/"
  private val BaseUrl       = "https://www.kinorialto.poznan.pl"


  private val DateTimePat = """- (\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}) -""".r
  private val RuntimePat  = """(\d+)\s*min""".r
  private val YearPat     = """\b((?:19|20)\d{2})\b""".r
  // The event page (not the repertoire listing) heads the description with a
  // pipe-delimited parameters line whose first segment is the genre list:
  //   `<p class="movie-parameters">Dramat, Komedia | 120 min</p>`
  //   `<p class="movie-parameters">Przygodowy Horror Sci-Fi | Od lat 12 | 99 min</p>`
  //   `<p class="movie-parameters">Fantastyczny / romantyczny | 90 min</p>`
  //   `<p class="movie-parameters">Animowany | 55 min</p>`
  // Genres are delimited inconsistently across pages — by comma, by a bare
  // space, or by " / " — so split the first segment on any run of those. The
  // remaining segments are an optional age rating and the runtime. Capture the
  // first segment; the `|` is required so a duration-only line ("55 min") can't
  // masquerade as a genre.
  private val GenrePat    = """movie-parameters[^>]*>\s*([^<|]+?)\s*\|""".r
  // Genre separators: a run of commas, slashes, and/or whitespace. Hyphens stay
  // intact so "Sci-Fi" survives as one genre.
  private val GenreSepPat = """[,/\s]+""".r
  // A first segment that is itself an age rating ("Od lat 12", "b.o.") or a
  // runtime means the film simply has no genre — don't emit it as one.
  private val NonGenreSegmentPat = """(?i)^(?:od lat \d+|b\.o\.|\d+\s*min)$""".r

  private val NonFilmTitlePatterns = Seq("bilet podarunkowy", "karta podarunkowa", "voucher")

  private def isNonFilmEntry(title: String): Boolean =
    NonFilmTitlePatterns.exists(title.toLowerCase.contains)

  private case class FilmEntry(
    title:          String,
    eventUrl:       String,
    posterUrl:      Option[String],
    synopsis:       Option[String],
    director:       Seq[String],
    runtimeMinutes: Option[Int]     = None,
    releaseYear:    Option[Int]     = None,
    countries:      Seq[String]     = Seq.empty
  )

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl, BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val filmEntries = parseRepertoire(http.get(RepertoireUrl))

    val eventDataByUrl: Map[String, Option[EventData]] =
      ParallelDetailFetch("rialto-events", filmEntries.map(_.eventUrl).distinct, 1.minute) { url =>
        Try(http.get(url)).toOption.map(html => EventData(parseEventPage(html), parseGenres(html)))
      }

    filmEntries
      .groupBy(_.title.toUpperCase)
      .values
      .flatMap { group =>
        val primary      = group.head
        val eventData    = group.flatMap(e => eventDataByUrl.get(e.eventUrl).flatten)
        val allShowtimes = eventData.flatMap(_.showtimes).sortBy(_.dateTime)
        val genres       = eventData.flatMap(_.genres).distinct
        if (allShowtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(primary.title, primary.runtimeMinutes, primary.releaseYear, countries = primary.countries, genres = genres),
          cinema    = Rialto,
          posterUrl = primary.posterUrl,
          filmUrl   = Some(primary.eventUrl),
          synopsis  = primary.synopsis,
          cast      = Seq.empty,
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
        val title    = RialtoClient.normalizeTitle(rawTitle)

        // Restrict to the actual movie image container — otherwise the first block
        // (which still includes the page header) picks up the Facebook-login icon.
        val posterUrl = Option(block.selectFirst(".list-item-image img[src], .image img[src]"))
                          .map(_.attr("src"))

        val (synopsis, director, runtime, year, countries) = Option(block.selectFirst("span.text")) match {
          case None => (None, Seq.empty[String], None, None, Seq.empty[String])
          case Some(span) =>
            val lines    = span.html().split("(?i)<br\\s*/?>").map(l => Jsoup.parseBodyFragment(l).body().text().trim)
            val dir      = lines.find(_.startsWith("Reż. ")).map(_.stripPrefix("Reż. ").trim).filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
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
            val cs = lines.find(l => YearPat.findFirstMatchIn(l).isDefined && !l.toLowerCase.startsWith("reż"))
              .flatMap(l => countryPat.findFirstMatchIn(l).map(_.group(1).trim))
              .map(_.stripSuffix(",").trim)
              .filter(_.nonEmpty)
              .toSeq
              .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
            (Option(synText).filter(_.nonEmpty), dir, rt, yr, cs)
        }

        FilmEntry(title, eventUrl, posterUrl, synopsis, director, runtime, year, countries)
      }.filterNot(e => isNonFilmEntry(e.title))
    }

  private case class EventData(showtimes: Seq[Showtime], genres: Seq[String])

  /** Genres from the event page's `movie-parameters` line, when present.
   *  Split on the page's inconsistent comma / space / slash delimiters and
   *  title-cased to match the genres TMDB / Filmweb and the other cinemas
   *  contribute. Empty for pages without the marker or whose first segment is
   *  an age rating / runtime rather than a genre. */
  def parseGenres(html: String): Seq[String] =
    GenrePat.findFirstMatchIn(html).map(_.group(1).trim).filterNot(NonGenreSegmentPat.matches).map { segment =>
      GenreSepPat.split(segment).map(_.trim).filter(_.nonEmpty)
        .map(tools.TextNormalization.titleCaseIfAllLower).toSeq
    }.getOrElse(Seq.empty)

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

  /** Full title cleanup: drop the cycle prefix, strip event-decoration
   *  suffixes, then sentence-case. When a recognised programme prefix survives
   *  (e.g. "Filmowy Klub Seniora:"), case the prefix and the film title
   *  independently so the film title keeps its own leading capital
   *  ("Filmowy klub seniora: Ojczyzna", not "… ojczyzna"). Public so it can be
   *  unit-tested directly. */
  def normalizeTitle(raw: String): String = {
    val cleaned = stripPreviewSuffix(stripCyclePrefix(raw))
    services.movies.TitleNormalizer.programmePrefix(cleaned) match {
      case Some(prefix) => normalizeCase(prefix) + normalizeCase(cleaned.substring(prefix.length))
      case None         => normalizeCase(cleaned)
    }
  }

  // Recurring-programme prefixes whose screenings are their OWN row, not folded
  // into the base film — a senior-club showing targets a distinct audience and
  // is listed separately. Matched on the registered programme set so the row
  // still enriches off the clean base title (see TitleNormalizer.ProgrammePrefix).
  private def stripCyclePrefix(title: String): String = {
    val colonIdx = title.indexOf(": ")
    if (colonIdx > 0 && colonIdx < 30) {
      val prefix = title.substring(0, colonIdx)
      if (prefix.equalsIgnoreCase("Filmowy Klub Seniora")) title
      else if (prefix != prefix.toUpperCase) title.substring(colonIdx + 2) else title
    } else title
  }

  // Promo decoration the page tacks onto a film's title for preview screenings
  // ("Ojczyzna - pokaz przedpremierowy"). Strip it so the entry merges with the
  // same film's regular run here and at the other cinemas. Case-insensitive —
  // raw titles arrive upper-cased — and tolerant of either dash the page uses.
  private val PreviewSuffixPat = """(?i)\s*[-–]\s*pokaz przedpremierowy\s*$""".r

  private def stripPreviewSuffix(title: String): String =
    PreviewSuffixPat.replaceFirstIn(title, "")

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
}

