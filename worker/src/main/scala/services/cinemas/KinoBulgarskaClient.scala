package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

class KinoBulgarskaClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper with DetailEnricher {

  val cinema: Cinema = KinoBulgarska
  private val PageUrl = "http://kinobulgarska19.pl/repertuar"

  // Trailer embed — per-film detail pages render the YouTube trailer in a
  // `<iframe class="..." src="https://www.youtube.com/embed/ID?...">` block
  // (a WordPress oEmbed wrapper). Anchor on the `youtube.com/embed/` or
  // `youtu.be/` host to skip unrelated iframes (analytics, share widgets).
  private val TrailerIframePat =
    """<iframe[^>]+src="(https?://(?:www\.)?(?:youtube\.com|youtu\.be|player\.vimeo\.com|vimeo\.com)/[^"]+)"""".r

  private val DatePat = """(\d+)\s+(\w+)""".r

  private def normalizeTitle(raw: String): String = KinoBulgarskaClient.normalizeTitle(raw)

  private def parsePolishDate(text: String): Option[LocalDate] =
    DatePat.findFirstMatchIn(text).flatMap { m =>
      ScraperParse.PolishMonths.get(m.group(2)).map { month =>
        val day       = m.group(1).toInt
        val candidate = LocalDate.of(today.getYear, month, day)
        // Cinema dates in the page have no year. Roll forward to next year
        // only when the parsed date is *substantially* in the past —
        // legitimate use case is the year-boundary (e.g. late December
        // showing "13.01" — which means Jan 13 of next year). Don't roll
        // recent past screenings forward: a website viewed in mid-May still
        // shows "13.05" / "14.05" entries from a day or two ago, and those
        // are this year, not next year. 6 months is the sweet spot:
        // distant-past dates (like January viewed from late June) bump
        // forward; nearby-past dates (yesterday, last week) stay put.
        if (candidate.isBefore(today.minusMonths(6))) candidate.plusYears(1) else candidate
      }
    }

  private val RuntimePat = """(\d+)\s*min""".r

  // movie-meta is formatted "reż. <DIRECTORS…>, <COUNTRIES…>, YEAR r., DURATION min."
  // First comma-separated chunk is the director, all chunks between director
  // and the year are production countries.
  private def extractMeta(section: Element): (Seq[String], Seq[String], Option[Int], Option[Int]) =
    Option(section.selectFirst("p.movie-meta")).map { meta =>
      val text = meta.text()
      if (!text.startsWith("reż. ")) (Seq.empty, Seq.empty, None, None)
      else {
        val afterRez  = text.stripPrefix("reż. ")
        val parts     = afterRez.split(",\\s*").map(_.trim)
        val yearIdx   = parts.indexWhere(_.matches("\\d{4} r\\."))
        val director  = if (yearIdx >= 1) parts(0).split(",").map(_.trim).filter(_.nonEmpty).toSeq else Seq.empty
        val countries = if (yearIdx > 1) parts.slice(1, yearIdx).filter(_.nonEmpty).toSeq else Seq.empty
        val year      = if (yearIdx >= 0) Try(parts(yearIdx).replaceAll("[^0-9]", "").toInt).toOption else None
        val runtime   = RuntimePat.findFirstMatchIn(text).flatMap(m => Try(m.group(1).toInt).toOption)
        (director, countries, year, runtime)
      }
    }.getOrElse((Seq.empty, Seq.empty, None, None))

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PageUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  override val detailGroup: String = "kino-bulgarska"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl. None on a fetch failure so the task stays stale and is
   *  retried by the next scrape rather than recording an empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(http.get(ref)).toOption.flatMap(parseTrailer).map(url => FilmDetail(trailerUrl = Some(url)))

  /** Trailer URL parsed from a Bulgarska film page. Returns the canonical
   *  `youtube.com/watch?v=ID` form when the iframe holds a YouTube video;
   *  vimeo URLs are passed through unchanged for the view layer's
   *  TrailerEmbed to reshape. Public for unit tests. */
  def parseTrailer(html: String): Option[String] =
    TrailerIframePat.findFirstMatchIn(html).map(_.group(1)).flatMap(ScraperParse.canonicalTrailer)

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc            = Jsoup.parse(html)
    val filmByUrl      = collection.mutable.Map[String, (String, Option[String], Option[String], Seq[String], Seq[String], Option[Int], Option[Int])]()
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
            val (director, countries, year, runtime) = extractMeta(section)
            filmByUrl(filmUrl) = (title, posterUrl, synopsis, director, countries, year, runtime)
          }

          timeOpt.foreach { dateTime =>
            showtimesByUrl.getOrElseUpdate(filmUrl, collection.mutable.ListBuffer()) += Showtime(dateTime, None, room)
          }
        }
      }
    }

    filmByUrl.toSeq.flatMap { case (filmUrl, (title, posterUrl, synopsis, director, countries, year, runtime)) =>
      showtimesByUrl.get(filmUrl).map { slots =>
        CinemaMovie(
          movie     = Movie(title, runtimeMinutes = runtime, releaseYear = year, countries = countries),
          cinema    = KinoBulgarska,
          posterUrl = posterUrl,
          filmUrl   = Some(filmUrl),
          synopsis  = synopsis,
          cast      = Seq.empty,
          director  = director,
          showtimes = slots.toSeq.sortBy(_.dateTime)
        )
      }
    }
  }
}

object KinoBulgarskaClient {
  // Cinema decoration suffixes the page tacks onto raw titles. Stripped in
  // this fixed order so chained suffixes peel off inside-out — "Drzewo magii
  // – pokazy przedpremierowe – kino dzieci" needs "– kino dzieci" gone first
  // so the next pattern's `$` anchor can match the residual "– pokazy
  // przedpremierowe" tail. Case-insensitive — the page's raw titles are
  // mixed-case ("DRZEWO MAGII – Pokazy przedpremierowe – Kino dzieci"),
  // sentence-casing happens after.
  private val SuffixPats: Seq[scala.util.matching.Regex] = Seq(
    """(?i)\s+–\s+kino dzieci$""".r,
    """(?i)\s+–\s+pokazy przedpremierowe$""".r,
    """(?i)\s+–\s+pokaz przedpremierowy$""".r,
    """(?i)\s+–\s+poznańska premiera$""".r
  )

  def normalizeTitle(raw: String): String = {
    val stripped = SuffixPats.foldLeft(raw)((acc, p) => p.replaceFirstIn(acc, ""))
    if (stripped.isEmpty) stripped else s"${stripped.head.toUpper}${stripped.tail.toLowerCase}"
  }
}

