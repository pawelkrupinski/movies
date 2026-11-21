package services.cinemas.pl

import java.time.{LocalDate, Period}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.ScraperParse
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail}
import tools.{HttpFetch, HttpRead}

class KinoBulgarskaClient(http: HttpFetch, today: => LocalDate) extends CinemaScraper with DetailEnricher {

  val cinema: Cinema = KinoBulgarska
  private val PageUrl = "http://kinobulgarska19.pl/repertuar"

  // Trailer embed — per-film detail pages render the YouTube trailer in a
  // `<iframe class="..." src="https://www.youtube.com/embed/ID?...">` block
  // (a WordPress oEmbed wrapper). Anchor on the `youtube.com/embed/` or
  // `youtu.be/` host to skip unrelated iframes (analytics, share widgets).
  private val TrailerIframePat =
    """<iframe[^>]+src="(https?://(?:www\.)?(?:youtube\.com|youtu\.be|player\.vimeo\.com|vimeo\.com)/[^"]+)"""".r

  private def normalizeTitle(raw: String): String = KinoBulgarskaClient.normalizeTitle(raw)

  // Cinema dates in the page have no year. Roll forward to next year only when
  // the parsed date is *substantially* in the past — the legitimate case is the
  // year boundary (late December showing "13.01" means January of next year).
  // A website viewed in mid-May still shows "13.05" / "14.05" entries from a
  // day or two ago, and those are this year: six months' grace keeps them put
  // while bumping January-viewed-from-late-June forward.
  private def parsePolishDate(text: String): Option[LocalDate] =
    ScraperParse.parseDayMonth(text).flatMap(ScraperParse.upcomingDate(_, today, grace = Period.ofMonths(6)))


  // movie-meta is formatted "reż. <DIRECTORS…>, <COUNTRIES…>, YEAR r., DURATION min."
  // First comma-separated chunk is the director, all chunks between director
  // and the year are production countries.
  private def extractMeta(section: Element): (Seq[String], Seq[String], Option[Int], Option[Int]) =
    Option(section.selectFirst("p.movie-meta")).map(_.text()).filter(_.startsWith("reż. "))
      .fold((Seq.empty[String], Seq.empty[String], Option.empty[Int], Option.empty[Int]))(metaLine)

  /** A `reż. …` line's (directors, countries, year, runtime). */
  private def metaLine(text: String): (Seq[String], Seq[String], Option[Int], Option[Int]) = {
    val parts     = text.stripPrefix("reż. ").split(",\\s*").map(_.trim)
    val yearIndex = parts.indexWhere(_.matches("\\d{4} r\\."))
    val director  = if (yearIndex >= 1) ScraperParse.commaItems(parts(0)) else Seq.empty
    val countries = if (yearIndex > 1) parts.slice(1, yearIndex).filter(_.nonEmpty).toSeq else Seq.empty
    val year      = if (yearIndex >= 0) Try(parts(yearIndex).replaceAll("[^0-9]", "").toInt).toOption else None
    val runtime   = ScraperParse.runtimeMinutesIn(text)
    (director, countries, year, runtime)
  }

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(PageUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(HttpRead.page(http, PageUrl))

  override val detailGroup: String = "kino-bulgarska"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl. None on a fetch failure so the task stays stale and is
   *  retried by the next scrape rather than recording an empty result as fresh.
   *
   *  A page that LOADED but embeds no trailer is a detail, not a failure: plenty
   *  of repertory titles simply have no trailer block, and folding that into None
   *  left the film unstamped, so `DetailReaper` re-enqueued it every tick forever
   *  (Absolwent alone: 1438 enrichment failures in 24h, /uptime row permanently
   *  red). An empty [[FilmDetail]] merges as a no-op and stamps the film fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(html => FilmDetail(trailerUrl = parseTrailer(html)))

  /** Trailer URL parsed from a Bulgarska film page. Returns the canonical
   *  `youtube.com/watch?v=ID` form when the iframe holds a YouTube video;
   *  vimeo URLs are passed through unchanged for the view layer's
   *  TrailerEmbed to reshape. Public for unit tests. */
  def parseTrailer(html: String): Option[String] =
    TrailerIframePat.findFirstMatchIn(html).map(_.group(1)).flatMap(ScraperParse.canonicalTrailer)

  private type FilmFacts = (String, Option[String], Option[String], Seq[String], Seq[String], Option[Int], Option[Int])

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val filmByUrl      = collection.mutable.Map[String, FilmFacts]()
    val showtimesByUrl = collection.mutable.Map[String, collection.mutable.ListBuffer[Showtime]]()

    for {
      article <- Jsoup.parse(html).select("article").asScala
      date    <- Option(article.selectFirst("h3")).flatMap(h3 => parsePolishDate(h3.text())).toSeq
      section <- article.select("section.clearfix").asScala
      link    <- Option(section.selectFirst("h4 a[href]"))
    } {
      val filmUrl = link.attr("href")
      if (!filmByUrl.contains(filmUrl)) filmByUrl(filmUrl) = filmFacts(section, normalizeTitle(link.text()))
      showtimeOf(section, date).foreach(showtimesByUrl.getOrElseUpdate(filmUrl, collection.mutable.ListBuffer()) += _)
    }

    filmByUrl.toSeq.flatMap { case (filmUrl, (title, posterUrl, synopsis, director, countries, year, runtime)) =>
      showtimesByUrl.get(filmUrl).map { slots =>
        CinemaMovie(
          movie     = Movie(title, runtimeMinutes = runtime, releaseYear = year, countries = countries),
          cinema    = KinoBulgarska,
          posterUrl = posterUrl,
          filmUrl   = Some(filmUrl),
          synopsis  = synopsis,
          director  = director,
          showtimes = slots.toSeq.sortBy(_.dateTime)
        )
      }
    }
  }

  private def filmFacts(section: Element, title: String): FilmFacts = {
    // the listing shows WordPress's 120x120 `attachment-thumbnail` copy of the poster
    val posterUrl = Option(section.selectFirst("img[src]")).map(img => ResizedPoster.fullSize(img.attr("src")))
    val synopsis                             = Option(section.selectFirst("p:not(.movie-meta)")).map(_.text()).filter(_.nonEmpty)
    val (director, countries, year, runtime) = extractMeta(section)
    (title, posterUrl, synopsis, director, countries, year, runtime)
  }

  private def showtimeOf(section: Element, date: LocalDate): Option[Showtime] = {
    val room = Option(section.selectFirst(".show-type-badge a")).map(_.text().trim).filter(_.nonEmpty)
    Option(section.selectFirst(".start-info.clock"))
      .flatMap(clock => ScraperParse.parseHHmm(clock.text().replaceAll("\\s+", "")))
      .map(time => Showtime(date.atTime(time), None, room))
  }
}

object KinoBulgarskaClient {
  // Cinema decoration suffixes the page tacks onto raw titles. Stripped in
  // this fixed order so chained suffixes peel off inside-out — "Drzewo magii
  // – pokazy przedpremierowe – kino dzieci" needs "– kino dzieci" gone first
  // so the next pattern's `$` anchor can match the residual "– pokazy
  // przedpremierowe" tail. Case-insensitive — the page's raw titles are
  // mixed-case ("DRZEWO MAGII – Pokazy przedpremierowe – Kino dzieci");
  // casing is applied centrally afterwards (`TitleNormalizer.recase`).
  private val SuffixPats: Seq[scala.util.matching.Regex] = Seq(
    """(?i)\s+–\s+kino dzieci$""".r,
    """(?i)\s+–\s+pokazy przedpremierowe$""".r,
    """(?i)\s+–\s+pokaz przedpremierowy$""".r,
    """(?i)\s+–\s+poznańska premiera$""".r
  )

  def normalizeTitle(raw: String): String =
    SuffixPats.foldLeft(raw)((acc, p) => p.replaceFirstIn(acc, ""))
}
