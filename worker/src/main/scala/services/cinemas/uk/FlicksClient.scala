package services.cinemas.uk

import tools.HttpFetch
import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper}

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Flicks (flicks.co.uk) — the UK's nationwide cinema-listings aggregator (chains
 * + independents), and the chosen UK source after Webedia retired its British
 * sibling Screenrush (`screenrush.co.uk` is gone from DNS, so
 * [[WebediaShowtimesClient]] can't reach the UK).
 *
 * Flicks renders a cinema's programme server-side but loads each day's sessions
 * on demand: the cinema page carries empty `<div data-date=…>` day tabs, and the
 * real showtimes come from an AJAX fragment (the request needs an
 * `is-ajax-call: yes` header):
 *
 *   GET https://www.flicks.co.uk/cinema/sessions/<slug>/<YYYY-MM-DD>/
 *     → an HTML fragment of `<article class="cinema-times__article">` per film:
 *        `h3.cinema-times__movie-title`, a `/movie/<slug>/` link, the runtime
 *        (`.cinema__movie-duration` "90 mins"), the director
 *        (`.cinema__director span`), and a `ul.times-calendar-times` of session
 *        buttons — each an `<a>` to the cinema chain's own booking deep-link,
 *        with the 24h time in `data-optlabel` (falling back to the visible
 *        "h:mm am/pm") and a premium/format label (IMAX, LuxeSuite, iSense…) on
 *        variant screenings.
 *
 * One instance serves one venue — its Flicks `cinemaSlug` + the [[Cinema]] it
 * feeds, mirroring [[FilmwebShowtimesClient]]. One AJAX call per day; the film's
 * numeric Flicks id (`content_id`) rides along as an `externalId`. TMDB enriches
 * synopsis/cast/year downstream.
 */
class FlicksClient(
  http:       HttpFetch,
  cinemaSlug: String,
  override val cinema: Cinema,
  today:      LocalDate = LocalDate.now(ZoneId.of("Europe/London"))
) extends ChunkedCinemaScraper {

  import FlicksClient._

  private val programmeUrl = s"$BaseUrl/cinema/$cinemaSlug/"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(programmeUrl)

  // Each populated day is one chunk, run as its own `ScrapeChunk` task (see
  // ChunkedCinemaScraper / ScrapeChunkHandler). The days spread across the task
  // queue and the shared Flicks pace gate instead of bursting from a single task
  // that parks a worker thread for that many back-to-back AJAX calls. The
  // in-process `fetch()` the trait composes (planChunks → fetchChunk →
  // reduceChunks) is used only by the deterministic fixture harness + unit tests.

  /** The days to scrape, read off the venue programme page's
   *  `<div class="timetable__day" data-date="YYYY-MM-DD">` day tabs — the exact
   *  days with sessions, gap days excluded (a date absent from the tab list
   *  returns an empty sessions fragment). The list is sparse and reaches months
   *  out (Flicks advertises a venue's whole booking horizon, not a fixed window),
   *  so reading it once gives the site's full advertised horizon WITHOUT firing a
   *  request per empty day: the page names precisely which days to fetch. Bounded
   *  to `[today, today+MaxHorizonDays]` so a stray attribute date can't balloon
   *  the per-venue chunk fan-out.
   *
   *  The programme page is the ONLY source of days — there is no fixed-grid
   *  fallback. When the page can't be fetched (its `http.get` throws, propagating
   *  here) or carries no in-horizon day tab, this throws: an index-page failure
   *  fails the whole scrape (recorded as a normal scrape outcome by the planner),
   *  which keeps the venue's last-known listing rather than silently narrowing it
   *  to a guessed 7-day window that would drop every advertised far-out day. */
  def planChunks(): Seq[String] = {
    val dates = parseProgrammeDates(http.get(programmeUrl))
      .filter(d => !d.isBefore(today) && !d.isAfter(today.plusDays(MaxHorizonDays.toLong)))
    if (dates.isEmpty)
      throw new IllegalStateException(
        s"Flicks programme page for '$cinemaSlug' listed no day tabs within the horizon")
    dates.map(_.toString)
  }

  /** Fetch + parse ONE day's sessions fragment into that day's films. The fetch
   *  THROWS on failure so ONLY that day's chunk task reschedules (the per-day
   *  retry); the other days are unaffected. A day that ANSWERS with an empty
   *  fragment (no programme) is a valid empty result, not a failure. */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] = {
    val date = LocalDate.parse(dateKey)
    moviesFor(parseDay(http.get(sessionsUrl(cinemaSlug, date), AjaxHeaders), date))
  }

  /** Merge every day's films into the venue's listing: one row per film (grouped
   *  by its stable `/movie/<slug>` `filmUrl`), showtimes unioned, deduped by
   *  (time, booking) and time-ordered — the same grouping the monolithic scrape
   *  used, so `reduceChunks ∘ fetchChunk ∘ planChunks` equals the old `fetch()`.
   *  Overrides the identity default only to keep the exact (time, booking) dedup
   *  key and the by-title final ordering. */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    chunks.toSeq.sortBy(_._1).flatMap(_._2)
      .groupBy(m => m.filmUrl.getOrElse(m.movie.title))
      .toSeq.sortBy(_._1)
      .flatMap { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes)
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None else Some(group.head.copy(showtimes = showtimes))
      }
      .sortBy(_.movie.title)

  /** Build one film row per stable `/movie/<slug>` from a day's session slots,
   *  showtimes deduped by (time, booking) and time-ordered. */
  private def moviesFor(slots: Seq[RawFlicksSlot]): Seq[CinemaMovie] =
    slots.groupBy(_.slug).toSeq.flatMap { case (_, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking, None, s.format))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie       = Movie(title = head.title, runtimeMinutes = head.runtimeMinutes),
          cinema      = cinema,
          posterUrl   = head.posterUrl,
          filmUrl     = Some(s"$BaseUrl/movie/${head.slug}/"),
          synopsis    = None,
          cast        = Seq.empty,
          director    = head.director.toSeq,
          showtimes   = showtimes,
          externalIds = head.contentId.map("flicks" -> _).toMap
        ))
      }
    }
}

object FlicksClient {

  val BaseUrl = "https://www.flicks.co.uk"

  /** A safety ceiling on the discovered horizon, not a target: Flicks advertises
   *  a venue's whole booking horizon (observed out to ~5 months), and we fetch
   *  every advertised day, but bound it so a stray far-future date in the tab
   *  list can't balloon a venue's chunk fan-out. ~7 months clears the real
   *  horizon with headroom. */
  val MaxHorizonDays = 210

  private val DataDate = """data-date="(\d{4}-\d{2}-\d{2})"""".r

  /** The days a venue has a programme on, read off the programme page's
   *  `<div class="timetable__day" data-date="YYYY-MM-DD">` day tabs — a sparse,
   *  months-long list with gap days omitted (a date absent from it returns an
   *  empty sessions fragment). Every `data-date` on the page is a day tab, so
   *  pull the ISO dates straight out; deduped and sorted. Empty when no tab is
   *  present (older / changed markup) → the caller falls back to the fixed grid.
   *  Pure + public so a spec feeds it a recorded page directly. */
  def parseProgrammeDates(html: String): Seq[LocalDate] =
    DataDate.findAllMatchIn(html).map(_.group(1)).toSeq
      .flatMap(s => Try(LocalDate.parse(s)).toOption)
      .distinct.sortBy(_.toString)

  // Flicks 403s a non-browser fetch and only serves the sessions fragment (rather
  // than the full page) when this header is set; RealHttpFetch already sends a
  // browser User-Agent, so this is the one extra header the endpoint needs.
  private val AjaxHeaders = Map("is-ajax-call" -> "yes")

  def sessionsUrl(cinemaSlug: String, date: LocalDate): String =
    s"$BaseUrl/cinema/sessions/$cinemaSlug/$date/"

  private val SlugPat    = """/movie/([^/?#]+)""".r
  private val DigitsPat  = """(\d+)""".r
  private val OptTimePat = """(\d{1,2}):(\d{2}):\d{2}""".r
  private val AmPmPat    = """(?i)(\d{1,2}):(\d{2})\s*(am|pm)""".r
  private val ContentId  = """"content_id"\s*:\s*"(\d+)"""".r

  /** One session slot off the fragment: the film's slug (stable id) + title +
   *  metadata (constant across a film's sessions) and the single screening. */
  case class RawFlicksSlot(
    slug:           String,
    title:          String,
    runtimeMinutes: Option[Int],
    posterUrl:      Option[String],
    director:       Option[String],
    contentId:      Option[String],
    dateTime:       LocalDateTime,
    booking:        Option[String],
    format:         List[String]
  )

  /** Parse one day's sessions fragment for the given calendar date. Pure +
   *  public so the spec feeds it the recorded HTML directly. */
  def parseDay(html: String, date: LocalDate): Seq[RawFlicksSlot] = {
    val doc = Jsoup.parse(html, BaseUrl)
    doc.select("article.cinema-times__article").asScala.toSeq.flatMap { article =>
      val slug  = firstMovieSlug(article)
      val title = Option(article.selectFirst("h3.cinema-times__movie-title")).map(_.text.trim).filter(_.nonEmpty)
      (slug, title) match {
        case (Some(sl), Some(t)) =>
          val runtime   = Option(article.selectFirst(".cinema__movie-duration"))
            .map(_.text).flatMap(s => DigitsPat.findFirstIn(s)).map(_.toInt).filter(_ > 0)
          val poster    = Option(article.selectFirst(".cinema-times__image img")).map(_.attr("src")).filter(_.nonEmpty)
          val director  = Option(article.selectFirst(".cinema__director span")).map(_.text.trim).filter(_.nonEmpty)
          val contentId = article.select("a.times-calendar-times__button").asScala.iterator
            .flatMap(b => ContentId.findFirstMatchIn(b.attr("data-eventjson")).map(_.group(1))).nextOption()

          article.select("li.times-calendar-times__el").asScala.toSeq.flatMap { li =>
            val button = Option(li.selectFirst("a.times-calendar-times__button"))
            button.flatMap(parseTime).map { time =>
              val booking = button.map(_.attr("href")).filter(_.nonEmpty)
              val label   = Option(li.selectFirst("span.times-calendar-times__el__label span"))
                .map(_.text.trim).filter(_.nonEmpty)
              RawFlicksSlot(sl, t, runtime, poster, director, contentId,
                LocalDateTime.of(date, time), booking, label.toList)
            }
          }
        case _ => Seq.empty
      }
    }
  }

  private def firstMovieSlug(article: Element): Option[String] =
    article.select("""a[href*="/movie/"]""").asScala.iterator
      .flatMap(a => SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1))).nextOption()

  /** A session's start time: the 24h time trailing `data-optlabel`
   *  ("Odeon Cinema Norwich-10:10:00"), which some sessions omit — then the
   *  visible "10:10 am" text. */
  private def parseTime(button: Element): Option[LocalTime] = {
    val fromOptLabel = OptTimePat.findAllMatchIn(button.attr("data-optlabel")).toSeq.lastOption
      .flatMap(m => Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)
    fromOptLabel.orElse {
      val text = Option(button.selectFirst("span.times-calendar-times__el__time")).map(_.text).getOrElse("")
      parseAmPm(text)
    }
  }

  /** "10:10 am" / "7:05 pm" → `LocalTime`. */
  private def parseAmPm(text: String): Option[LocalTime] =
    AmPmPat.findFirstMatchIn(text).flatMap { m =>
      val minute = m.group(2).toInt
      val pm     = m.group(3).equalsIgnoreCase("pm")
      val hour12 = m.group(1).toInt % 12
      val hour   = if (pm) hour12 + 12 else hour12
      Try(LocalTime.of(hour, minute)).toOption
    }
}
