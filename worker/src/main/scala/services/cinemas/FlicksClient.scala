package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.concurrent.duration._
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
  daysAhead:  Int       = 6,
  today:      LocalDate = LocalDate.now(ZoneId.of("Europe/London"))
) extends CinemaScraper {

  import FlicksClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/cinema/$cinemaSlug/")

  def fetch(): Seq[CinemaMovie] = {
    val dates = (0 to daysAhead).map(today.plusDays(_))

    // One sessions fragment per day, in parallel + tolerantly: a day whose
    // request fails or won't parse contributes nothing rather than failing the
    // whole venue (Flicks serves an empty fragment for days with no programme).
    val byDate = ParallelDetailFetch.keyed(
      "flicks-sessions", dates, 1.minute, maxConcurrent = 2
    )(d => sessionsUrl(cinemaSlug, d)) { url =>
      Try(http.get(url, AjaxHeaders)).toOption.toSeq.flatMap(html => parseDayForUrl(html, url))
    }
    val slots = dates.flatMap(d => byDate.getOrElse(d, Seq.empty))

    // One row per film (grouped by its stable `/movie/<slug>`), showtimes across
    // all days merged, deduped by (time, booking) and time-ordered.
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
    }.sortBy(_.movie.title)
  }

  private def parseDayForUrl(html: String, url: String): Seq[RawFlicksSlot] =
    dateOf(url).map(d => Try(parseDay(html, d)).getOrElse(Seq.empty)).getOrElse(Seq.empty)
}

object FlicksClient {

  val BaseUrl = "https://www.flicks.co.uk"

  // Flicks 403s a non-browser fetch and only serves the sessions fragment (rather
  // than the full page) when this header is set; RealHttpFetch already sends a
  // browser User-Agent, so this is the one extra header the endpoint needs.
  private val AjaxHeaders = Map("is-ajax-call" -> "yes")

  def sessionsUrl(cinemaSlug: String, date: LocalDate): String =
    s"$BaseUrl/cinema/sessions/$cinemaSlug/$date/"

  private val SlugPat    = """/movie/([^/?#]+)""".r
  private val DigitsPat  = """(\d+)""".r
  private val DatePat    = """(\d{4}-\d{2}-\d{2})""".r
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

  private def dateOf(url: String): Option[LocalDate] =
    DatePat.findFirstIn(url).flatMap(s => Try(LocalDate.parse(s)).toOption)
}
