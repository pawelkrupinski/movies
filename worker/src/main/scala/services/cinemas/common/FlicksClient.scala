package services.cinemas.common

import tools.HttpFetch
import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Flicks — a nationwide cinema-listings aggregator (chains + independents) that
 * runs the SAME site on several ccTLDs. One instance serves one venue of one
 * [[FlicksMarket]]: `flicks.co.uk` for the UK — the chosen source there after
 * Webedia retired its British sibling Screenrush (`screenrush.co.uk` is gone
 * from DNS, so [[services.cinemas.de.WebediaShowtimesClient]] can't reach the
 * UK) — and `flicks.us` for the United States, which is why this client lives in
 * `services.cinemas.common` rather than a per-country package.
 *
 * Flicks renders a cinema's programme server-side but loads each day's sessions
 * on demand: the cinema page carries empty `<div data-date=…>` day tabs, and the
 * real showtimes come from an AJAX fragment (the request needs an
 * `is-ajax-call: yes` header):
 *
 *   GET <market base>/cinema/sessions/<slug>/<YYYY-MM-DD>/
 *     → an HTML fragment of `<article class="cinema-times__article">` per film:
 *        `h3.cinema-times__movie-title`, a `/movie/<slug>/` link, the runtime
 *        (`.cinema__movie-duration` "90 mins"), the director
 *        (`.cinema__director span`), and a `ul.times-calendar-times` of session
 *        buttons — carrying the 24h time in `data-optlabel` (falling back to the
 *        visible "h:mm am/pm") and a premium/format label (IMAX, LuxeSuite,
 *        iSense…) on variant screenings. A button is an `<a>` to the cinema
 *        chain's booking deep-link only where the venue has online booking wired
 *        in; venues without it render the identical button as a `<span>`, so the
 *        session is matched by CLASS, not tag, and its booking link is simply
 *        absent.
 *
 * One instance serves one venue — its Flicks `cinemaSlug` + the [[Cinema]] it
 * feeds, mirroring [[FilmwebShowtimesClient]]. One AJAX call per day. Each
 * session button carries a `data-eventjson` blob from which we lift the film's
 * numeric Flicks id (`content_id`, an `externalId`), its `content_cast`
 * (comma-separated) and `content_genre` (comma-separated); the film card also
 * carries a `/trailer/` link. TMDB still enriches synopsis/year downstream.
 */
class FlicksClient(
  http:       HttpFetch,
  cinemaSlug: String,
  override val cinema: Cinema,
  market:     FlicksMarket,
  today:      Option[LocalDate] = None
) extends ChunkedCinemaScraper {

  import FlicksClient._

  private val baseUrl = market.baseUrl
  // An absent `today` means the MARKET's current calendar day, not the JVM's: a
  // worker in Europe planning US venues must not start from a date those venues
  // have not reached. Resolved in the body rather than as a default argument
  // because a Scala default cannot read an earlier parameter of the same list.
  private val referenceDay: LocalDate = today.getOrElse(LocalDate.now(market.zoneId))

  private val programmeUrl = s"$baseUrl/cinema/$cinemaSlug/"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
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
   *  here) this throws: an index-page failure fails the whole scrape (recorded as
   *  a normal scrape outcome by the planner), which keeps the venue's last-known
   *  listing rather than silently narrowing it to a guessed 7-day window that
   *  would drop every advertised far-out day.
   *
   *  A page that fetched fine but names no day is split in two, the same line
   *  [[WebediaShowtimesClient]] draws for Germany:
   *   - NO timetable block at all → we aren't parsing the page we think we are
   *     (markup drift, an error page, a redirect). A FAILURE, so it throws.
   *   - timetable block present, no day tabs in it → the venue simply has
   *     nothing on. EXPECTED DATA, so it returns empty: the planner records that
   *     as a successful scrape of an empty repertoire, and
   *     `MovieCache.recordCinemaScrape` bails on an empty result so the venue
   *     keeps its last-known listing regardless. Throwing here instead left five
   *     UK venues permanently red on /uptime (2026-07-26). */
  def planChunks(): Seq[String] = {
    val html  = http.get(programmeUrl)
    val dates = parseProgrammeDates(html)
      .filter(d => !d.isBefore(referenceDay) && !d.isAfter(referenceDay.plusDays(MaxHorizonDays.toLong)))
    if (dates.isEmpty && !hasTimetable(html))
      throw new IllegalStateException(
        s"Flicks programme page for '$cinemaSlug' carried no timetable block")
    dates.map(_.toString)
  }

  /** Fetch + parse ONE day's sessions fragment into that day's films. The fetch
   *  THROWS on failure so ONLY that day's chunk task reschedules (the per-day
   *  retry); the other days are unaffected. A day that ANSWERS with an empty
   *  fragment (no programme) is a valid empty result, not a failure. */
  def fetchChunk(dateKey: String): Seq[CinemaMovie] = {
    val date = LocalDate.parse(dateKey)
    moviesFor(parseDay(http.get(sessionsUrl(market, cinemaSlug, date), AjaxHeaders), date, market))
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
          movie       = Movie(title = head.title, runtimeMinutes = head.runtimeMinutes, genres = head.genres),
          cinema      = cinema,
          posterUrl   = head.posterUrl,
          filmUrl     = Some(s"$baseUrl/movie/${head.slug}/"),
          synopsis    = None,
          cast        = head.cast,
          director    = head.director.toSeq,
          showtimes   = showtimes,
          externalIds = head.contentId.map("flicks" -> _).toMap,
          trailerUrl  = head.trailerUrl,
          ageRating   = head.ageRating
        ))
      }
    }
}

object FlicksClient {

  /** The shared scrape horizon — see [[services.cinemas.common.ScrapeHorizon]]. Flicks
   *  advertises a venue's whole booking horizon as day tabs and we fetch every advertised
   *  day; this only bounds a stray far-future tab. Deliberately the SAME number the chain
   *  clients use, so a venue's primary and its Flicks fallback cover one window and
   *  neither prunes the other's tail. */
  val MaxHorizonDays = ScrapeHorizon.MaxDays

  private val DataDate = """data-date="(\d{4}-\d{2}-\d{2})"""".r
  // The venue programme page's day-tab container. Rendered on EVERY venue page,
  // including one with nothing on (verified 2026-07-27 across venues both with
  // and without listings), so its absence — not the absence of day tabs — is what
  // marks a page we failed to parse. See `planChunks`.
  private val TimetableBlock = """class="timetable timetable--cinema"""".r

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

  /** Whether the page carries the venue day-tab container — i.e. it really is a
   *  Flicks programme page, whether or not the venue has anything on. */
  def hasTimetable(html: String): Boolean = TimetableBlock.findFirstIn(html).isDefined

  // Flicks 403s a non-browser fetch and only serves the sessions fragment (rather
  // than the full page) when this header is set; RealHttpFetch already sends a
  // browser User-Agent, so this is the one extra header the endpoint needs.
  private val AjaxHeaders = Map("is-ajax-call" -> "yes")

  def sessionsUrl(market: FlicksMarket, cinemaSlug: String, date: LocalDate): String =
    s"${market.baseUrl}/cinema/sessions/$cinemaSlug/$date/"

  private val SlugPat    = """/movie/([^/?#]+)""".r
  private val DigitsPat  = """(\d+)""".r
  private val OptTimePat = """(\d{1,2}):(\d{2}):\d{2}""".r
  private val AmPmPat    = """(?i)(\d{1,2}):(\d{2})\s*(am|pm)""".r
  // Keys lifted from a session button's `data-eventjson` blob (jsoup returns it
  // entity-decoded, so we match against real quotes). `content_cast` and
  // `content_genre` are comma-separated lists; `content_awards` has no model
  // home and is ignored. The age rating (`content_rating` in the blob) is read
  // instead off the film card's `.cinema__movie-classification` element, where
  // it renders cleanly as the BBFC label.
  private val ContentId    = """"content_id"\s*:\s*"(\d+)"""".r
  private val ContentCast  = """"content_cast"\s*:\s*"([^"]*)"""".r
  private val ContentGenre = """"content_genre"\s*:\s*"([^"]*)"""".r

  /** Split one of the comma-separated `data-eventjson` list values into trimmed,
   *  non-blank entries (`"a, b ,"` → `List("a", "b")`). */
  private def commaList(value: String): List[String] =
    value.split(",").iterator.map(_.trim).filter(_.nonEmpty).toList

  /** One session slot off the fragment: the film's slug (stable id) + title +
   *  metadata (constant across a film's sessions) and the single screening. */
  case class RawFlicksSlot(
    slug:           String,
    title:          String,
    runtimeMinutes: Option[Int],
    posterUrl:      Option[String],
    director:       Option[String],
    contentId:      Option[String],
    cast:           Seq[String],
    genres:         Seq[String],
    trailerUrl:     Option[String],
    ageRating:      Option[String],
    dateTime:       LocalDateTime,
    booking:        Option[String],
    format:         List[String]
  )

  /** Parse one day's sessions fragment for the given calendar date. Pure +
   *  public so the spec feeds it the recorded HTML directly. */
  def parseDay(html: String, date: LocalDate, market: FlicksMarket): Seq[RawFlicksSlot] = {
    val doc = Jsoup.parse(html, market.baseUrl)
    doc.select("article.cinema-times__article").asScala.toSeq.flatMap { article =>
      val slug  = firstMovieSlug(article)
      val title = Option(article.selectFirst("h3.cinema-times__movie-title")).map(_.text.trim).filter(_.nonEmpty)
      (slug, title) match {
        case (Some(sl), Some(t)) =>
          val runtime   = Option(article.selectFirst(".cinema__movie-duration"))
            .map(_.text).flatMap(s => DigitsPat.findFirstIn(s)).map(_.toInt).filter(_ > 0)
          val poster    = Option(article.selectFirst(".cinema-times__image img")).map(_.attr("src")).filter(_.nonEmpty)
          val director  = Option(article.selectFirst(".cinema__director span")).map(_.text.trim).filter(_.nonEmpty)
          // Every session button in a film's card carries the same `data-eventjson`
          // blob; read the first non-empty one once and lift id/cast/genre from it.
          val eventJson = article.select(".times-calendar-times__button").asScala.iterator
            .map(_.attr("data-eventjson")).find(_.nonEmpty).getOrElse("")
          val contentId = ContentId.findFirstMatchIn(eventJson).map(_.group(1))
          val cast      = ContentCast.findFirstMatchIn(eventJson).map(_.group(1)).map(commaList).getOrElse(Nil)
          val genres    = ContentGenre.findFirstMatchIn(eventJson).map(_.group(1)).map(commaList).getOrElse(Nil)
          val trailer   = Option(article.selectFirst(""".cinema__trailer-wrap a[href^="/trailer/"]"""))
            .map(_.attr("href")).filter(_.nonEmpty)
            .map(h => if (h.startsWith("http")) h else s"${market.baseUrl}$h")
          // The BBFC label ("U"/"PG"/"12A") the card renders in its own element.
          val ageRating = Option(article.selectFirst(".cinema__movie-classification"))
            .map(_.text).flatMap(AgeRating.normalize)

          article.select("li.times-calendar-times__el").asScala.toSeq.flatMap { li =>
            // Tag-agnostic: a bookable session is an `<a …>`, an unbookable one the
            // same button as a `<span>`. Keying on the `<a>` silently dropped every
            // showtime at a venue with no booking deep-links (see the spec).
            val button = Option(li.selectFirst(".times-calendar-times__button"))
            button.flatMap(parseTime).map { time =>
              // A `<span>` button has no href, so this stays None — an unbookable
              // screening is still a screening.
              val booking = button.map(_.attr("href")).filter(_.nonEmpty)
              val label   = Option(li.selectFirst("span.times-calendar-times__el__label span"))
                .map(_.text.trim).filter(_.nonEmpty)
              RawFlicksSlot(sl, t, runtime, poster, director, contentId, cast, genres, trailer,
                ageRating, LocalDateTime.of(date, time), booking, label.toList)
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
