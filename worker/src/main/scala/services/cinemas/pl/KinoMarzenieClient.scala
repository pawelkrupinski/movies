package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DayChunks, DetailEnricher, DetailFetchOutcome, FilmDetail, ScrapeHorizon, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead, ReadOutcome}

import java.time.LocalDate
import java.util.Locale
import scala.jdk.CollectionConverters._

/**
 * Kino Marzenie (Tarnów, run by Tarnowskie Centrum Kultury). Its repertoire
 * page at `kinomarzenie.pl/repertuar` is a bespoke server-rendered site whose
 * own calendar slider swaps days via a lighter AJAX partial:
 * `kinomarzenie.pl/embed/events?start_date=YYYY-MM-DD&end_date=YYYY-MM-DD&category_id=8`
 * (`category_id=8` selects film screenings). There is no whole-programme feed
 * — each request answers for exactly the one requested day, `end_date`
 * notwithstanding — so the programme is read one fetch per day.
 *
 * The day list is the source's own: `/repertuar`'s calendar slider carries one
 * `a.item[data-href]` per day it offers (about two months ahead), each
 * `data-href` the day's `/embed/events` partial — read as the chunk plan.
 *
 * Chunked, because the partials are SLOW: from 2026-09-30 each took 8–10 s to
 * first byte from the worker (connect stays fast), so fourteen sequential days
 * overran the 45 s per-scrape ceiling of `AdaptiveTimeoutScraper` on every
 * scrape and the venue sat red for a week with no Filmweb cover. A chunked
 * cinema is scraped as its own per-chunk tasks, each bounded by itself.
 *
 * Booking is delegated to the parent org's own MSI ticketing backend
 * (`marzenieonline.tck.pl/MSI/...`), linked directly off each showtime with no
 * iframe embed — so it's the listing page, not the ticketing domain, that's
 * scraped.
 *
 * Per `div.row.item` film block on a day's response:
 *   - `h4.m-bottom-10 a`           → title (`ownText`, which skips the leading
 *                                     sr-only "Zobacz więcej na temat:" span)
 *                                     and the `/repertuar/<id>,<slug>` detail URL
 *   - `img.img-responsive`         → poster
 *   - `span.d-block.color-3.small` → "GENRE, GENRE | AGE+ LAT" — genres are the
 *                                     comma-list before the `|`
 *   - one `a.btn.btn-outline-primary` per showtime, whose sibling
 *     `div.d-flex.flex-wrap` carries the format/version tag ("NAPISY" /
 *     "DUBBING"), empty when there is none. A film listed with no showtime
 *     that day (its poster still renders, but the times column is empty) is
 *     dropped for that day — it may still appear on a day it does screen.
 *
 * `category_id=8` already scopes the feed to films, so unlike the venues that
 * share their ticketing surface with concerts/theatre, no [[OnlyMovieEventsFilter]]
 * is needed here.
 *
 * The film's own page (`/repertuar/<id>,<slug>`) lists its facts as
 * `li > span.color-3 "label:"` + value span — "reżyseria", "obsada", "czas
 * trwania" ("1 godz. 37 min."), "produkcja", "premiera" ("24 marca 1980", the
 * year read) — read as the deferred detail ([[fetchFilmDetail]]). Without the
 * director, "DYRYGENT - POŁĄCZONY Z KONCERTEM MUZYKI NA ŻYWO" stood beside every
 * other "Dyrygent"; its page credits Andrzej Wajda, 1980.
 */
class KinoMarzenieClient(
  http:        HttpFetch,
  override val cinema: Cinema = KinoMarzenie,
  today:       => LocalDate
) extends ChunkedCinemaScraper with DetailEnricher {

  import KinoMarzenieClient._

  override val detailGroup: String = "kino-marzenie"

  /** The film page's facts; None on a transient failure, a durable 404/410 escaping. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(parseDetail)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def planChunks(): Seq[String] = {
    val lastDay = today.plusDays(ScrapeHorizon.MaxDays.toLong)
    // the slider's container, not its items: a slider listing no days is a venue with nothing on, a page
    // without one (a maintenance page served 200, a redesign) a failed read that must not publish the venue empty
    val page = HttpRead.html(http, RepertoireUrl, SliderMarker)(ReadOutcome.Answered(_)).required
    DayChunks.keys(sliderDays(page).filter(d => !d.isBefore(today) && !d.isAfter(lastDay)))
  }

  def fetchChunk(key: String): Seq[CinemaMovie] = {
    val slots = DayChunks.days(key).flatMap(date => parseDay(HttpRead.page(http, eventsUrl(date)), date))

    SlotsToMovies.fold(slots, titleOf = _.title, showtimeOf = _.showtime) { (_, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title = head.title, genres = head.genres),
        cinema    = cinema,
        posterUrl = head.poster,
        filmUrl   = head.filmUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoMarzenieClient {

  val BaseUrl              = "https://www.kinomarzenie.pl"
  val RepertoireUrl         = s"$BaseUrl/repertuar"
  private val EventsCategoryId = 8

  private val SliderMarker = HttpRead.PageMarker("calendar-slider")

  /** `start_date=2026-10-09` off a slider item's `data-href` partial URL. */
  private val SliderDayPat = """start_date=(\d{4}-\d{2}-\d{2})""".r

  /** The days `/repertuar`'s calendar slider offers, in order. */
  private[cinemas] def sliderDays(html: String): Seq[LocalDate] =
    Jsoup.parse(html, BaseUrl).select("a.item[data-href]").asScala.toSeq
      .flatMap(a => SliderDayPat.findFirstMatchIn(a.attr("data-href")))
      .flatMap(m => scala.util.Try(LocalDate.parse(m.group(1))).toOption)
      .distinct

  private[cinemas] def eventsUrl(date: LocalDate): String =
    s"$BaseUrl/embed/events?start_date=$date&end_date=$date&category_id=$EventsCategoryId"

  /** The film page's `li > span.color-3 "label:"` + value-span facts. */
  private[cinemas] def parseDetail(html: String): FilmDetail = {
    val fields = Jsoup.parse(html, BaseUrl).select("ul.list-unstyled li").asScala.toSeq.flatMap { item =>
      Option(item.selectFirst("span.color-3")).flatMap(label => Option(label.nextElementSibling).map(value =>
        label.text.trim.stripSuffix(":").trim.toLowerCase(Locale.ROOT) -> value.text.trim))
    }.toMap
    def names(label: String) = fields.get(label).toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
    FilmDetail(
      director       = names("reżyseria"),
      cast           = names("obsada"),
      countries      = names("produkcja"),
      runtimeMinutes = fields.get("czas trwania").flatMap(ScraperParse.hoursMinutesRuntime),
      releaseYear    = fields.get("premiera").flatMap(ScraperParse.yearIn))
  }

  private case class RawSlot(title: String, genres: Seq[String], poster: Option[String],
                              filmUrl: Option[String], showtime: Showtime)

  private def parseDay(html: String, date: LocalDate): Seq[RawSlot] =
    Jsoup.parse(html, BaseUrl).select("div.row.item").asScala.toSeq.flatMap(parseFilmBlock(_, date))

  private def parseFilmBlock(block: Element, date: LocalDate): Seq[RawSlot] = {
    val titleLink = Option(block.selectFirst("h4.m-bottom-10 a"))
    val title = titleLink.map(_.ownText.trim).filter(_.nonEmpty)
    title.toSeq.flatMap { t =>
      val filmUrl = titleLink.map(_.attr("abs:href")).filter(_.nonEmpty)
      val poster  = Option(block.selectFirst("img.img-responsive")).map(_.attr("abs:src")).filter(_.nonEmpty)
      val genres  = Option(block.selectFirst("span.d-block.color-3.small")).map(_.text).toSeq
        .flatMap(_.split("\\|").headOption)
        .flatMap(_.split(",").map(_.trim))
        .filter(_.nonEmpty)

      block.select("a.btn.btn-outline-primary[href]").asScala.toSeq.flatMap { a =>
        ScraperParse.parseHHmm(a.text).map { time =>
          val format = Option(a.parent).flatMap(p => Option(p.selectFirst("div.d-flex.flex-wrap")))
            .map(_.text).toSeq.flatMap(ScraperParse.formatTokensIn).distinct.toList
          RawSlot(t, genres, poster, filmUrl,
            Showtime(date.atTime(time), Option(a.attr("abs:href")).filter(_.nonEmpty), format = format))
        }
      }
    }
  }
}
