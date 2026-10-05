package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead}

import java.time.LocalDate
import java.util.Locale
import scala.jdk.CollectionConverters._

/**
 * Kino Marzenie (Tarnów, run by Tarnowskie Centrum Kultury). Its repertoire
 * page at `kinomarzenie.pl/repertuar` is a bespoke server-rendered site whose
 * own calendar slider swaps days via a lighter AJAX partial:
 * `kinomarzenie.pl/embed/events?start_date=YYYY-MM-DD&end_date=YYYY-MM-DD&category_id=8`
 * (`category_id=8` selects film screenings). There is no whole-programme feed
 * — each request answers for exactly the one requested day — so the client
 * sweeps a fixed window of days from `today`, one fetch per day.
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
  today:       => LocalDate,
  windowDays:  Int = 14
) extends CinemaScraper with DetailEnricher {

  import KinoMarzenieClient._

  override val detailGroup: String = "kino-marzenie"

  /** The film page's facts; None on a transient failure, a durable 404/410 escaping. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(parseDetail)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    val slots = (0 until windowDays).flatMap { offset =>
      val date = today.plusDays(offset.toLong)
      parseDay(HttpRead.page(http, eventsUrl(date)), date)
    }

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
