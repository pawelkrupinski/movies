package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}
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
 */
class KinoMarzenieClient(
  http:        HttpFetch,
  override val cinema: Cinema = KinoMarzenie,
  today:       LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")),
  windowDays:  Int = 14
) extends CinemaScraper {

  import KinoMarzenieClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    val slots = (0 until windowDays).flatMap { offset =>
      val date = today.plusDays(offset.toLong)
      parseDay(http.get(eventsUrl(date)), date)
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
