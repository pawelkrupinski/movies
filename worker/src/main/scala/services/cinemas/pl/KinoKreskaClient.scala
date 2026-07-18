package services.cinemas.pl

import play.api.libs.json.Json
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kreska — the cinema run by Studio Filmów Rysunkowych in Bielsko-Biała.
 * Screenings are served via the SFR CMS through a JSON/HTML hybrid endpoint
 * that returns rendered `<li>` tiles per screening:
 *
 *   POST /heroapp/terms/rest/load
 *   Body: offset=0&itemsPerLoading=200&filters[category]=Repertuar kinowy&orderBy=&lang=pl
 *
 * The response is JSON with `status: 0` (success) and an `items` field
 * containing raw HTML. Each tile is a `<li class="ajax-loading-items-manager__item">` with:
 *   - `div.std-horizontal-tile__time div` (first child)  → time `HH:MM`
 *   - `div.std-horizontal-tile__time div` (second child) → date `YYYY-MM-DD`
 *   - `a.std-link.std-link--hudge`                       → film title + per-event href
 *   - `a.std-button`                                     → booking link
 *
 * The `filters[category]=Repertuar kinowy` narrows results to cinema screenings
 * only, excluding the permanent "Wystawa Stała w OKU" exhibition slots and other
 * non-film events.
 *
 * Previously served from Filmweb; migrated when Filmweb stopped carrying the
 * venue's repertoire reliably.
 */
class KinoKreskaClient(
  http:  HttpFetch,
  override val cinema: Cinema,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoKreskaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/kino-kreska")

  def fetch(): Seq[CinemaMovie] = {
    val json = Try(
      http.post(TermsUrl, PostBody, "application/x-www-form-urlencoded")
    ).getOrElse("")
    if (json.isEmpty) return Seq.empty

    val itemsHtml = Try(
      (Json.parse(json) \ "items").asOpt[String].getOrElse("")
    ).getOrElse("")
    if (itemsHtml.isEmpty) return Seq.empty

    parse(itemsHtml, cinema)
  }
}

object KinoKreskaClient {

  val BaseUrl  = "https://www.sfr.pl"
  val TermsUrl = s"$BaseUrl/heroapp/terms/rest/load"

  // Percent-encoded form body: filters[category]=Repertuar kinowy, returns only cinema events.
  val PostBody = "offset=0&itemsPerLoading=200&filters%5Bcategory%5D=Repertuar+kinowy&orderBy=&lang=pl"

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    filmUrl:  Option[String]
  )

  def parse(itemsHtml: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(itemsHtml, BaseUrl)
    val slots = document.select("li.ajax-loading-items-manager__item").asScala.toSeq.flatMap { li =>
      val timeDivs = li.select("div.std-horizontal-tile__time div").asScala.toSeq
      for {
        timeStr <- timeDivs.headOption.map(_.text.trim)
        dateStr <- timeDivs.drop(1).headOption.map(_.text.trim)
        time    <- Try(java.time.LocalTime.parse(timeStr)).toOption
        date    <- Try(LocalDate.parse(dateStr)).toOption
        titleEl <- Option(li.selectFirst("a.std-link.std-link--hudge"))
        title    = titleEl.text.trim if title.nonEmpty
      } yield RawSlot(
        title    = title,
        dateTime = LocalDateTime.of(date, time),
        booking  = Option(li.selectFirst("a.std-button")).map(_.attr("abs:href")).filter(_.nonEmpty),
        filmUrl  = Option(titleEl.attr("abs:href")).filter(_.nonEmpty)
      )
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(ScraperParse.stripFormatTags(title)),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}
