package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import tools.{CachingDetailFetch, HttpFetch}
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Cytadela (Muzeum Historii Polski, Warszawa). The `/repertuar` page is a
 * flat list of day headers (`time.repertoire-list__title[datetime]`) and
 * screening rows (`div.repertoire-item`); each row carries its time, title,
 * genre, year/director/runtime and a sklep.muzhp.pl booking link. The
 * `/kino-film/<slug>` page adds the synopsis. (`/repertuar`, NOT `/pl/repertuar`
 * — the latter is the JS app shell.)
 */
class CytadelaClient(http: HttpFetch) extends CinemaScraper with DetailEnricher {

  // Static /kino-film detail pages cached across passes; the repertoire listing
  // keeps the live `http` since its showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = KinoCytadela

  private val BaseUrl    = "https://muzhp.pl"
  private val ListingUrl = s"$BaseUrl/repertuar"
  private val SlugPat    = """/kino-film/([a-z0-9-]+)""".r

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, genres: Seq[String],
                             year: Option[Int], director: Seq[String], runtime: Option[Int],
                             booking: Option[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = parseListing(http.get(ListingUrl))

  private def parseListing(html: String): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html)

    var date: Option[LocalDate] = None
    val slots = document.select("time.repertoire-list__title, div.repertoire-item").asScala.toSeq.flatMap { element =>
      if (element.hasClass("repertoire-list__title")) { date = Try(LocalDate.parse(element.attr("datetime"))).toOption; Seq.empty }
      else date.toSeq.flatMap(d => rowSlot(element, d))
    }

    val bySlug = slots.groupBy(_.slug)
    bySlug.toSeq.flatMap { case (slug, group) =>
      val primary   = group.head
      val showtimes = group.map(s => Showtime(s.dateTime, s.booking, None, Nil))
                       .distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title, runtimeMinutes = primary.runtime, releaseYear = primary.year, genres = primary.genres),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/kino-film/$slug") else None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = primary.director,
        showtimes = showtimes
      ))
    }
  }

  override val detailGroup: String = "cytadela"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl. None on a fetch failure so the task stays stale and is
   *  retried by the next scrape rather than recording an empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val document      = Jsoup.parse(html)
      // The description tails into an inline venue/ticket footer — a "♦ Bilety:
      // …", a "Kino Cytadela znajduje się …" venue blurb, a "◊ sprawdź dojazd
      // TUTAJ" link, or a "W programie:" event agenda. These are plain text
      // after the prose (not separate elements), so truncate at the first
      // footer marker.
      val synopsis = Option(document.selectFirst("div.article-info__text.description")).map(ScraperParse.blockText(_).trim)
        .map(CytadelaClient.FooterMarker.replaceFirstIn(_, "").trim).filter(_.length > 20)
      FilmDetail(synopsis = synopsis)
    }

  private def rowSlot(item: Element, date: LocalDate): Option[RawSlot] = {
    val link = Option(item.selectFirst("h2.repertoire-item__content__title a[href]"))
    val slug = link.flatMap(a => SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1)))
    val title = link.map(_.text.trim).filter(_.nonEmpty)
    val time = Option(item.selectFirst("time.repertoire-item__time")).map(t => Option(t.attr("datetime")).filter(_.nonEmpty).getOrElse(t.text).trim)
                 .flatMap(ScraperParse.parseHHmm)
    for { s <- slug; t <- title; tm <- time } yield {
      val genres = item.select("li.repertoire-item__categories__category").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)
                     .map(tools.TextNormalization.titleCaseIfAllLower)
      val spans  = item.select("span.text-with-sections-movies__item__content-text").asScala.toSeq.map(_.text.trim)
      val year   = spans.flatMap(x => """(?i)rok produkcji:\s*(\d{4})""".r.findFirstMatchIn(x).map(_.group(1).toInt)).headOption
      val directory    = spans.flatMap(x => """(?i)reżyseria:\s*(.+)$""".r.findFirstMatchIn(x).map(_.group(1).trim)).headOption
                     .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      val rt     = spans.flatMap(x => """(?i)czas:\s*(\d+)\s*min""".r.findFirstMatchIn(x).map(_.group(1).toInt)).headOption
      val booking = Option(item.selectFirst("a.repertoire-item__container__button--dark[href]")).map(_.attr("href")).filter(_.nonEmpty)
      val poster  = Option(item.selectFirst("img.repertoire-item__image[src]")).map(_.attr("src")).filter(_.nonEmpty)
                      .map(u => if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}")
      RawSlot(s, t, date.atTime(tm), genres, year, directory, rt, booking, poster)
    }
  }
}

object CytadelaClient {
  /** Inline footer that tails Cytadela synopses: a "♦ Bilety…" price block, a
   *  "◊ sprawdź dojazd" line, or a "W programie:" event agenda. Everything from
   *  the first marker onward is dropped. */
  private[cinemas] val FooterMarker = """(?s)\s*(?:[♦◊]|W programie:).*$""".r
}
