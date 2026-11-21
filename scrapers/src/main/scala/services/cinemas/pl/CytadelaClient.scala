package services.cinemas.pl

import java.time.{LocalDate, LocalDateTime}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.ScraperParse
import services.cinemas.common.SlotsToMovies
import services.cinemas.common.SynopsisProse
import services.cinemas.common.{SingleSiteScraper, DetailEnricher, DetailFetchOutcome, FilmDetail}
import services.titles.FormatTags
import tools.{HttpFetch, HttpRead}

/**
 * Kino Cytadela (Muzeum Historii Polski, Warszawa). The `/repertuar` page is a
 * flat list of day headers (`time.repertoire-list__title[datetime]`) and
 * screening rows (`div.repertoire-item`); each row carries its time, title,
 * genre, year/director/runtime and a sklep.muzhp.pl booking link. The
 * `/kino-film/<slug>` page adds the synopsis. (`/repertuar`, NOT `/pl/repertuar`
 * — the latter is the JS app shell.)
 */
class CytadelaClient(http: HttpFetch
) extends SingleSiteScraper with DetailEnricher {


  val cinema: Cinema = KinoCytadela

  private val BaseUrl    = "https://muzhp.pl"
  private val ListingUrl = s"$BaseUrl/repertuar"
  private val SlugPat    = """/kino-film/([a-z0-9-]+)""".r

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, genres: Seq[String],
                             year: Option[Int], director: Seq[String], runtime: Option[Int],
                             booking: Option[String], poster: Option[String], version: List[String])

  protected def siteUrl: String = BaseUrl

  def fetch(): Seq[CinemaMovie] = parseListing(HttpRead.page(http, ListingUrl))

  private def parseListing(html: String): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html)

    var date: Option[LocalDate] = None
    val slots = document.select("time.repertoire-list__title, div.repertoire-item").asScala.toSeq.flatMap { element =>
      if (element.hasClass("repertoire-list__title")) { date = Try(LocalDate.parse(element.attr("datetime"))).toOption; Seq.empty }
      else date.toSeq.flatMap(d => rowSlot(element, d))
    }

    SlotsToMovies.fold(slots, _.slug, s => Showtime(s.dateTime, s.booking, None, s.version), order = SlotsToMovies.MovieOrder.asGrouped) { (slug, group, showtimes) =>
      val primary = group.head
      CinemaMovie(
        movie     = Movie(title = primary.title, runtimeMinutes = primary.runtime, releaseYear = primary.year, genres = primary.genres),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/kino-film/$slug") else None,
        director  = primary.director,
        showtimes = showtimes
      )
    }
  }

  override val detailGroup: String = "cytadela"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl. None on a fetch failure so the task stays stale and is
   *  retried by the next scrape rather than recording an empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map { html =>
      val document      = Jsoup.parse(html)
      // The description tails into an inline venue/ticket footer — a "♦ Bilety:
      // …", a "Kino Cytadela znajduje się …" venue blurb, a "◊ sprawdź dojazd
      // TUTAJ" link, or a "W programie:" event agenda. These are plain text
      // after the prose (not separate elements), so truncate at the first
      // footer marker.
      val synopsis = Option(document.selectFirst("div.article-info__text.description")).map(SynopsisProse.blockText(_).trim)
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
      val director = spans.flatMap(x => """(?i)reżyseria:\s*(.+)$""".r.findFirstMatchIn(x).map(_.group(1).trim)).headOption
                     .toSeq.flatMap(ScraperParse.commaItems)
      val runtime = spans.flatMap(x => """(?i)czas:\s*(\d+)\s*min""".r.findFirstMatchIn(x).map(_.group(1).toInt)).headOption
      val booking = Option(item.selectFirst("a.repertoire-item__container__button--dark[href]")).map(_.attr("href")).filter(_.nonEmpty)
      val poster  = Option(item.selectFirst("img.repertoire-item__image[src]")).flatMap(ScraperParse.largestImageSource)
                      .map(ScraperParse.onSite(BaseUrl))
      // The row's language badge ("dubbing", "napisy", "polski lektor"; "oryginalny" names none), which the title never repeats.
      val version = item.select("li.repertoire-item__info__item").asScala.toSeq
                      .flatMap(badge => FormatTags.formatTokensIn(badge.text)).distinct.toList
      RawSlot(s, t, date.atTime(tm), genres, year, director, runtime, booking, poster, version)
    }
  }
}

object CytadelaClient {
  /** Inline footer that tails Cytadela synopses: a "♦ Bilety…" price block, a
   *  "◊ sprawdź dojazd" line, or a "W programie:" event agenda. Everything from
   *  the first marker onward is dropped. */
  private[cinemas] val FooterMarker = """(?s)\s*(?:[♦◊]|W programie:).*$""".r
}
