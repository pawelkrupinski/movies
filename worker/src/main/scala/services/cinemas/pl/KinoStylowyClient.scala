package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{AgeRating, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ListingPages, ScraperParse, SlotsToMovies}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * CKF Stylowy (Zamość). A bespoke site whose repertoire is one page per day,
 * `repertuar/repertuar.html?rep_date=YYYY-MM-DD`; every page carries the same
 * date strip (`a[href*='rep_date=']`) naming the days the cinema has programmed
 * (twelve at capture time). The scrape reads today's page, then every other
 * day the strip offers — a page that fails is tolerated while any other
 * answers ([[ListingPages.requireAnyReached]]).
 *
 * A day page holds one `div.card` per film:
 *   - `a.card-title strong`       → the (ALL-CAPS) title; recased centrally
 *   - `a.card-title[href]`        → the `/film/<id>-<slug>.html` detail page
 *   - `.startRepertuarPoster img` → poster
 *   - `a[data-ytlink]`            → the YouTube trailer id
 *   - `p.card-text.text-secondary`→ genres, then an `em.badge` "od lat N" age
 *   - `.btnKupBilet25`            → one per screening: `a.bKB25hour` is the
 *     `HH:MM` and the iKsoris booking link (`bilety.stylowy.net`),
 *     `.bKB25feat` any version badges
 * The listing's synopsis is truncated with "…", so it is left to the detail.
 *
 * The detail page carries what TMDB identifies a film by and the listing
 * lacks: `<strong>original title</strong> / countries, YEAR, genres` plus a
 * runtime badge, and labelled `Reżyseria` / `Obsada` / `Opis` blocks — hence
 * [[DetailEnricher]] with resolution deferred until the detail lands.
 */
class KinoStylowyClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoStylowy,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with DetailEnricher {

  import KinoStylowyClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl, BookingBaseUrl)
  override def sourceUrl: Option[String] = Some(dayUrl(today))

  def fetch(): Seq[CinemaMovie] = {
    // Today's page must answer: it is the one that names the other days.
    val first     = Jsoup.parse(http.get(dayUrl(today)), BaseUrl)
    val otherDays = programmedDays(first).filter(_.isAfter(today))
    val others    = otherDays.map(day => day -> Try(Jsoup.parse(http.get(dayUrl(day)), BaseUrl)))
    ListingPages.requireAnyReached(others.map(_._2))
    val pages = (today -> first) +: others.flatMap { case (day, page) => page.toOption.map(day -> _) }
    parse(pages, cinema)
  }

  override val detailGroup: String = "kino-stylowy"

  /** A durable 404/410 escapes (see [[DetailFetchOutcome]]); a loaded page is a
   *  detail even when it parses to nothing, so it is stamped, not retried. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map(parseDetail)
}

object KinoStylowyClient {

  val BaseUrl        = "https://www.stylowy.net"
  val BookingBaseUrl = "https://bilety.stylowy.net"

  def dayUrl(day: LocalDate): String = s"$BaseUrl/repertuar/repertuar.html?rep_date=$day"

  private val RepDate  = """rep_date=(\d{4}-\d{2}-\d{2})""".r
  private val AgeDigits = """(\d+)""".r
  private val Year     = """(?:19|20)\d{2}""".r

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    format:    List[String],
    filmUrl:   Option[String],
    poster:    Option[String],
    trailer:   Option[String],
    genres:    Seq[String],
    ageRating: Option[String]
  )

  /** The days the page's date strip links to, in order, deduplicated. */
  private[pl] def programmedDays(page: Document): Seq[LocalDate] =
    page.select("a[href*='rep_date=']").asScala.toSeq
      .flatMap(a => RepDate.findFirstMatchIn(a.attr("href")))
      .flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)
      .distinct

  private[pl] def parse(pages: Seq[(LocalDate, Document)], cinema: Cinema): Seq[CinemaMovie] = {
    val slots = pages.flatMap { case (day, page) => page.select("#startRepertuar div.card").asScala.toSeq.flatMap(filmCard(_, day)) }
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie      = Movie(title, genres = head.genres),
        cinema     = cinema,
        posterUrl  = head.poster,
        filmUrl    = head.filmUrl,
        synopsis   = None,
        cast       = Seq.empty,
        director   = Seq.empty,
        showtimes  = showtimes,
        trailerUrl = head.trailer,
        ageRating  = head.ageRating
      )
    }
  }

  private def filmCard(card: Element, day: LocalDate): Seq[RawSlot] =
    Option(card.selectFirst("a.card-title")).toSeq.flatMap { link =>
      val title = Option(link.selectFirst("strong")).getOrElse(link).text.trim
      val info  = Option(card.selectFirst("p.card-text.text-secondary"))
      val genres = info.toSeq.flatMap(_.ownText.split(",")).map(_.trim).filter(_.nonEmpty)
      val age    = info.flatMap(i => Option(i.selectFirst("em.badge"))).flatMap(ageOf)
      val filmUrl = Option(link.attr("abs:href")).filter(_.nonEmpty)
      val poster  = Option(card.selectFirst(".startRepertuarPoster img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)
      val trailer = Option(card.selectFirst("[data-ytlink]")).map(_.attr("data-ytlink").trim).filter(_.nonEmpty)
        .flatMap(id => ScraperParse.canonicalTrailer(s"https://www.youtube.com/watch?v=$id"))
      if (title.isEmpty) Seq.empty
      else card.select(".btnKupBilet25").asScala.toSeq.flatMap { button =>
        val hour = Option(button.selectFirst("a.bKB25hour"))
        hour.flatMap(h => ScraperParse.parseHHmm(h.text)).map { time =>
          RawSlot(
            title     = title,
            dateTime  = LocalDateTime.of(day, time),
            booking   = hour.map(_.attr("abs:href")).filter(_.nonEmpty),
            format    = Option(button.selectFirst(".bKB25feat")).map(f => ScraperParse.formatTokensIn(f.text)).getOrElse(Nil),
            filmUrl   = filmUrl,
            poster    = poster,
            trailer   = trailer,
            genres    = genres,
            ageRating = age
          )
        }
      }
    }

  /** "od lat 12" → "12+"; "od lat " (no number: unrated) → None. */
  private def ageOf(badge: Element): Option[String] =
    AgeDigits.findFirstIn(badge.text).flatMap(n => AgeRating.normalize(s"$n+"))

  private[pl] def parseDetail(html: String): FilmDetail = {
    val doc  = Jsoup.parse(html, BaseUrl)
    val meta = Option(doc.selectFirst("#pageFilm div.text-secondary.border-top"))
    val originalTitle = meta.flatMap(m => Option(m.selectFirst("strong"))).map(_.text.trim).filter(_.nonEmpty)
    // "/ Francja, Belgia, 2025, animowany": countries precede the year, genres follow it.
    val parts = meta.toSeq.flatMap(_.ownText.stripPrefix("/").trim.stripPrefix("/").split(",")).map(_.trim).filter(_.nonEmpty)
    val yearAt = parts.indexWhere(Year.matches)
    val (countries, year, genres) =
      if (yearAt < 0) (Seq.empty, None, Seq.empty)
      else (parts.take(yearAt), Some(parts(yearAt).toInt), parts.drop(yearAt + 1))
    val runtime = meta.flatMap(m => Option(m.selectFirst("em.badge"))).flatMap(e => ScraperParse.hoursMinutesRuntime(e.text))

    val fields = doc.select("#pageFilm label").asScala.iterator.flatMap { label =>
      Option(label.nextElementSibling).map(v => label.text.trim.stripSuffix(":").toLowerCase -> v)
    }.toMap
    def people(label: String): Seq[String] =
      fields.get(label).toSeq
        .flatMap(_.text.split(","))
        .map(_.replaceAll("""\s*\([^)]*\)""", "").trim.stripSuffix(".").trim)
        .filter(_.nonEmpty)
    val synopsis = doc.select("#pageFilm h2.labelh2").asScala.find(_.text.trim.startsWith("Opis"))
      .flatMap(h => Option(h.nextElementSibling))
      .map(ScraperParse.cleanSynopsis(_)).filter(_.nonEmpty)

    FilmDetail(
      synopsis       = synopsis,
      cast           = people("obsada"),
      director       = people("reżyseria"),
      runtimeMinutes = runtime,
      releaseYear    = year,
      originalTitle  = originalTitle,
      countries      = countries,
      genres         = genres
    )
  }
}
