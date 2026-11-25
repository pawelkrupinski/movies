package services.cinemas.pl

import java.time.{LocalDate, LocalDateTime}
import java.util.Locale

import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ScrapeClock, ScrapeDates, ScrapeImages, ScrapeRuntime, ScraperParse}
import services.cinemas.common.SlotsToMovies
import services.cinemas.common.{SingleSiteScraper, DetailEnricher, DetailFetchOutcome, FilmDetail}
import tools.{HttpFetch, HttpRead}

/**
 * Kino Muranów (Warszawa, Gutek Film). The repertoire page is a month
 * calendar: each filled day column holds the day's screenings, each with its
 * time, title, ticket link (`/tickets/<id>/buy`) and a link to the film's
 * Drupal node (`/film/<slug>`) carrying the richer metadata. Films are grouped
 * by that slug; the per-film detail page is fetched for runtime / director /
 * year / countries / genres / synopsis, degrading to listing-only on failure.
 */
class MuranowClient(http: HttpFetch, today: => LocalDate
) extends SingleSiteScraper with DetailEnricher {


  val cinema: Cinema = KinoMuranow

  private val BaseUrl       = "https://kinomuranow.pl"
  private val RepertoireUrl = s"$BaseUrl/repertuar/"

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, poster: Option[String], bookingUrl: Option[String])

  protected def siteUrl: String = BaseUrl

  def fetch(): Seq[CinemaMovie] = {
    val document  = Jsoup.parse(HttpRead.page(http, RepertoireUrl))
    val year = MuranowClient.yearFromLabel(Option(document.selectFirst("p.calendar-seance-full__month-label")).map(_.text).getOrElse(""), today.getYear)

    val slots = document.select("div.calendar-seance-full__day--filled").asScala.toSeq.flatMap { day =>
      MuranowClient.dayDate(day, year) match {
        case None => Seq.empty[RawSlot]
        case Some(date) =>
          day.select("div.movie-calendar-info").asScala.toSeq.flatMap { info =>
            for {
              _     <- Option(info.selectFirst("div.movie-calendar-info__inner"))  // only a slot with an inner block
              time  <- Option(info.selectFirst("span.movie-calendar-info__date")).map(_.text.trim)
              dt    <- MuranowClient.parseTime(date, time)
              title <- Option(info.selectFirst("h5.movie-calendar-info__title")).map(_.text.trim).filter(_.nonEmpty)
            } yield {
              val expand  = Option(info.selectFirst("div.movie-calendar-info-expand"))
              val thumb   = expand.flatMap(e => Option(e.selectFirst("a.movie-calendar-info-expand__thumb")))
              val slug    = thumb.map(_.attr("href")).flatMap(MuranowClient.slugOf).getOrElse(title)
              val poster  = thumb.flatMap(t => Option(t.selectFirst("img[src]"))).flatMap(ScrapeImages.largestImageSource)
              val booking = expand.flatMap(e => Option(e.selectFirst("a[href][class*=c-button-tickets--buy-link]")))
                              .map(_.attr("href")).filter(_.nonEmpty).map(abs)
              RawSlot(slug, title, dt, poster, booking)
            }
          }
      }
    }

    SlotsToMovies.fold(slots, _.slug, s => Showtime(s.dateTime, s.bookingUrl, None, Nil), order = SlotsToMovies.MovieOrder.asGrouped) { (slug, group, showtimes) =>
      val primary = group.head
      CinemaMovie(
        movie      = Movie(title = primary.title),
        cinema     = cinema,
        posterUrl  = group.flatMap(_.poster).headOption,
        filmUrl    = if (slug.startsWith("/")) Some(BaseUrl + slug) else None,
        showtimes  = showtimes,
        trailerUrl = None
      )
    }
  }

  override val detailGroup: String = "muranow"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://kinomuranow.pl/film/<slug>`). None on fetch
   *  failure so the task stays stale and is retried rather than recording an
   *  empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map { html =>
      val detail = MuranowClient.parseDetail(html)
      FilmDetail(
        synopsis       = detail.synopsis,
        cast           = detail.cast,
        director       = detail.director,
        runtimeMinutes = detail.runtimeMinutes,
        releaseYear    = detail.year,
        originalTitle  = detail.originalTitle,
        countries      = detail.countries,
        genres         = detail.genres,
        trailerUrl     = detail.trailer
      )
    }

  private def abs(href: String): String = if (href.startsWith("http")) href else BaseUrl + href
}

object MuranowClient {

  private val YearPat = """\b(20\d{2})\b""".r

  /** A credits-only body line, e.g. "Przechodzień Passer-By reż. Andrzej
   *  Titkow, Polska, 1984, 36 min" — director + country + year + runtime, no
   *  prose. Used to skip such lines when picking the synopsis paragraph. */
  private[cinemas] val CreditsLine = """(?i)\breż\.\s.*,\s*\d{4},\s*\d+\s*min\s*$""".r

  def yearFromLabel(label: String, fallbackYear: Int): Int =
    YearPat.findFirstMatchIn(label).map(_.group(1).toInt).getOrElse(fallbackYear)

  def slugOf(href: String): Option[String] =
    """(/film/[^?#"']+)""".r.findFirstMatchIn(href).map(_.group(1))

  /** Build the cell's date from its day-number + Polish month name, taking the
   *  year from the calendar header but rolling forward when the cell's month is
   *  earlier than the header's (a December→January calendar wrap). */
  def dayDate(day: Element, headerYear: Int): Option[java.time.LocalDate] =
    for {
      dStr  <- Option(day.selectFirst("span.cell-date-header__day-num")).map(_.text.trim)
      mName <- Option(day.selectFirst("span.cell-date-header__day-month")).map(_.text.trim.toLowerCase(Locale.ROOT))
      d     <- Try(dStr.toInt).toOption
      m     <- ScrapeDates.polishMonth(mName)
    } yield java.time.LocalDate.of(headerYear, m, d)

  def parseTime(date: java.time.LocalDate, time: String): Option[LocalDateTime] =
    ScrapeClock.atClock(date, time)

  final case class Detail(
    runtimeMinutes: Option[Int],
    year:           Option[Int],
    countries:      Seq[String],
    genres:         Seq[String],
    director:       Seq[String],
    cast:           Seq[String],
    originalTitle:  Option[String],
    synopsis:       Option[String],
    trailer:        Option[String]
  )
  object Detail { val empty: Detail = Detail(None, None, Seq.empty, Seq.empty, Seq.empty, Seq.empty, None, None, None) }

  /** One running time per film the screening shows, "+"-joined: "36 min + 93 min" (a short before a feature). */
  private val FilmRuntimes = """\d+\s*min\.?(?:\s*\+\s*\d+\s*min\.?)+""".r
  private val Minutes      = """\d+""".r

  /** How long the screening runs: "97 min"; a relay's "około 2 godzin i 50 minut, w tym jedna przerwa", its hours
   *  counted too; a bill of several films' "36 min + 93 min" (the 100th Konwicki anniversary's "Przechodzień" and "Jak
   *  daleko stąd, jak blisko"), their sum: the bill is neither film, and its first film's 36 minutes read as the whole
   *  screening's made a fact for the short alone. */
  private[cinemas] def screeningRuntime(duration: String): Option[Int] = duration.trim match {
    case FilmRuntimes() => Some(Minutes.findAllIn(duration).map(_.toInt).sum).filter(_ > 0)
    case _              => ScrapeRuntime.hoursMinutesRuntime(duration)
  }

  private def field(document: org.jsoup.nodes.Document, name: String): Option[String] =
    Option(document.selectFirst(s"div.field--name-$name .field__item")).map(_.text.trim).filter(_.nonEmpty)

  private def fields(document: org.jsoup.nodes.Document, name: String): Seq[String] =
    document.select(s"div.field--name-$name .field__item").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)

  /** Parse the Drupal `/film/<slug>` node for the metadata fields. */
  def parseDetail(html: String): Detail = {
    val document = Jsoup.parse(html)
    Detail(
      runtimeMinutes = field(document, "field-movie-duration").flatMap(screeningRuntime),
      year           = field(document, "field-movie-production-year").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries      = fields(document, "field-movie-production-country"),
      genres         = field(document, "field-movie-category").toSeq.flatMap(ScraperParse.commaItems)
                         .map(tools.TextNormalization.titleCaseIfAllLower),
      director       = field(document, "field-movie-director").toSeq.flatMap(ScraperParse.commaItems),
      cast           = field(document, "field-movie-cast").toSeq.flatMap(ScraperParse.commaItems),
      originalTitle  = field(document, "field-movie-original-title"),
      // Retrospective screenings lead the body with a credits-only paragraph
      // ("<title> <orig> reż. <dir>, <country>, <year>, <NN> min") before the
      // real prose. Take the first body paragraph that ISN'T such a line.
      synopsis       = document.select("div.field--name-body p").asScala.toSeq.map(_.text.trim)
                         .filter(_.length > 20).find(MuranowClient.CreditsLine.findFirstIn(_).isEmpty),
      // The Drupal remote-video paragraph renders the trailer as
      // `<div class="youtube-player" data-vid="<id>">`; build the canonical
      // watch URL from the id.
      trailer        = Option(document.selectFirst("div.youtube-player[data-vid]")).map(_.attr("data-vid")).filter(_.nonEmpty)
                         .flatMap(id => ScraperParse.canonicalTrailer(s"https://www.youtube.com/watch?v=$id"))
    )
  }
}
