package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import tools.{CachingDetailFetch, HttpFetch}
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Muranów (Warszawa, Gutek Film). The repertoire page is a month
 * calendar: each filled day column holds the day's screenings, each with its
 * time, title, ticket link (`/tickets/<id>/buy`) and a link to the film's
 * Drupal node (`/film/<slug>`) carrying the richer metadata. Films are grouped
 * by that slug; the per-film detail page is fetched for runtime / director /
 * year / countries / genres / synopsis, degrading to listing-only on failure.
 */
class MuranowClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper with DetailEnricher {

  // Static film node pages cached across passes; the calendar listing keeps the
  // live `http` since its showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = KinoMuranow

  private val BaseUrl       = "https://kinomuranow.pl"
  private val RepertoireUrl = s"$BaseUrl/repertuar/"

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, poster: Option[String], bookingUrl: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = fetchBare()

  private def fetchBare(): Seq[CinemaMovie] = {
    val document  = Jsoup.parse(http.get(RepertoireUrl))
    val year = MuranowClient.yearFromLabel(Option(document.selectFirst("p.calendar-seance-full__month-label")).map(_.text).getOrElse(""), today.getYear)

    val slots = document.select("div.calendar-seance-full__day--filled").asScala.toSeq.flatMap { day =>
      MuranowClient.dayDate(day, year) match {
        case None => Seq.empty[RawSlot]
        case Some(date) =>
          day.select("div.movie-calendar-info").asScala.toSeq.flatMap { info =>
            for {
              inner <- Option(info.selectFirst("div.movie-calendar-info__inner"))
              id     = inner.attr("data-id")
              time  <- Option(info.selectFirst("span.movie-calendar-info__date")).map(_.text.trim)
              dt    <- MuranowClient.parseTime(date, time)
              title <- Option(info.selectFirst("h5.movie-calendar-info__title")).map(_.text.trim).filter(_.nonEmpty)
            } yield {
              val expand  = Option(info.selectFirst("div.movie-calendar-info-expand"))
              val thumb   = expand.flatMap(e => Option(e.selectFirst("a.movie-calendar-info-expand__thumb")))
              val slug    = thumb.map(_.attr("href")).flatMap(MuranowClient.slugOf).getOrElse(title)
              val poster  = thumb.flatMap(t => Option(t.selectFirst("img[src]"))).map(_.attr("src")).filter(_.nonEmpty)
              val booking = expand.flatMap(e => Option(e.selectFirst("a[href][class*=c-button-tickets--buy-link]")))
                              .map(_.attr("href")).filter(_.nonEmpty).map(abs)
              RawSlot(slug, title, dt, poster, booking)
            }
          }
      }
    }

    val bySlug = slots.groupBy(_.slug)
    bySlug.toSeq.flatMap { case (slug, group) =>
      val primary   = group.head
      val showtimes = group.distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
                        .map(s => Showtime(s.dateTime, s.bookingUrl, None, Nil))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie      = Movie(title = primary.title),
        cinema     = cinema,
        posterUrl  = group.flatMap(_.poster).headOption,
        filmUrl    = if (slug.startsWith("/")) Some(BaseUrl + slug) else None,
        synopsis   = None,
        cast       = Seq.empty,
        director   = Seq.empty,
        showtimes  = showtimes,
        trailerUrl = None
      ))
    }
  }

  override val detailGroup: String = "muranow"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://kinomuranow.pl/film/<slug>`). None on fetch
   *  failure so the task stays stale and is retried rather than recording an
   *  empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
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

  def yearFromLabel(label: String, fallbackYear: Int = LocalDate.now(ZoneId.of("Europe/Warsaw")).getYear): Int =
    YearPat.findFirstMatchIn(label).map(_.group(1).toInt).getOrElse(fallbackYear)

  def slugOf(href: String): Option[String] =
    """(/film/[^?#"']+)""".r.findFirstMatchIn(href).map(_.group(1))

  /** Build the cell's date from its day-number + Polish month name, taking the
   *  year from the calendar header but rolling forward when the cell's month is
   *  earlier than the header's (a December→January calendar wrap). */
  def dayDate(day: Element, headerYear: Int): Option[java.time.LocalDate] =
    for {
      dStr  <- Option(day.selectFirst("span.cell-date-header__day-num")).map(_.text.trim)
      mName <- Option(day.selectFirst("span.cell-date-header__day-month")).map(_.text.trim.toLowerCase)
      d     <- Try(dStr.toInt).toOption
      m     <- ScraperParse.PolishMonths.get(mName)
    } yield java.time.LocalDate.of(headerYear, m, d)

  def parseTime(date: java.time.LocalDate, time: String): Option[LocalDateTime] =
    """(\d{1,2}):(\d{2})""".r.findFirstMatchIn(time).flatMap { m =>
      Try(date.atTime(m.group(1).toInt, m.group(2).toInt)).toOption
    }

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

  private def field(document: org.jsoup.nodes.Document, name: String): Option[String] =
    Option(document.selectFirst(s"div.field--name-$name .field__item")).map(_.text.trim).filter(_.nonEmpty)

  private def fields(document: org.jsoup.nodes.Document, name: String): Seq[String] =
    document.select(s"div.field--name-$name .field__item").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)

  /** Parse the Drupal `/film/<slug>` node for the metadata fields. */
  def parseDetail(html: String): Detail = {
    val document = Jsoup.parse(html)
    Detail(
      runtimeMinutes = field(document, "field-movie-duration").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year           = field(document, "field-movie-production-year").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries      = fields(document, "field-movie-production-country"),
      genres         = field(document, "field-movie-category").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
                         .map(tools.TextNormalization.titleCaseIfAllLower),
      director       = field(document, "field-movie-director").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      cast           = field(document, "field-movie-cast").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
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
