package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.LocalDateTime
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
class MuranowClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoMuranow

  private val BaseUrl       = "https://kinomuranow.pl"
  private val RepertoireUrl = s"$BaseUrl/repertuar/"

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, poster: Option[String], bookingUrl: Option[String])

  def fetch(): Seq[CinemaMovie] = {
    val doc  = Jsoup.parse(http.get(RepertoireUrl))
    val year = MuranowClient.yearFromLabel(Option(doc.selectFirst("p.calendar-seance-full__month-label")).map(_.text).getOrElse(""))

    val slots = doc.select("div.calendar-seance-full__day--filled").asScala.toSeq.flatMap { day =>
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

    val bySlug  = slots.groupBy(_.slug)
    val details = {
      val pending = bySlug.keys.toSeq.filter(_.startsWith("/")).map(slug => slug -> http.getAsync(BaseUrl + slug))
      pending.map { case (slug, f) => slug -> Try(f.join()).toOption.map(MuranowClient.parseDetail).getOrElse(MuranowClient.Detail.empty) }.toMap
    }

    bySlug.toSeq.flatMap { case (slug, group) =>
      val primary = group.head
      val showtimes = group.distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
                        .map(s => Showtime(s.dateTime, s.bookingUrl, None, Nil))
      if (showtimes.isEmpty) None
      else {
        val d = details.getOrElse(slug, MuranowClient.Detail.empty)
        Some(CinemaMovie(
          movie     = Movie(
            title          = primary.title,
            runtimeMinutes = d.runtimeMinutes,
            releaseYear    = d.year,
            countries      = d.countries,
            genres         = d.genres
          ),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = if (slug.startsWith("/")) Some(BaseUrl + slug) else None,
          synopsis  = d.synopsis,
          cast      = Seq.empty,
          director  = d.director,
          showtimes = showtimes
        ))
      }
    }
  }

  private def abs(href: String): String = if (href.startsWith("http")) href else BaseUrl + href
}

object MuranowClient {

  // Polish genitive month names as they appear in the calendar day cells.
  private val Months = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4, "maja" -> 5, "czerwca" -> 6,
    "lipca" -> 7, "sierpnia" -> 8, "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )
  private val YearPat = """\b(20\d{2})\b""".r

  def yearFromLabel(label: String): Int =
    YearPat.findFirstMatchIn(label).map(_.group(1).toInt).getOrElse(java.time.LocalDate.now().getYear)

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
      m     <- Months.get(mName)
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
    synopsis:       Option[String]
  )
  object Detail { val empty: Detail = Detail(None, None, Seq.empty, Seq.empty, Seq.empty, None) }

  private def field(doc: org.jsoup.nodes.Document, name: String): Option[String] =
    Option(doc.selectFirst(s"div.field--name-$name .field__item")).map(_.text.trim).filter(_.nonEmpty)

  private def fields(doc: org.jsoup.nodes.Document, name: String): Seq[String] =
    doc.select(s"div.field--name-$name .field__item").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)

  /** Parse the Drupal `/film/<slug>` node for the metadata fields. */
  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    Detail(
      runtimeMinutes = field(doc, "field-movie-duration").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year           = field(doc, "field-movie-production-year").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries      = fields(doc, "field-movie-production-country"),
      genres         = field(doc, "field-movie-category").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
                         .map(tools.TextNormalization.titleCaseIfAllLower),
      director       = field(doc, "field-movie-director").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      synopsis       = Option(doc.selectFirst("div.field--name-body p")).map(_.text.trim).filter(_.length > 20)
    )
  }
}
