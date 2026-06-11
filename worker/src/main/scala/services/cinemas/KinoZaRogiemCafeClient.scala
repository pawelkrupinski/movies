package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino za Rogiem Cafe (Rzeszów) — a small independent cinema running a custom
 * WordPress theme at `kzrcafe.pl`. The repertoire page (`/repertuar/`) renders
 * ALL upcoming screenings in one static response with no per-day pagination.
 *
 * Each screening is an `article.news-v9` card inside the film-post grid
 * (`section.blog-classic-v6`). The card contains:
 *  - A blue `div.ffb-html-1` with a date/time label in one of three formats:
 *      - "Dzisiaj, HH:MM"     → today
 *      - "Jutro, HH:MM"       → tomorrow
 *      - "DD miesiąca YYYY, HH:MM"  → e.g. "09 czerwca 2026, 17:00"
 *  - `h2.news-v9-title a[href=/film/<slug>/]` → film title and slug.
 *
 * Booking is a reservation form embedded on the film detail page; there is no
 * online ticketing system, so `bookingUrl` is always `None`. The film URL
 * (`filmUrl`) is set to the cinema's own `/film/<slug>/` page.
 *
 * Films with multiple upcoming screenings each produce multiple cards, so we
 * aggregate by film slug (the last path segment of the `/film/<slug>/` URL).
 */
class KinoZaRogiemCafeClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoZaRogiemCafeClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val doc  = Jsoup.parse(html)
    val slots = parseDoc(doc, today)

    val bySlug = slots.groupBy(_.filmSlug)
    bySlug.toSeq.flatMap { case (_, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, bookingUrl = None))
        .distinctBy(_.dateTime)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie     = Movie(head.title),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = Some(BaseUrl + "/film/" + head.filmSlug + "/"),
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }
}

object KinoZaRogiemCafeClient {

  val BaseUrl       = "https://kzrcafe.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  // "09 czerwca 2026, 17:00"
  private val FullDatePat =
    """(\d{1,2})\s+([\p{L}]+)\s+(\d{4}),\s*(\d{1,2}:\d{2})""".r

  // "Dzisiaj, 11:00" or "Jutro, 17:30"
  private val RelativeDatePat = """(Dzisiaj|Jutro),\s*(\d{1,2}:\d{2})""".r

  // /film/<slug>/ URL
  private val SlugPat = """/film/([^/]+)/""".r

  // The blue label sits just after the last <span> closing the background overlay.
  // CSS colour "#2e3192" is the distinctive blue box.  (?s) enables DOTALL so
  // the `.*?` bridges any whitespace/newlines between the property and the tags.
  private val BlueLabelPat =
    """(?s)background-color: #2e3192.*?</span></span>([^<]+)""".r

  private[cinemas] case class RawSlot(
    title:    String,
    filmSlug: String,
    dateTime: LocalDateTime
  )

  private[cinemas] def parseDoc(doc: Document, today: LocalDate): Seq[RawSlot] = {
    // The film-post grid is the section containing blog-classic-v6 posts of
    // type "film". Each wrapping div has class "post-wrapper … film …".
    doc.select("div.post-wrapper.film").asScala.toSeq.flatMap { wrapper =>
      val article = Option(wrapper.selectFirst("article.news-v9"))

      val dateTime: Option[LocalDateTime] = article.flatMap { a =>
        // The date/time label is encoded in inline style — cheapest extraction is
        // from the raw outerHtml of the article's own HTML snippet.
        val raw = a.outerHtml
        BlueLabelPat.findFirstMatchIn(raw)
          .map(_.group(1).trim)
          .flatMap(parseLabel(_, today))
      }

      val titleAnchor = article.flatMap(a => Option(a.selectFirst("h2.news-v9-title a[href]")))
      val title       = titleAnchor.map(_.text.trim).filter(_.nonEmpty)
      val filmSlug    = titleAnchor
        .flatMap(a => SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1)))

      for {
        dt   <- dateTime
        t    <- title
        slug <- filmSlug
      } yield RawSlot(t, slug, dt)
    }
  }

  /** Parse the label text from the blue box into a `LocalDateTime`. */
  private[cinemas] def parseLabel(label: String, today: LocalDate): Option[LocalDateTime] = {
    // "Dzisiaj, HH:MM" / "Jutro, HH:MM"
    RelativeDatePat.findFirstMatchIn(label).flatMap { m =>
      val base = if (m.group(1) == "Dzisiaj") today else today.plusDays(1)
      ScraperParse.parseHHmm(m.group(2)).map(base.atTime)
    }.orElse {
      // "09 czerwca 2026, 17:00"
      FullDatePat.findFirstMatchIn(label).flatMap { m =>
        for {
          month <- ScraperParse.PolishMonths.get(m.group(2).toLowerCase)
          day    = m.group(1).toInt
          year   = m.group(3).toInt
          date  <- Try(LocalDate.of(year, month, day)).toOption
          time  <- ScraperParse.parseHHmm(m.group(4))
        } yield LocalDateTime.of(date, time)
      }
    }
  }
}
