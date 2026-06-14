package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{CachingDetailFetch, HttpFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KINOMUZEUM (Muzeum Sztuki Nowoczesnej, Warszawa). The `/pl/repertuar` page is
 * server-rendered as day sections (a header carrying "… DD mmm") followed by a
 * card per screening (title + `/wydarzenia/<slug>` link, time, runtime, poster,
 * booking). Per-film detail pages add year / countries / director / synopsis.
 * Section headers omit the year, so `today` supplies it.
 */
class KinomuzeumClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper with DetailEnricher {

  // Static detail pages cached across passes; the listing keeps the live `http`
  // since its showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = Kinomuzeum

  private val BaseUrl    = "https://artmuseum.pl"
  private val ListingUrl = s"$BaseUrl/pl/repertuar"
  private val SlugPat    = """/wydarzenia/([a-z0-9-]+)""".r

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, runtime: Option[Int],
                             booking: Option[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = fetchBare()

  private def fetchBare(): Seq[CinemaMovie] = {
    val document = Jsoup.parse(http.get(ListingUrl))

    var date: Option[LocalDate] = None
    val slots = document.select("div.section_title_small, h3.h4").asScala.toSeq.flatMap { element =>
      if (element.hasClass("section_title_small")) { date = KinomuzeumClient.parseDate(element.text, today); Seq.empty }
      else date.toSeq.flatMap(d => cardSlot(element, d))
    }

    // A film can recur under several slugs (a plain screening + a "+ spotkanie"
    // event), all with the same title — group by title so each is one row.
    val byTitle = slots.groupBy(_.title)
    byTitle.toSeq.flatMap { case (_, group) =>
      val primary   = group.head
      val slug      = primary.slug
      val showtimes = group.distinctBy(s => (s.dateTime, s.booking)).sortBy(_.dateTime)
                        .map(s => Showtime(s.dateTime, s.booking, None, Nil))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(
          title          = primary.title,
          runtimeMinutes = primary.runtime
        ),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/wydarzenia/$slug") else None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }
  }

  override val detailGroup: String = "kinomuzeum"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://artmuseum.pl/wydarzenia/<slug>`). None on
   *  fetch failure so the task stays stale and is retried rather than recording
   *  an empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val d = KinomuzeumClient.parseDetail(html)
      FilmDetail(
        synopsis       = d.synopsis,
        cast           = Seq.empty,
        director       = d.director,
        runtimeMinutes = d.runtime,
        releaseYear    = d.year,
        countries      = d.countries,
        posterUrl      = d.poster
      )
    }

  private def cardSlot(titleElement: Element, date: LocalDate): Option[RawSlot] = {
    val link  = Option(titleElement.selectFirst("a[href*=/wydarzenia/]"))
    val card  = titleElement.parents().asScala.find(p => p.selectFirst("p.allcaps") != null)
    for {
      a    <- link
      slug <- SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1))
      c    <- card
      spans = c.select("p.allcaps span").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)
      time <- spans.find(s => """^\d{1,2}:\d{2}$""".r.matches(s))
      dt   <- KinomuzeumClient.parseTime(date, time)
    } yield {
      val title   = a.text.trim
      val runtime = spans.flatMap(s => """(\d+)\s*min""".r.findFirstMatchIn(s).map(_.group(1).toInt)).headOption
      val booking = Option(c.selectFirst("a[href*=nienumerowane]")).map(_.attr("href")).filter(_.nonEmpty)
      val poster  = Option(c.selectFirst("img[width=320]")).orElse(Option(c.selectFirst("img[src]")))
                      .map(_.attr("src")).filter(_.nonEmpty)
      RawSlot(slug, title, dt, runtime, booking, poster)
    }
  }
}

object KinomuzeumClient {

  private val Months = Map("sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12)
  private val DayMonthPat = """(\d{1,2})\s+([a-ząćęłńóśźż]{3})""".r

  def parseDate(raw: String, today: LocalDate): Option[LocalDate] =
    DayMonthPat.findFirstMatchIn(raw.toLowerCase).flatMap { m =>
      Months.get(m.group(2)).flatMap { mon =>
        val year = if (mon < today.getMonthValue) today.getYear + 1 else today.getYear
        Try(LocalDate.of(year, mon, m.group(1).toInt)).toOption
      }
    }

  def parseTime(date: LocalDate, time: String): Option[LocalDateTime] =
    """(\d{1,2}):(\d{2})""".r.findFirstMatchIn(time).flatMap(m => Try(date.atTime(m.group(1).toInt, m.group(2).toInt)).toOption)

  final case class Detail(runtime: Option[Int], year: Option[Int], countries: Seq[String],
                          director: Seq[String], synopsis: Option[String], poster: Option[String])
  object Detail { val empty: Detail = Detail(None, None, Seq.empty, Seq.empty, None, None) }

  private def meta(document: org.jsoup.nodes.Document, label: String): Option[String] =
    document.select("div.meta-data").asScala.find(_.text.toLowerCase.contains(label))
      .flatMap(d => Option(d.selectFirst(".description"))).map(_.text.trim).filter(_.nonEmpty)

  def parseDetail(html: String): Detail = {
    val document = Jsoup.parse(html)
    Detail(
      runtime   = meta(document, "czas trwania").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year      = meta(document, "rok produkcji").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries = meta(document, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      director  = meta(document, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      synopsis  = Option(document.selectFirst("div.block-element.text .description")).map(_.text.trim)
                    .orElse(Option(document.selectFirst("meta[property=og:description]")).map(_.attr("content").trim))
                    .filter(_.length > 20),
      poster    = Option(document.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty)
    )
  }
}
