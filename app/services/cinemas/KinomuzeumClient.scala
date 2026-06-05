package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KINOMUZEUM (Muzeum Sztuki Nowoczesnej, Warszawa). The `/pl/repertuar` page is
 * server-rendered as day sections (a header carrying "… DD mmm") followed by a
 * card per screening (title + `/wydarzenia/<slug>` link, time, runtime, poster,
 * booking). Per-film detail pages add year / countries / director / synopsis.
 * Section headers omit the year, so `today` supplies it.
 */
class KinomuzeumClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper {

  val cinema: Cinema = Kinomuzeum

  private val BaseUrl    = "https://artmuseum.pl"
  private val ListingUrl = s"$BaseUrl/pl/repertuar"
  private val SlugPat    = """/wydarzenia/([a-z0-9-]+)""".r

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, runtime: Option[Int],
                             booking: Option[String], poster: Option[String])

  def fetch(): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(http.get(ListingUrl))

    var date: Option[LocalDate] = None
    val slots = doc.select("div.section_title_small, h3.h4").asScala.toSeq.flatMap { el =>
      if (el.hasClass("section_title_small")) { date = KinomuzeumClient.parseDate(el.text, today); Seq.empty }
      else date.toSeq.flatMap(d => cardSlot(el, d))
    }

    // A film can recur under several slugs (a plain screening + a "+ spotkanie"
    // event), all with the same title — group by title so each is one row.
    val byTitle = slots.groupBy(_.title)
    val details = ParallelDetailFetch.keyed("kinomuzeum-details", byTitle.values.map(_.head.slug).toSeq.distinct.filter(_.nonEmpty), 1.minute)(s => s"$BaseUrl/wydarzenia/$s") { url =>
      Try(http.get(url)).toOption.map(KinomuzeumClient.parseDetail).getOrElse(KinomuzeumClient.Detail.empty)
    }

    byTitle.toSeq.flatMap { case (title, group) =>
      val primary    = group.head
      val slug       = primary.slug
      val showtimes  = group.distinctBy(s => (s.dateTime, s.booking)).sortBy(_.dateTime)
                         .map(s => Showtime(s.dateTime, s.booking, None, Nil))
      if (showtimes.isEmpty) None
      else {
        val d = details.getOrElse(slug, KinomuzeumClient.Detail.empty)
        Some(CinemaMovie(
          movie     = Movie(
            title          = primary.title,
            runtimeMinutes = primary.runtime.orElse(d.runtime),
            releaseYear    = d.year,
            countries      = d.countries
          ),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption.orElse(d.poster),
          filmUrl   = if (slug.nonEmpty) Some(s"$BaseUrl/wydarzenia/$slug") else None,
          synopsis  = d.synopsis,
          cast      = Seq.empty,
          director  = d.director,
          showtimes = showtimes
        ))
      }
    }
  }

  private def cardSlot(titleEl: Element, date: LocalDate): Option[RawSlot] = {
    val link  = Option(titleEl.selectFirst("a[href*=/wydarzenia/]"))
    val card  = titleEl.parents().asScala.find(p => p.selectFirst("p.allcaps") != null)
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

  private def meta(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    doc.select("div.meta-data").asScala.find(_.text.toLowerCase.contains(label))
      .flatMap(d => Option(d.selectFirst(".description"))).map(_.text.trim).filter(_.nonEmpty)

  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    Detail(
      runtime   = meta(doc, "czas trwania").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year      = meta(doc, "rok produkcji").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries = meta(doc, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      director  = meta(doc, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      synopsis  = Option(doc.selectFirst("div.block-element.text .description")).map(_.text.trim)
                    .orElse(Option(doc.selectFirst("meta[property=og:description]")).map(_.attr("content").trim))
                    .filter(_.length > 20),
      poster    = Option(doc.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty)
    )
  }
}
