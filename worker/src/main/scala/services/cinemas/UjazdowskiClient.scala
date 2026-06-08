package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{Instant, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino U-jazdowski (CSW Zamek Ujazdowski, Warszawa). The repertoire page lists
 * the day timestamps (`ut`, midnight epoch seconds) in its nav; each day's
 * `week.ajax?ut=N` returns that day's screening cards (title, time, a
 * director/country/year/runtime meta line, poster) linking to the film page.
 * The per-film page adds the synopsis. The date comes from the `ut`, so the
 * replay is deterministic. Booking is the film page (no stable deep-link).
 */
class UjazdowskiClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = Ujazdowski

  private val BaseUrl    = "https://u-jazdowski.pl"
  private val ListingUrl = s"$BaseUrl/kino/repertuar"
  private val UtPat      = """ut=(\d+)""".r
  private val SlugPat    = """/kino/repertuar/([a-z0-9-]+)""".r
  private val WarsawZone = ZoneId.of("Europe/Warsaw")

  private case class RawSlot(slug: String, title: String, dateTime: LocalDateTime, meta: Option[String], poster: Option[String])

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val main = http.get(ListingUrl)
    val uts  = UtPat.findAllMatchIn(main).map(_.group(1)).toSeq.distinct

    val dayPages = ParallelDetailFetch.keyed("ujazdowski-days", uts, 1.minute, maxConcurrent = 1)(ut => s"$ListingUrl/week.ajax?ut=$ut") { url =>
      Try(http.get(url)).toOption
    }
    val slots = uts.flatMap { ut =>
      val date = Try(Instant.ofEpochSecond(ut.toLong).atZone(WarsawZone).toLocalDate).toOption
      date.toSeq.flatMap(d => dayPages.getOrElse(ut, None).toSeq.flatMap(html => parseDay(html, d)))
    }

    val bySlug  = slots.groupBy(_.slug)
    val details = ParallelDetailFetch.keyed("ujazdowski-details", bySlug.keys.toSeq.filter(_.nonEmpty), 1.minute)(s => s"$BaseUrl/kino/repertuar/$s") { url =>
      Try(http.get(url)).toOption.map(Jsoup.parse)
    }

    bySlug.toSeq.flatMap { case (slug, group) =>
      val primary    = group.head
      val detailUrl  = s"$BaseUrl/kino/repertuar/$slug"
      val showtimes  = group.map(s => Showtime(s.dateTime, Some(detailUrl), None, Nil))
                         .distinctBy(_.dateTime).sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val meta = UjazdowskiClient.parseMeta(primary.meta.getOrElse(""))
        val detail   = details.getOrElse(slug, None)
        val synopsis = detail.flatMap(d => Option(d.selectFirst("div.body.max-w"))).map(_.text.trim).filter(_.length > 20)
        val origTitle = detail.flatMap(UjazdowskiClient.originalTitleOf)
        Some(CinemaMovie(
          movie     = Movie(title = primary.title, runtimeMinutes = meta.runtime, releaseYear = meta.year,
                            countries = meta.countries, originalTitle = origTitle),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = Some(detailUrl),
          synopsis  = synopsis,
          cast      = Seq.empty,
          director  = meta.director,
          showtimes = showtimes
        ))
      }
    }
  }

  private def parseDay(html: String, date: java.time.LocalDate): Seq[RawSlot] =
    Jsoup.parse(html).select("a.event-list-day-box").asScala.toSeq.flatMap { a =>
      val slug  = SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1))
      val title = Option(a.selectFirst(".title em")).orElse(Option(a.selectFirst(".title"))).map(_.text.trim).filter(_.nonEmpty)
      val time  = Option(a.selectFirst(".hours")).map(_.text.trim)
                    .flatMap(ScraperParse.parseHHmm)
      for { s <- slug; t <- title; tm <- time } yield {
        val meta   = Option(a.selectFirst(".fs-20.max-w")).map(_.text.trim).filter(_.nonEmpty)
        val poster = Option(a.selectFirst("img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                       .map(u => if (u.startsWith("http")) u else BaseUrl + u)
        RawSlot(s, t, date.atTime(tm), meta, poster)
      }
    }
}

object UjazdowskiClient {

  // "[Original Title], reż. Director, Country1/ Country2 YEAR, RUNTIME'"
  private val MetaPat = """reż\.\s*(.+?),\s*(.+?)\s+((?:19|20)\d{2}),\s*(\d+)['’]""".r
  // The film page's header renders that meta with the original title bracketed
  // (`<i class="finterp">[</i><em>…</em>…`), so jsoup's text reads "[Orig], …".
  // Polish films omit the bracket, so this is `None` for them.
  private val OrigTitlePat = """^\s*\[(.+?)\]""".r

  /** The bracketed original title from the film-page header meta, e.g.
   *  "[Da hong deng long gao gao gua], reż. …" → "Da hong deng long gao gao gua". */
  def originalTitleOf(doc: org.jsoup.nodes.Document): Option[String] =
    Option(doc.selectFirst("div.event-content-header div.fs-20.max-w")).map(_.text.trim)
      .flatMap(s => OrigTitlePat.findFirstMatchIn(s).map(_.group(1).trim)).filter(_.nonEmpty)

  final case class Meta(director: Seq[String], countries: Seq[String], year: Option[Int], runtime: Option[Int])

  def parseMeta(s: String): Meta =
    MetaPat.findFirstMatchIn(s) match {
      case Some(m) => Meta(
        director  = m.group(1).split(",").map(_.trim).filter(_.nonEmpty).toSeq,
        countries = m.group(2).split("[,/]").map(_.trim).filter(_.nonEmpty).toSeq,
        year      = Some(m.group(3).toInt),
        runtime   = Some(m.group(4).toInt)
      )
      case None => Meta(Seq.empty, Seq.empty, None, None)
    }
}
