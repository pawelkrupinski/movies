package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cinemas run by Białołęcki Ośrodek Kultury — Kino na Boku and Kino Głębocka 66
 * — share an identical site under `bok.waw.pl/<prefix>`. Parameterised by the
 * slug prefix + cinema so one client serves both; `today` supplies the year the
 * listing omits.
 *
 * The repertoire is a per-day calendar: the listing page (`/<prefix>`) shows
 * only TODAY's screenings, plus a week of day tabs
 * (`/<prefix>,ts:<epoch-seconds>`). Each day URL re-renders the listing scoped
 * to that day, with every showing film as a `a.movie-list` card carrying its
 * title and that day's times (`movieshow-list-movie-descr`). Relying on the
 * film detail pages alone — as an earlier version did — captured only the one
 * or two films whose page still carried a `movieshow-list` block, missing the
 * bulk of the week's programme. So showtimes come from the day pages; the detail
 * pages are still fetched, but only for enrichment (cast / synopsis / director /
 * runtime / poster) and to recover the biletyna booking links the day cards omit.
 */
class BokClient(http: HttpFetch, prefix: String, override val cinema: Cinema,
                today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper {

  private val BaseUrl  = "https://bok.waw.pl"
  private val Warsaw   = ZoneId.of("Europe/Warsaw")

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val listing = http.get(s"$BaseUrl/$prefix")
    val days    = dayLinks(listing)

    // Fetch every day page (today's is the listing we already have); parse the
    // showing cards on each into per-slug, per-day screenings.
    val dayHtml = listing :: days.tail.map { case (_, url) =>
      Try(http.get(url)).toOption.getOrElse("")
    }
    val showings: Seq[DayShowing] =
      days.map(_._1).zip(dayHtml).flatMap { case (date, html) => showingsOn(html, date) }

    // One CinemaMovie per slug, showtimes aggregated across the week.
    val bySlug = showings.groupBy(_.slug)
    val slugs  = bySlug.keys.toSeq.distinct

    val detailPages = ParallelDetailFetch.keyed("bok-films", slugs, 1.minute)(s => s"$BaseUrl/$prefix/$s") { url =>
      Try(http.get(url)).toOption
    }

    slugs.flatMap { slug =>
      val group = bySlug(slug)
      val detail = detailPages.getOrElse(slug, None).map(Jsoup.parse)
      buildMovie(slug, group, detail)
    }
  }

  /** The day tabs: "Dziś" → today (the bare `/<prefix>` URL), then a week of
   *  `/<prefix>,ts:<epoch-seconds>` links whose timestamp is local midnight of
   *  the day. Falls back to today-only if the calendar markup is missing. */
  private def dayLinks(listing: String): List[(LocalDate, String)] = {
    val document  = Jsoup.parse(listing)
    val tabs = document.select("div.calendar-submenu-week a.day[href]").asScala.toList
    val parsed = tabs.flatMap { a =>
      val href = a.attr("href")
      BokClient.TimestampPat.findFirstMatchIn(href) match {
        case Some(m) =>
          val date = Instant.ofEpochSecond(m.group(1).toLong).atZone(Warsaw).toLocalDate
          Some(date -> s"$BaseUrl$href")
        case None => Some(today -> s"$BaseUrl/$prefix") // the "Dziś" tab
      }
    }
    val withToday = if (parsed.exists(_._1 == today)) parsed else (today -> s"$BaseUrl/$prefix") :: parsed
    withToday.distinctBy(_._1)
  }

  /** Films actually screening on `date`: every `a.movie-list` card that carries
   *  at least one time. Upcoming-only ("Zapowiedzi") cards have no times and are
   *  dropped, so they don't masquerade as scheduled screenings. */
  private def showingsOn(html: String, date: LocalDate): Seq[DayShowing] = {
    if (html.isEmpty) return Seq.empty
    Jsoup.parse(html).select("a.movie-list[href]").asScala.toSeq.flatMap { card =>
      val href  = card.attr("href")
      val slug  = href.stripPrefix(s"/$prefix/").stripPrefix("/")
      val rawTitle = Option(card.selectFirst("div.fs-30.fw-black")).map(_.text)
      val title = rawTitle.map(BokClient.cleanTitle).filter(_.nonEmpty)
      val times = card.select("span.movieshow-list-movie-descr").asScala.toSeq
        .flatMap(s => ScraperParse.parseHHmm(s.text.trim))
      title.filter(_ => times.nonEmpty && slug.matches("[a-z0-9-]+"))
        .map(t => DayShowing(slug, t, rawTitle.getOrElse(t), times.map(date.atTime)))
    }
  }

  private def buildMovie(slug: String, showings: Seq[DayShowing], detail: Option[Document]): Option[CinemaMovie] = {
    val title    = showings.map(_.title).headOption.filter(_.nonEmpty)
    val rawTitle = showings.map(_.rawTitle).headOption
    // Biletyna booking links live on the detail page's `movieshow-list` blocks,
    // keyed by their date+time — match each day-page screening to its link.
    val bookingByTime: Map[LocalDateTime, String] = detail.toSeq.flatMap(detailBookings).toMap
    val showtimes = showings.flatMap(_.dateTimes).distinct.sorted
      .map(dt => Showtime(dt, bookingByTime.get(dt)))

    title.filter(_ => showtimes.nonEmpty).map { t =>
      val director  = detail.flatMap(d => metaRow(d, "reżyseria")).toSeq.flatMap(splitCsv)
      val countries = detail.flatMap(d => metaRow(d, "produkcja")).toSeq.flatMap(splitCsv)
      val cast      = detail.flatMap(d => metaRow(d, "obsada")).toSeq.flatMap(splitCsv)
      val runtime   = detail.flatMap(d => metaRow(d, "czas trwania"))
        .flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt))
      CinemaMovie(
        movie     = Movie(title = t, runtimeMinutes = runtime, releaseYear = None, countries = countries, rawTitle = rawTitle),
        cinema    = cinema,
        posterUrl = detail.flatMap(d => Option(d.selectFirst("div.item-image-thumb img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                      .orElse(Option(d.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty))),
        filmUrl   = Some(s"$BaseUrl/$prefix/$slug"),
        synopsis  = detail.flatMap(d => Option(d.selectFirst("meta[name=description]")).map(_.attr("content").trim).filter(_.length > 20)),
        cast      = cast,
        director  = director,
        showtimes = showtimes
      )
    }
  }

  /** Date+time → biletyna URL from a detail page's `movieshow-list` blocks. */
  private def detailBookings(document: Document): Seq[(LocalDateTime, String)] =
    document.select("div.movieshow-list.p-relative").asScala.toSeq.flatMap { block =>
      val date = Option(block.selectFirst("div.fs-24.fw-black")).map(_.text.trim)
      val time = Option(block.selectFirst("div.fs-16.fw-black")).map(_.text.trim)
      for {
        d <- date.flatMap(BokClient.parseDate(_, today))
        t <- time.flatMap(ScraperParse.parseHHmm)
        b <- Option(block.selectFirst("a[href*=biletyna]")).map(_.attr("href")).filter(_.nonEmpty)
      } yield d.atTime(t) -> b
    }

  private def splitCsv(s: String): Seq[String] = s.split(",").map(_.trim).filter(_.nonEmpty).toSeq

  private def metaRow(document: Document, label: String): Option[String] =
    document.select("div.meta-row").asScala.find(_.text.toLowerCase.contains(label))
      .flatMap(r => Option(r.selectFirst("div.body"))).map(_.text.trim).filter(_.nonEmpty)

  private case class DayShowing(slug: String, title: String, rawTitle: String, dateTimes: Seq[LocalDateTime])
}

object BokClient {
  private val DatePat = """(\d{1,2})\.(\d{1,2})""".r
  private val TimestampPat   = """ts:(\d+)""".r
  // BoK piles two kinds of `|`-delimited decoration onto a title:
  //  - a trailing ALL-CAPS promo tag ("Drzewo magii | PREMIERA"), and
  //  - a leading recurring-programme banner ("Kino dla Seniora | Tajny agent",
  //    "Kinowy Poranek | …"), which is a distinct screening of its own and
  //    must stay attached so two different senior-club films don't collapse
  //    onto the bare banner.
  // Drop only the trailing promo tag; rewrite the remaining `|` separators to
  // a readable "Banner: Film" so the programme banner survives as a prefix.
  // This cleanup now lives in the editable "bok" rules (TitleRules); both
  // BoK venues share the "bok" key (see TitleRuleKey).
  /** Clean a BoK card/h2 title: drop the trailing ALL-CAPS promo tag, keep any
   *  recurring-programme banner, and normalise whitespace. */
  def cleanTitle(raw: String): String =
    services.movies.TitleNormalizer.cinemaClean("bok", raw)

  /** "05.06" (DD.MM, no year) → a date; year from `today`, rolling forward when
   *  the month is already behind us. */
  def parseDate(raw: String, today: LocalDate): Option[LocalDate] =
    DatePat.findFirstMatchIn(raw).flatMap { m =>
      val mon  = m.group(2).toInt
      val year = if (mon < today.getMonthValue) today.getYear + 1 else today.getYear
      Try(LocalDate.of(year, mon, m.group(1).toInt)).toOption
    }
}
