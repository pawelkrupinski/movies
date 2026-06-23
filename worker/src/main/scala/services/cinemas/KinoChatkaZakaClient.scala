package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Chatka Żaka — the cinema of the UMCS student-culture centre (Akademickie
 * Centrum Kultury i Mediów "Chatka Żaka", Lublin). It has no standalone
 * repertoire page; its film screenings live in the venue's event calendar
 * (`umcs.pl/pl/kalendarz-wydarzen,9469,…`) alongside the centre's concerts,
 * theatre, kabaret and student events — so this client mixes in
 * [[OnlyMovieEventsFilter]] to drop the non-film rows by title.
 *
 * The calendar is two-layered, server-rendered, no JSON-LD:
 *   - the LIST page groups events under `<h3 class="header-light">DAY, DD.MM.YYYY</h3>`
 *     date headers, one `<div class="box-row">` per event carrying the full
 *     descriptive title in `<h4 class="header">` and a `Więcej` link to the
 *     detail page. The list has the full-year date but NO time.
 *   - each DETAIL page (`…,<slug>,<id>.chtm`) carries the screening's wall-clock
 *     time, director, country, year and runtime in its `<meta name="description">`
 *     (`"DD.MM.YY | day | HH:MM\nTITLE (Original)\nreż. Director, Country Year (NN min)"`).
 *
 * So the list gives the date + film identity, the detail gives the time + the
 * cinema metadata. The descriptive list title is `TITLE (Original) reż. Director
 * | NN. Programme banner`; [[KinoChatkaZakaClient.cleanTitle]] reduces it to the
 * bare Polish title (sentence-cased), lifting the original title out.
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue.
 */
class KinoChatkaZakaClient(http: HttpFetch, override val cinema: Cinema = KinoChatkaZaka)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoChatkaZakaClient.CalendarUrl)
  override def sourceUrl: Option[String] = Some(KinoChatkaZakaClient.CalendarUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val pairs = KinoChatkaZakaClient.parseList(http.get(KinoChatkaZakaClient.CalendarUrl)).flatMap { entry =>
      KinoChatkaZakaClient.parseDetail(http.get(entry.detailUrl)).map(entry -> _)
    }
    KinoChatkaZakaClient.fold(cinema, pairs)
  }
}

object KinoChatkaZakaClient {

  val BaseUrl     = "https://www.umcs.pl"
  // Calendar id 9469 = the Chatka Żaka (ACKiM UMCS) event calendar; `,1.lhtm`
  // is its first/only list page (the venue currently runs a single upcoming
  // programme at a time, so the page number is not load-bearing pagination).
  val CalendarUrl = s"$BaseUrl/pl/kalendarz-wydarzen,9469,1.lhtm"

  /** One event row off the calendar LIST page: its date (read off the section
   *  header), the descriptive title and the detail-page URL. The time is not on
   *  the list — it comes from the detail (see [[Detail]]). */
  private case class ListEntry(date: java.time.LocalDate, rawTitle: String, detailUrl: String)

  /** The per-screening facts the DETAIL page adds to a [[ListEntry]]: the
   *  wall-clock time and the cinema metadata pulled from its meta description. */
  private case class Detail(time: java.time.LocalTime, director: Seq[String], countries: Seq[String], year: Option[Int], runtime: Option[Int])

  // "23.06.2026" off the `<h3>` date header — full year, so no inference.
  private val ListDatePat = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
  // The detail meta description's leading "… | HH:MM" stamp.
  private val DetailTimePat = """\|\s*(\d{1,2}):(\d{2})""".r
  // The detail meta's "reż. Director, Country[/Country…] Year (NN min)" line.
  private val DetailMetaPat = """(?s)reż\.\s*(.+?),\s*([^()]+?)\s+(\d{4})\s*\((\d+)\s*min\)""".r
  // A trailing "(Original title)" segment on the descriptive list title.
  private val OriginalTitlePat = """^(.*?)\s*\(([^)]+)\)\s*$""".r

  private def parseList(html: String): Seq[ListEntry] = {
    val document = Jsoup.parse(html, BaseUrl)

    // The list is a flat sibling stream: a `<h3>` date header, then the
    // `.box-row` events under it, then the next header. Walk the whole list
    // container's children in order, carrying the most recent date forward.
    var currentDate = Option.empty[java.time.LocalDate]
    document.select("h3.header-light, div.box-row").asScala.toSeq.flatMap { el =>
      if (el.tagName == "h3") {
        currentDate = ListDatePat.findFirstMatchIn(el.text)
          .flatMap(m => Try(java.time.LocalDate.of(m.group(3).toInt, m.group(2).toInt, m.group(1).toInt)).toOption)
        None
      } else
        for {
          date  <- currentDate
          title <- Option(el.selectFirst("h4.header")).map(_.text.trim).filter(_.nonEmpty)
          link  <- Option(el.selectFirst("div.link a[href]")).map(_.attr("abs:href")).filter(_.nonEmpty)
        } yield ListEntry(date, title, link)
    }
  }

  private def parseDetail(html: String): Option[Detail] = {
    val description = Option(Jsoup.parse(html).selectFirst("meta[name=description]")).map(_.attr("content")).getOrElse("")
    DetailTimePat.findFirstMatchIn(description).flatMap { t =>
      Try(java.time.LocalTime.of(t.group(1).toInt, t.group(2).toInt)).toOption.map { time =>
        val meta = DetailMetaPat.findFirstMatchIn(description)
        Detail(
          time      = time,
          director  = meta.map(m => splitList(m.group(1))).getOrElse(Seq.empty),
          countries = meta.map(m => m.group(2).split('/').map(_.trim).filter(_.nonEmpty).toSeq).getOrElse(Seq.empty),
          year      = meta.map(_.group(3).toInt),
          runtime   = meta.map(_.group(4).toInt)
        )
      }
    }
  }

  /** Fold (list entry, detail) pairs into title-sorted [[CinemaMovie]]s, one per
   *  cleaned title. The descriptive list title is the verbatim source title
   *  ([[Movie.rawTitle]]); [[cleanTitle]] reduces it to the display title. */
  private def fold(cinema: Cinema, pairs: Seq[(ListEntry, Detail)]): Seq[CinemaMovie] =
    SlotsToMovies.fold(
      pairs,
      (pair: (ListEntry, Detail)) => cleanTitle(pair._1.rawTitle).title,
      (pair: (ListEntry, Detail)) => Showtime(LocalDateTime.of(pair._1.date, pair._2.time), Some(pair._1.detailUrl))
    ) { (_, group, showtimes) =>
      val (entry, detail) = group.head
      val parsed          = cleanTitle(entry.rawTitle)
      CinemaMovie(
        movie     = Movie(
          title          = parsed.title,
          runtimeMinutes = detail.runtime,
          releaseYear    = detail.year,
          countries      = detail.countries,
          originalTitle  = parsed.original,
          rawTitle       = Some(entry.rawTitle)
        ),
        cinema    = cinema,
        posterUrl = None, // the calendar exposes only the venue logo, no film poster
        filmUrl   = Some(entry.detailUrl),
        synopsis  = None,
        cast      = Seq.empty,
        director  = detail.director,
        showtimes = showtimes
      )
    }

  private case class CleanTitle(title: String, original: Option[String])

  /** Reduce the descriptive calendar title to the bare Polish display title.
   *  The source form is `"TITLE (Original) reż. Director | NN. Programme banner"`
   *  (some segments optional): drop the ` | …` programme banner, drop the
   *  trailing `reż. …` directors, lift a trailing `(Original)` into its own
   *  field, then sentence-case the ALL-CAPS title the calendar publishes. */
  private def cleanTitle(raw: String): CleanTitle = {
    val withoutBanner   = raw.split('|').head.trim
    val withoutDirector = withoutBanner.replaceAll("""(?i)\s*reż\..*$""", "").trim
    val (title, original) = withoutDirector match {
      case OriginalTitlePat(t, orig) => (t.trim, Some(orig.trim))
      case t                         => (t, None)
    }
    CleanTitle(ScraperParse.sentenceCase(title), original.map(_.trim).filter(_.nonEmpty))
  }

  private def splitList(s: String): Seq[String] =
    s.split(',').map(_.trim).filter(_.nonEmpty).toSeq
}
