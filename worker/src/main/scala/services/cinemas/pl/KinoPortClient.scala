package services.cinemas.pl

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import models._
import play.api.libs.json.{JsValue, Json}
import tools.HttpFetch
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * KinoPort — the art-house screen run by GCSW (Gdańskie Centrum Sztuki
 * Współczesnej, formerly CSW Łaźnia) in Gdańsk.
 *
 * Previously scraped from Filmweb (cinemaId 1735), which went silently empty —
 * `/api/v1/cinema/1735/seances?date=…` returns `[]` for every date even though
 * `/info` still names the venue correctly — so the scrape recorded a white
 * "0 showtimes" bar while the venue was screening five films a day. The venue's
 * own old site (`laznia.pl/kinoport/repertuar/`) is now just a pointer: "Obecnie
 * informacje nt. kina i bieżącego repertuaru znajdziecie na: www.gcsw.pl".
 *
 * gcsw.pl is WordPress + Elementor and publishes the whole repertoire as ONE
 * post under the `kino` category. The post's dated permalink rotates whenever a
 * new repertoire is published, so we read it through the stable WP REST route
 * instead: `/wp-json/wp/v2/posts?categories=49` returns the current post(s) with
 * the rendered HTML in `content.rendered`.
 *
 * Inside that HTML the programme is a flat run of headings and paragraphs:
 *
 *   `<h3>…<strong>Lipiec 2026</strong></h3>`   month header, year OPTIONAL
 *                                              (the next month reads just
 *                                              "Sierpień" — the year carries over)
 *   `<h4>30.07 (czwartek)</h4>`                one day, `DD.MM`
 *   `<p><strong>18:00 – Arek. Mama. Panorama</strong> (72′)<br>
 *       <em>2026, reż. Mikołaj Janik</em><br><em>Pokaz przedpremierowy</em></p>`
 *                                              one screening
 *
 * So `<h4>` gives the day and `<strong>` the time — the month header is only
 * consulted for the YEAR. Runtime (`(72′)`, U+2032 PRIME), release year and
 * director come off the same paragraph.
 *
 * **The archive trap.** The same post continues, below the live programme, with
 * an Elementor accordion titled "ARCHIWALNE SEANSE" holding identically-shaped
 * PAST listings (and using a second, long-form day format — "2 lipca
 * (czwartek)"). Parsing it would resurrect months of finished screenings, so
 * every `<details>` under that accordion is dropped before parsing — structural,
 * not a text offset.
 *
 * Tickets are sold at the box office only ("Bilety … dostępne w sprzedaży
 * bezpośrednio przed pokazem w kasie kina"), so no screening carries a booking
 * deep-link; the repertoire post itself is the film URL.
 */
class KinoPortClient(
  http:  HttpFetch,
  override val cinema: Cinema,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoPortClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(ProgrammePageUrl)

  def fetch(): Seq[CinemaMovie] = {
    val slots = Json.parse(http.get(ProgrammeApiUrl)).as[Seq[JsValue]].flatMap { post =>
      for {
        rendered <- (post \ "content" \ "rendered").asOpt[String].toSeq
        link     <- (post \ "link").asOpt[String].toSeq
        slot     <- parseProgramme(rendered, link, today)
      } yield slot
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, group, showtimes) =>
      val first = group.minBy(_.dateTime)
      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = group.flatMap(_.runtimeMinutes).headOption,
          releaseYear    = group.flatMap(_.releaseYear).headOption
        ),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = Some(first.postUrl),
        synopsis  = None,
        cast      = Seq.empty,
        director  = first.director,
        showtimes = showtimes
      )
    }
  }
}

object KinoPortClient {

  val BaseUrl          = "https://gcsw.pl"
  // The `kino` category (id 49) holds exactly the current repertoire post. The
  // human-facing permalink is dated and rotates per repertoire, so the category
  // query — not that URL — is what we fetch.
  val ProgrammeApiUrl  = s"$BaseUrl/wp-json/wp/v2/posts?categories=49"
  // GCSW has no stable cinema landing page: `/kinoport/` and `/category/kino/`
  // both 404, and `/repertuar/` 302s to whichever repertoire post was current
  // when the alias was last pointed (today: a June one). The homepage does carry
  // a link to the live repertoire post, so it is the stable human-facing page.
  val ProgrammePageUrl = s"$BaseUrl/"

  /** `30.07 (czwartek)` — the live section's day header. The archive's long-form
   *  `2 lipca (czwartek)` deliberately does NOT match: those blocks are dropped
   *  wholesale below, and an unparseable heading must not inherit the previous
   *  day. */
  private val DayPat = """(\d{1,2})\.(\d{1,2})\b.*""".r
  /** `18:00 – Arek. Mama. Panorama` — time and title, read off the paragraph's
   *  `<strong>` text joined (some screenings split "17:00 – Tony" across
   *  three sibling `<strong>` tags instead of one), split on the en dash
   *  (U+2013) or a plain hyphen. */
  private val TimeTitlePat = """^(\d{1,2}):(\d{2})\s*[–-]\s*(.+)$""".r
  /** ` (72′)` — runtime in minutes, U+2032 PRIME, immediately after the strong. */
  private val RuntimePat = """\((\d{1,3})′\)""".r
  /** `2026, reż. Mikołaj Janik` / `2025` — the caption line, whether wrapped in
   *  `<em>` (older posts) or plain text after a `<br>` (September 2026 on). */
  private val YearPat     = """^(\d{4})\b""".r
  /** `reż. Mikołaj Janik` — the explicit-credit form. */
  private val DirectorPat = """reż\.\s*(.+?)\s*$""".r
  /** `1962, Orson Welles` — September 2026's captions dropped the "reż."
   *  prefix; a name straight after the year comma is still the director. */
  private val DirectorNoCreditWordPat = """^\d{4}\s*,\s*(.+?)\s*$""".r
  private val YearInHeaderPat = """\b(20\d{2})\b""".r
  /** Co-directed films are listed either "Wilhelm Sasnal i Anna Sasnal" or
   *  "Wilhelm Sasnal, Anna Sasnal"; both mean two people. */
  private val DirectorSeparator = """\s*,\s*|\s+i\s+""".r

  private case class RawSlot(
    title:          String,
    dateTime:       LocalDateTime,
    postUrl:        String,
    runtimeMinutes: Option[Int],
    releaseYear:    Option[Int],
    director:       Seq[String]
  )

  /** Walk the post body in document order, carrying the month header's year and
   *  the current day, and emit one slot per screening paragraph.
   *
   *  Day headers ("30.07 (czwartek)") used to live only in `<h4>`; the site's
   *  September 2026 relayout moved them into plain `<p>` instead (and moved
   *  the month header from `<h3>` into `<h4>`). Rather than hard-coding either
   *  tag, a day header is recognised by its TEXT SHAPE wherever it appears —
   *  `h3`, `h4` or `p` — and only an element that ISN'T one is considered for
   *  the other two roles (month/year header, or screening paragraph). This
   *  also fixes a latent bug the old tag-based dispatch had: a non-day `<h4>`
   *  (the month header, under the new layout) used to unconditionally reset
   *  `day` to `None` just by virtue of being an `<h4>` that didn't match
   *  `DayPat` — harmless while month headers only ever opened a post, but
   *  wrong in general. */
  private def parseProgramme(renderedHtml: String, postUrl: String, today: LocalDate): Seq[RawSlot] = {
    val body = Jsoup.parseBodyFragment(renderedHtml).body()
    dropArchive(body)

    var year: Int                 = today.getYear
    var previousMonth: Option[Int] = None
    var day: Option[LocalDate]     = None
    val slots = Seq.newBuilder[RawSlot]

    body.select("h3, h4, p").asScala.foreach { element =>
      val text = element.text.trim
      text match {
        case DayPat(d, m) =>
          val month = m.toInt
          // Months only ever advance within one post; a month that goes
          // BACKWARDS is a December→January wrap, so roll the year.
          if (previousMonth.exists(_ > month)) year += 1
          previousMonth = Some(month)
          day = Try(LocalDate.of(year, month, d.toInt)).toOption
        case _ if element.tagName == "h3" || element.tagName == "h4" =>
          // A month header with an explicit year re-anchors the calendar; reset
          // the rollover tracker so "Styczeń 2027" after a December day isn't
          // ALSO bumped a year by the wrap rule below.
          YearInHeaderPat.findFirstMatchIn(text).foreach { m =>
            year = m.group(1).toInt
            previousMonth = None
          }
        case _ =>
          for {
            date <- day
            slot <- parseScreening(element, date, postUrl)
          } slots += slot
      }
    }
    slots.result()
  }

  /** Remove the "ARCHIWALNE SEANSE" accordion — identically-shaped blocks of
   *  screenings that have already happened. */
  private def dropArchive(body: Element): Unit =
    body.select("details").asScala
      .filter(d => Option(d.selectFirst("summary")).exists(_.text.toUpperCase.contains("ARCHIWALNE")))
      .foreach(_.remove())

  /** One `<p>` → one screening, or None when the paragraph isn't one (a stray
   *  note between days). */
  private def parseScreening(paragraph: Element, date: LocalDate, postUrl: String): Option[RawSlot] =
    for {
      strongs         <- Option(paragraph.select("strong")).filter(!_.isEmpty)
      // "17:00 – Tony" sometimes arrives as ONE <strong>, sometimes split
      // across three siblings ("17:00" / "– " / "Tony"); joining and
      // collapsing whitespace handles both.
      strongText       = strongs.asScala.map(_.text.trim).mkString(" ").replaceAll("\\s+", " ").trim
      (time, title)   <- strongText match {
                           case TimeTitlePat(h, m, t) =>
                             Try(LocalTime.of(h.toInt, m.toInt)).toOption.map(_ -> t.trim).filter(_._2.nonEmpty)
                           case _ => None
                         }
    } yield {
      val caption = captionOf(paragraph)
      RawSlot(
        title          = title,
        dateTime       = LocalDateTime.of(date, time),
        postUrl        = postUrl,
        runtimeMinutes = RuntimePat.findFirstMatchIn(paragraph.text).map(_.group(1).toInt),
        releaseYear    = YearPat.findFirstMatchIn(caption).map(_.group(1).toInt),
        director       = directorOf(caption)
      )
    }

  /** The paragraph's second line — year/director/notes — whether it's wrapped
   *  in `<em>` (posts through August 2026) or left as plain text after a
   *  `<br>` (September 2026 on). Reads the raw HTML and splits on `<br>`
   *  rather than `Element.text`, which collapses line breaks entirely. */
  private def captionOf(paragraph: Element): String =
    Option(paragraph.selectFirst("em")).map(_.text.trim).filter(_.nonEmpty).getOrElse {
      paragraph.html.split("""<br\s*/?>""").drop(1).headOption
        .map(line => Jsoup.parseBodyFragment(line).body.text.trim).getOrElse("")
    }

  /** `reż. Mikołaj Janik` when the caption spells the credit out; September
   *  2026's captions dropped that word, leaving just `1962, Orson Welles`, so
   *  a name straight after the year comma is taken as the director too. */
  private def directorOf(caption: String): Seq[String] = {
    val credited = DirectorPat.findFirstMatchIn(caption).map(_.group(1))
    val fallback = if (credited.isEmpty) DirectorNoCreditWordPat.findFirstMatchIn(caption).map(_.group(1)) else None
    (credited orElse fallback).toSeq
      .flatMap(DirectorSeparator.split(_).toSeq)
      .map(_.trim).filter(_.nonEmpty)
  }
}
