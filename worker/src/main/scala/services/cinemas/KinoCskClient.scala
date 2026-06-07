package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino CSK — Centrum Spotkania Kultur (Lublin). The venue's Sala Kinowa hosts
 * a regular film programme that runs in themed monthly blocks (art-house,
 * award-season screenings, festival retrospectives). Tickets are sold through
 * iKSORIS at `bilety.csklublin.pl`; the main listing at `/termin.html` is a
 * server-rendered page that shows ALL upcoming events grouped by event type.
 *
 * Page structure at `bilety.csklublin.pl/termin.html`:
 *   - Multiple `<h3>` section headers — "Seanse filmowe", "Spektakle",
 *     "Koncerty", "Wystawy". Only the "Seanse filmowe" section is relevant.
 *   - Within that section, events alternate as:
 *       * `<div class="data">DD.MM.YYYY</div>` — event date.
 *       * `<a … aria-controls="…"><span>TITLE</span> …</a>` — event title.
 *       * A collapsible `<div class="collapse">` with a `<table>` listing one
 *         row per time slot: `<td>HH:MM-HH:MM</td>`, and a booking `<a>`.
 *
 * The CSK also runs "Letnie Kino na Dachu" (summer rooftop cinema, June–
 * August) whose events appear in the same "Seanse filmowe" section but are
 * always titled with the "KINO NA DACHU | " prefix — those are excluded here
 * because they are outdoor seasonal screenings, not Sala Kinowa repertoire.
 *
 * The start time is the first `HH:MM` from the time-range cell.
 * Booking URLs point to `bilety.csklublin.pl/nienumerowane.html?id=…`.
 *
 * When the Sala Kinowa is between monthly programmes the client may return an
 * empty list — this is normal; the worker retries on its next cycle.
 */
class KinoCskClient(http: HttpFetch, override val cinema: Cinema = KinoCskLublin) extends CinemaScraper {

  import KinoCskClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(TerminUrl)
    val doc  = Jsoup.parse(html)
    val slots = parseDoc(doc)

    val byTitle = slots.groupBy(_.title)
    byTitle.toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object KinoCskClient {

  val BaseUrl   = "https://bilety.csklublin.pl"
  val TerminUrl = s"$BaseUrl/termin.html"

  /** Events whose title starts with this prefix are outdoor rooftop screenings
    * — excluded from the Sala Kinowa repertoire. */
  private val OutdoorPrefix = "KINO NA DACHU"

  private val DateFmt  = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  private val DatePat  = """(\d{2}\.\d{2}\.\d{4})""".r

  private[cinemas] case class RawSlot(title: String, dateTime: LocalDateTime, bookingUrl: Option[String])

  /** Parse the iKSORIS termin page into film slots from the "Seanse filmowe"
    * section, excluding outdoor rooftop events. */
  private[cinemas] def parseDoc(doc: Document): Seq[RawSlot] = {
    // Find the <h3>Seanse filmowe</h3> header element.
    val sectionHeader = doc.select("h3").asScala.find(_.text.trim == "Seanse filmowe")
    sectionHeader match {
      case None => Seq.empty
      case Some(h3) =>
        // Walk the siblings after the <h3> until we hit the next <h3>.
        collectEventSlots(h3)
    }
  }

  /** Collect RawSlots by walking siblings of the "Seanse filmowe" h3. Each
    * event group is:
    *   div.data  (date)
    *   a[aria-controls]  (title)
    *   div.collapse  (time + booking rows)
    * Stop when the next <h3> is encountered. */
  private def collectEventSlots(h3: Element): Seq[RawSlot] = {
    val slots   = scala.collection.mutable.Buffer[RawSlot]()
    var sibling = h3.nextElementSibling()
    var currentDate: Option[LocalDate] = None

    while (sibling != null && sibling.tagName != "h3") {
      // Date div
      if (sibling.tagName == "div" && sibling.hasClass("data")) {
        currentDate = DatePat.findFirstIn(sibling.text.trim)
          .flatMap(s => Try(LocalDate.parse(s, DateFmt)).toOption)
      }

      // Title anchor (aria-controls links to the collapse div id)
      if (sibling.tagName == "a" && sibling.hasAttr("aria-controls")) {
        val titleEl = sibling.selectFirst("span")
        val rawTitle = Option(titleEl).map(_.text.trim).getOrElse("").trim
        // Skip outdoor rooftop events
        if (rawTitle.nonEmpty && !rawTitle.startsWith(OutdoorPrefix)) {
          val cleanTitle = cleanEventTitle(rawTitle)
          val collapseId = sibling.attr("aria-controls")
          // The collapse div follows immediately after the anchor element
          val collapseDiv = Option(sibling.nextElementSibling()).filter(e =>
            e.tagName == "div" && e.id == collapseId)

          collapseDiv.foreach { collapse =>
            val rows = collapse.select("tbody tr").asScala.toSeq
            rows.foreach { row =>
              val cells = row.select("td").asScala.toSeq
              val timeCell = cells.headOption.map(_.text.trim).getOrElse("")
              val time = ScraperParse.parseHHmm(timeCell)
              val bookingAnchor = Option(row.selectFirst("a[href*=nienumerowane]"))
              val bookingUrl = bookingAnchor.map(_.attr("href")).filter(_.nonEmpty)
                .map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

              for {
                date <- currentDate
                lt   <- time
              } slots += RawSlot(cleanTitle, LocalDateTime.of(date, lt), bookingUrl)
            }
          }
        }
      }

      sibling = sibling.nextElementSibling()
    }
    slots.toSeq
  }

  /** Strip the " | <location>" suffix that CSK appends to event titles
    * (e.g. "Mała Amelia | Kino CSK" → "Mała Amelia"). When there is no
    * suffix the title is returned unchanged. */
  private[cinemas] def cleanEventTitle(raw: String): String = {
    val pipe = raw.lastIndexOf(" | ")
    if (pipe >= 0) raw.substring(0, pipe).trim else raw.trim
  }
}
