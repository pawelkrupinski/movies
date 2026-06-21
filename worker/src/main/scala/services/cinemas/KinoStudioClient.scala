package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Studio (Opole, run by MDK Opole). Its repertoire at
 * `mdk.opole.pl/kino-studio.html` is a hand-edited CMS page inside
 * `div.ckeditor`. The page lists one film at a time with the structure:
 *   - `<h3>DD.MM (...)` → the screening date (day.month, no year — inferred
 *     from `today`; a month before today rolls to next year)
 *   - `<p>godziny seansów: <strong>HH.MM [i HH.MM ...]</strong>` → times,
 *     separated by " i " and using `.` as hour:minute separator
 *   - `<h1>` → the film title
 *   - `<p>gatunek: <strong>…</strong>` → genre
 *   - `<p><em>` → synopsis (first `<em>` paragraph)
 *   - `<p><img>` above the `<h3>` → poster
 *
 * Box-office only — no booking URLs. The cinema has one screen and typically
 * runs one film for a short season, so it's common to see only a single
 * `<h1>` block with one or two `<h3>` date headers.
 */
class KinoStudioClient(
  http:             HttpFetch,
  override val cinema: Cinema = KinoStudio,
  today:            LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoStudioClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoStudioClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = KinoStudioClient.parse(http.get(KinoStudioClient.RepertoireUrl), cinema, today)
}

object KinoStudioClient {

  val BaseUrl       = "https://mdk.opole.pl"
  val RepertoireUrl = s"$BaseUrl/kino-studio.html"

  /** "DD.MM" at the start of an `<h3>` heading. */
  private val DatePat = """^(\d{1,2})\.(\d{1,2})\b""".r

  /** "HH.MM" time tokens (dot-separated). */
  private val TimePat = """(\d{1,2})\.(\d{2})""".r

  private case class RawFilm(
    title:     String,
    showtimes: Seq[LocalDateTime],
    posterUrl: Option[String],
    synopsis:  Option[String],
    genres:    Seq[String]
  )

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val doc     = Jsoup.parse(html, BaseUrl)
    val ckeditor = Option(doc.selectFirst("div.ckeditor")).getOrElse(doc.body())
    val films   = extractFilms(ckeditor, today)
    films.map { f =>
      CinemaMovie(
        movie     = Movie(ScraperParse.stripFormatTags(f.title), genres = f.genres),
        cinema    = cinema,
        posterUrl = f.posterUrl,
        filmUrl   = Some(RepertoireUrl),
        synopsis  = f.synopsis,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = f.showtimes.map(dt => Showtime(dt, None)).sortBy(_.dateTime)
      )
    }
  }

  /**
   * Walk the children of the content div in document order. The structure is
   * free-form CMS HTML, so we use a state machine: each `<h3>` that starts with
   * "DD.MM" opens a new date context; `<h1>` opens a new film context (title +
   * resets accumulated dates/times); accumulated (date, time) pairs are folded
   * into showtimes when a new `<h1>` or the end of the element stream is reached.
   */
  private def extractFilms(container: Element, today: LocalDate): Seq[RawFilm] = {
    val children = container.children().asScala.toSeq
    val films    = scala.collection.mutable.Buffer.empty[RawFilm]

    // Mutable state as we scan
    var pendingDates  = Seq.empty[LocalDate]
    var pendingTimes  = Seq.empty[LocalTime]
    var pendingPoster = Option.empty[String]
    var currentTitle  = Option.empty[String]
    var currentGenres = Seq.empty[String]
    var currentSynopsis = Option.empty[String]

    def flushFilm(): Unit =
      currentTitle.filter(_.nonEmpty).foreach { title =>
        val showtimes = for { d <- pendingDates; t <- pendingTimes } yield LocalDateTime.of(d, t)
        if (showtimes.nonEmpty)
          films += RawFilm(title, showtimes.distinct.sorted, pendingPoster, currentSynopsis, currentGenres)
        pendingDates    = Seq.empty
        pendingTimes    = Seq.empty
        pendingPoster   = None
        currentTitle    = None
        currentGenres   = Seq.empty
        currentSynopsis = None
      }

    children.foreach { el =>
      el.tagName match {
        case "h1" =>
          // A new film title — commit the previous film then start fresh.
          flushFilm()
          currentTitle = Some(el.text.trim).filter(_.nonEmpty)

        case "h3" =>
          // Date line — "25.06 (czwartek) - ..."; only count it if the
          // date regex matches so non-date <h3>s (e.g. "REPERTUAR") are ignored.
          DatePat.findFirstMatchIn(el.text).foreach { m =>
            val day   = m.group(1).toInt
            val month = m.group(2).toInt
            val year  = if (month < today.getMonthValue) today.getYear + 1 else today.getYear
            Try(LocalDate.of(year, month, day)).toOption.foreach { d =>
              pendingDates = pendingDates :+ d
            }
          }
          // Reset times when a new date header appears — each date block
          // carries its own "godziny seansów" in the following <p>.
          pendingTimes = Seq.empty

        case "p" =>
          val text  = el.text
          val lower = text.toLowerCase
          // "godziny seansów:" line — extract all HH.MM tokens from the <strong>
          if (lower.contains("godziny") || lower.contains("seansów") || lower.contains("seansow")) {
            val strong = Option(el.selectFirst("strong")).map(_.text).getOrElse(text)
            pendingTimes = TimePat.findAllMatchIn(strong)
              .flatMap(m => Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)
              .toSeq
          }
          // Metadata block — may contain gatunek, reżyseria, obsada, etc.,
          // each on a <br/>-separated line. Extract the "gatunek: <strong>X</strong>"
          // line by scanning the element's own text lines and correlating with
          // the <strong> elements at matching positions.
          else if (lower.contains("gatunek") || lower.contains("reżyseria") || lower.contains("rezyseria") || lower.contains("obsada")) {
            // wholeOwnText has no <br> → use the original HTML, split on <br>
            // and parse each fragment independently using Jsoup to extract the value.
            val fragments = el.html.split("(?i)<br\\s*/?>").toSeq
            fragments.foreach { frag =>
              val fragLower = frag.toLowerCase
              if (fragLower.contains("gatunek") && currentGenres.isEmpty) {
                val fragEl = Jsoup.parseBodyFragment(frag).body
                Option(fragEl.selectFirst("strong")).map(_.text.trim)
                  .filter(_.nonEmpty)
                  .foreach { g =>
                    currentGenres = g.split(",").map(_.trim).filter(_.nonEmpty).toSeq
                  }
              }
            }
          }
          // First synopsis paragraph (em-wrapped prose)
          else if (currentSynopsis.isEmpty && el.select("em").size > 0) {
            val em = el.selectFirst("em")
            val prose = Option(em).map(_.text.trim).filter(_.length > 20)
            currentSynopsis = prose
          }
          // Poster — a bare <p> containing only an <img> above the <h3> date
          else if (el.select("img").size > 0 && el.text.trim.isEmpty) {
            pendingPoster = Option(el.selectFirst("img"))
              .map(_.attr("abs:src")).filter(_.nonEmpty)
          }

        case _ =>
      }
    }
    flushFilm()
    films.toSeq
  }
}
