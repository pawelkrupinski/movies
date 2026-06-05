package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Dolnośląskie Centrum Filmowe (Wrocław). The repertoire page lists every
 * screening with its full date, time, and auditorium encoded in the
 * `aria-label` of each slot; ticketing and the richer film metadata live on
 * the Bilety24 event page (`dcf.bilety24.pl/wydarzenie/?id=<filmId>`), fetched
 * per film for runtime / genres / director / country / year / synopsis. A
 * missing or slow detail fetch degrades to listing-only data for that film.
 */
class DcfClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = DolnoslaskieCentrumFilmowe

  private val RepertoireUrl = "https://dcf.wroclaw.pl/repertuar/"
  private val EventBase     = "https://dcf.bilety24.pl/wydarzenie/?id="
  private val TicketBase    = "https://dcf.bilety24.pl/kup-bilety/?id="

  // aria-label: "Tytuł; Miejsce: Sala Warszawa; Data: 05.06.2026 15:30"
  private val AriaPat = """^(.+?); Miejsce: (.+?); Data: (\d{2}\.\d{2}\.\d{4}) (\d{2}:\d{2})$""".r
  private val DateTimeFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")
  private val FilmIdPat   = """film-(\d+)""".r

  private case class RawSlot(dateTime: LocalDateTime, room: Option[String], bookingUrl: Option[String])

  def fetch(): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(http.get(RepertoireUrl))

    // A film can repeat under several date sections; each repeat is its own
    // `.film__item` block. Group by the numeric film id so all its slots merge.
    case class Block(filmId: String, title: String, poster: Option[String], slots: Seq[RawSlot])
    val blocks = doc.select("div.film__item").asScala.toSeq.flatMap { el =>
      val filmId = el.classNames().asScala.collectFirst { case FilmIdPat(id) => id }
      val title  = Option(el.selectFirst("h3.film__title")).map(_.text.trim).filter(_.nonEmpty)
      for { id <- filmId; t <- title } yield
        Block(id, t, posterOf(el), slotsOf(el))
    }

    // The same film recurs under several film ids when it plays in more than
    // one programme strand (a regular run + a "| FKS" / "| DKF" / preview slot).
    // Group by the cleaned title so each film is one row; the primary block
    // (first seen) supplies the poster and the detail page to enrich from.
    val byTitle = blocks.groupBy(b => DcfClient.normalizeTitle(b.title))

    val details: Map[String, DcfClient.Detail] = {
      val pending = byTitle.values.map(_.head.filmId).toSeq.distinct
        .map(id => id -> http.getAsync(EventBase + id))
      pending.map { case (id, f) =>
        id -> Try(f.join()).toOption.map(DcfClient.parseDetail).getOrElse(DcfClient.Detail.empty)
      }.toMap
    }

    byTitle.toSeq.flatMap { case (title, group) =>
      val primary = group.head
      val slots   = group.flatMap(_.slots).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      if (slots.isEmpty) None
      else {
        val d = details.getOrElse(primary.filmId, DcfClient.Detail.empty)
        Some(CinemaMovie(
          movie     = Movie(
            title          = title,
            runtimeMinutes = d.runtimeMinutes,
            releaseYear    = d.year,
            countries      = d.countries,
            genres         = d.genres
          ),
          cinema    = cinema,
          posterUrl = primary.poster,
          filmUrl   = Some(EventBase + primary.filmId),
          synopsis  = d.synopsis,
          cast      = Seq.empty,
          director  = d.director,
          showtimes = slots.map(s => Showtime(s.dateTime, s.bookingUrl, s.room, Nil))
        ))
      }
    }
  }

  private def posterOf(block: Element): Option[String] =
    Option(block.selectFirst("div.film__poster .thumbnail-cover"))
      .map(_.attr("style"))
      .flatMap(s => """url\((?:'|"|&quot;)?(.+?)(?:'|"|&quot;)?\)""".r.findFirstMatchIn(s).map(_.group(1)))
      .filter(_.nonEmpty)

  private def slotsOf(block: Element): Seq[RawSlot] =
    block.select("div.repertoir__item").asScala.toSeq.flatMap { item =>
      val label = Option(item.selectFirst("a.link-absolute")).map(_.attr("aria-label")).getOrElse("")
      AriaPat.findFirstMatchIn(label).flatMap { m =>
        Try(LocalDateTime.parse(s"${m.group(3)} ${m.group(4)}", DateTimeFmt)).toOption.map { dt =>
          val room    = Some(m.group(2).trim).filter(_.nonEmpty)
          val showId  = Option(item.selectFirst("[data-target]")).map(_.attr("data-target"))
                          .flatMap(t => """#repertoir(\d+)""".r.findFirstMatchIn(t).map(_.group(1)))
          RawSlot(dt, room, showId.map(TicketBase + _))
        }
      }
    }
}

object DcfClient {

  /** DCF tags special-programme screenings with a trailing `" | LABEL"`
   *  (e.g. "| DKF", "| FKS", "| pokaz przedpremierowy"). Strip it so a tagged
   *  screening merges onto — and enriches off — the same clean film title as
   *  the regular run. Public for direct unit tests. */
  def normalizeTitle(raw: String): String = {
    val i = raw.indexOf(" | ")
    (if (i > 0) raw.substring(0, i) else raw).trim
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

  private val RuntimePat = """(\d+)\s*min""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  /** Parse the Bilety24 event page: a `p.movie-parameters` pipe line
   *  ("Dramat | Komedia | 120 min") and a description block whose first line is
   *  "reż. <director> | <country> | <year>" followed by the synopsis. */
  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)

    val params = Option(doc.selectFirst("p.movie-parameters")).map(_.text.trim).getOrElse("")
    val segs   = params.split("\\|").map(_.trim).filter(_.nonEmpty).toSeq
    val runtime = segs.flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt)).headOption
    val genres  = segs.filterNot(s => RuntimePat.findFirstMatchIn(s).isDefined)
                      .map(tools.TextNormalization.titleCaseIfAllLower)

    Option(doc.selectFirst("div.title-description-content")) match {
      case None => Detail(runtime, None, Seq.empty, genres, Seq.empty, None)
      case Some(desc) =>
        val lines    = desc.html.split("(?i)<br\\s*/?>").map(l => Jsoup.parseBodyFragment(l).text.trim).filter(_.nonEmpty)
        val infoLine = lines.headOption.getOrElse("")
        val parts    = infoLine.split("\\|").map(_.trim).filter(_.nonEmpty)
        val director = parts.find(_.toLowerCase.startsWith("reż")).map(_.replaceFirst("(?i)^reż\\.?\\s*", "").trim)
                          .filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
        val year     = YearPat.findFirstMatchIn(infoLine).map(_.group(1).toInt)
        val country  = parts.find(p => !p.toLowerCase.startsWith("reż") && YearPat.findFirstMatchIn(p).isEmpty)
                          .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
        val synopsis = Option(lines.drop(1).mkString(" ").trim).filter(_.length > 20)
        Detail(runtime, year, country, genres, director, synopsis)
    }
  }
}
