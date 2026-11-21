package services.cinemas.pl

import java.util.Locale

import scala.concurrent.duration.*

import models.*
import org.jsoup.Jsoup
import services.cinemas.common.SingleSiteScraper
import services.cinemas.common.SlotsToMovies
import services.cinemas.common.SynopsisProse
import services.cinemas.common.{ScraperParse, ListingPages}
import tools.{HttpFetch, HttpRead}

/**
 * Kino Kępa at PROM Kultury Saska Kępa (Warszawa). The "kino-kepa" category
 * page links to one event page per screening; each event page carries its
 * date+time, a `reż. … , Country Year, NNN'` metadata line, the booking link
 * and synopsis. Films are grouped by their (de-prefixed) title.
 */
class PromKepaClient(http: HttpFetch) extends SingleSiteScraper {

  val cinema: Cinema = KinoKepa

  private val BaseUrl    = "https://promkultury.pl"
  private val ListingUrl = s"$BaseUrl/events/category/kino-kepa/"
  private val EventPat   = """/wydarzenie/([a-z0-9-]+)""".r

  protected def siteUrl: String = BaseUrl

  def fetch(): Seq[CinemaMovie] = {
    val listing = HttpRead.page(http, ListingUrl)
    val slugs   = EventPat.findAllMatchIn(listing).map(_.group(1)).toSeq.distinct

    val pages = ListingPages.readMore("prom-kepa-events", slugs, s => s"$BaseUrl/wydarzenie/$s", timeout = 1.minute) { url =>
      Option(HttpRead.page(http, url))
    }.toMap
    val events = slugs.flatMap(s => pages.getOrElse(s, None).flatMap(html => PromKepaClient.parseEvent(html, s)))

    // The source formats the same film inconsistently (quoted vs ALL-CAPS), so
    // group case-insensitively and display the best-cased variant.
    SlotsToMovies.foldBlocks(events, _.title.toLowerCase(Locale.ROOT), _.showtime.toSeq, order = SlotsToMovies.MovieOrder.asGrouped) { (_, group, showtimes) =>
      val primary = group.head
      val title   = group.map(_.title).find(_.exists(_.isLower)).getOrElse(primary.title)
      Some(CinemaMovie(
        movie     = Movie(title = title, runtimeMinutes = primary.runtime, releaseYear = primary.year, countries = primary.countries),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = Some(s"$BaseUrl/wydarzenie/${primary.slug}"),
        synopsis  = primary.synopsis,
        director  = primary.director,
        showtimes = showtimes
      ))
    }
  }
}

object PromKepaClient {

  private val TimelinePat = raw"""(\d{1,2}\.\d{2}\.\d{4},\s*${ScraperParse.ClockText})""".r
  // "reż. David Frankel, USA 2026, 120'"
  private val MetaPat = """reż\.\s*(.+?),\s*(.+?)\s+((?:19|20)\d{2}),\s*(\d+)['’]""".r

  private case class Event(slug: String, title: String, showtime: Option[Showtime], runtime: Option[Int],
                           year: Option[Int], countries: Seq[String], director: Seq[String],
                           synopsis: Option[String], poster: Option[String])

  /** "Francja, Włochy", "Polska/USA" and the spaced "Polska USA": each piece that is no country itself but a run of
   *  them ("Polska USA", "Wielka Brytania Irlandia") is split into those; anything else stays as written. */
  private def countriesIn(text: String): Seq[String] =
    text.split("[,/]").toSeq.map(_.trim).filter(_.nonEmpty).flatMap { piece =>
      if (CountryNames.isCountry(piece)) Seq(piece) else countryRun(piece.split("\\s+").toList).getOrElse(Seq(piece))
    }

  /** `words` read as consecutive country names, the longest name first at each step; `None` when they are not. */
  private def countryRun(words: List[String]): Option[List[String]] =
    if (words.isEmpty) Some(Nil)
    else (words.length to 1 by -1).iterator.flatMap { length =>
      val name = words.take(length).mkString(" ")
      if (CountryNames.isCountry(name)) countryRun(words.drop(length)).map(name :: _) else None
    }.nextOption()

  private def parseEvent(html: String, slug: String): Option[Event] = {
    val document   = Jsoup.parse(html)
    val title = Option(document.selectFirst("h3.event_title")).map(_.text.trim)
      .map(_.replaceFirst("(?i)^KINO KĘPA:\\s*", "").replaceAll("[\"„”“]", "").trim).filter(_.nonEmpty)

    title.map { t =>
      val dt = TimelinePat.findFirstMatchIn(Option(document.selectFirst("p.timeline")).map(_.text).getOrElse(""))
        .flatMap(m => ScraperParse.parseDateTime(m.group(1)))
      val booking = Option(document.selectFirst("p.ticket a.cfs-hyperlink[href]")).map(_.attr("href")).filter(_.nonEmpty)
      // The page nests the metadata `<p>` inside `p.repertuar_description`, which an HTML parser closes before it, so
      // the line is the description's next sibling: read it off the event's whole info column.
      val meta = MetaPat.findFirstMatchIn(Option(document.selectFirst("div.content_item")).map(_.text).getOrElse(""))
      Event(
        slug      = slug,
        title     = t,
        showtime  = dt.map(d => Showtime(d, booking, None, Nil)),
        runtime   = meta.map(_.group(4).toInt),
        year      = meta.map(_.group(3).toInt),
        countries = meta.toSeq.flatMap(m => countriesIn(m.group(2))),
        director  = meta.toSeq.flatMap(m => ScraperParse.commaItems(m.group(1))),
        synopsis  = Option(document.selectFirst("div.single_content")).map(SynopsisProse.blockText(_).trim)
                      // (?s) so the booking-footer cut spans the paragraph newlines blockText now keeps.
                      .map(_.replaceAll("(?is)\\s*bilet[^.]*\\d+\\s*zł.*$", "").trim).filter(_.length > 20),
        poster    = Option(document.selectFirst("div.image_item img.wp-post-image[src]")).flatMap(ScraperParse.largestImageSource)
      )
    }
  }
}
