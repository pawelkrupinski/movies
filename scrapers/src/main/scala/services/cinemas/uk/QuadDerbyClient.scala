package services.cinemas.uk

import models.{CinemaMovie, QuadDerby}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.Logging
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScraperParse}
import tools.{HttpFetch, HttpRead}
import tools.Whitespace.collapsedSpaces

import java.time.LocalDate
import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Derby QUAD. Its Spektrix box office (`derbyquad`) lists the programme, but files a film with
 * no description, image, web link, year or director: "Obsession (PG)", 96 minutes, is all it
 * says of Edward Dmytryk's 1949 noir, which then matched nothing. The venue's own page for it
 * (`/whats-on/obsession-pg`) gives the director, country, genre, running time and year as a
 * `Label: value` list, the synopsis, and the poster.
 *
 * The page's slug is the venue's to choose ("night-of-the-demon" for "Night of the Demon (PG)"),
 * and the page names no Spektrix id, so the listing finds it on the venue's programme,
 * `/whats-on`: each card there gives the page's link, the film's title and its certificate, which
 * together spell the box office's name for it. A film the programme bills otherwise ("The Blob"
 * for "Geek Bank Presents: The Blob (18)") keeps no page rather than a guessed one.
 */
class QuadDerbyClient(http: HttpFetch, today: => LocalDate)
  extends SpektrixClient(http, "derbyquad", QuadDerby, today) with DetailEnricher with Logging {

  import QuadDerbyClient._

  override def scrapeHosts: Set[String] = super.scrapeHosts ++ CinemaScraper.hostsOf(SiteUrl)

  /** The box office's films, each pointed at its page on the venue's programme. That page only adds the link: when
   *  it fails the films are still listed, without one, rather than the box office's answer failing with it. */
  override protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val pages = Try(pagesByName(HttpRead.page(http, WhatsOnUrl))).fold(
      e => { logger.info(s"Derby QUAD's programme page failed, so its films carry no page: $e"); Map.empty[String, String] },
      identity)
    super.fetchUnfiltered().map(cm => cm.copy(filmUrl = cm.filmUrl.orElse(pages.get(nameKey(cm.movie.title)))))
  }

  override val detailGroup: String = "quad-derby"

  /** A durable 404/410 escapes, so a page gone for good is stamped rather than retried — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(html => parseDetail(Jsoup.parse(html, ref)))
}

object QuadDerbyClient {

  val SiteUrl    = "https://www.derbyquad.co.uk"
  val WhatsOnUrl = s"$SiteUrl/whats-on"

  /** Each programme card's page, by the name the box office gives its film: the card's title, and
   *  that title with its certificate in brackets ("Obsession" + "PG" → "Obsession (PG)"). A name two
   *  cards share names neither. */
  def pagesByName(html: String): Map[String, String] = {
    val cards = Jsoup.parse(html, SiteUrl).select("div.card__title").asScala.toSeq.flatMap { title =>
      Option(title.selectFirst("a[href]")).map(_.absUrl("href")).filter(_.nonEmpty).toSeq.flatMap { page =>
        val name        = title.selectFirst("a[href]").text
        val certificate = Option(title.selectFirst(".certificate-icon")).map(_.text.trim).filter(_.nonEmpty)
        (name +: certificate.map(c => s"$name ($c)").toSeq).map(nameKey(_) -> page)
      }
    }
    cards.groupBy(_._1).collect { case (name, pages) if pages.map(_._2).distinct.size == 1 => name -> pages.head._2 }
  }

  /** The running time, which the page gives as bare minutes ("96"). */
  private val Minutes = """\d+""".r

  private def nameKey(name: String): String = name.collapsedSpaces.toLowerCase(Locale.ROOT)

  def parseDetail(document: Document): FilmDetail = {
    val facts = document.select("p.event__metadata-item").asScala.toSeq.flatMap { item =>
      for {
        label <- Option(item.selectFirst(".event__metadata-label"))
        value <- Option(item.selectFirst(".event__metadata-value")).map(_.text.trim).filter(_.nonEmpty)
      } yield label.text.trim.stripSuffix(":").toLowerCase(Locale.ROOT) -> value
    }.toMap
    FilmDetail(
      synopsis       = Option(document.selectFirst("div.event__description")).map(ScraperParse.blockText(_).trim).filter(_.nonEmpty),
      director       = facts.get("director").toSeq.flatMap(ScraperParse.listItems),
      runtimeMinutes = facts.get("running time").flatMap(Minutes.findFirstIn).map(_.toInt).filter(_ > 0),
      releaseYear    = facts.get("year").flatMap(ScraperParse.yearIn),
      countries      = facts.get("country of origin").toSeq.flatMap(ScraperParse.listItems),
      genres         = facts.get("film genre").toSeq.flatMap(ScraperParse.listItems),
      // The hero image, resized by the image CDN's `?tr=` transform; the bare URL is the original.
      posterUrl      = Option(document.selectFirst("img[alt^=Event poster]")).map(_.absUrl("src").takeWhile(_ != '?')).filter(_.nonEmpty)
    )
  }
}
