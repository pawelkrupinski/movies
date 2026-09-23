package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._

/**
 * Kino za Rogiem w Siedlcu (GOK Siedlec, Wielkopolska — near Wolsztyn, NOT the
 * city of Siedlce). Its repertoire at `goksiedlec.pl/kino/` is a WooCommerce
 * shop: every screening is its own WooCommerce PRODUCT (not a calendar entry),
 * one `li.product` per `ul.products` page, with WordPress's own pagination
 * (`?product-page=N`) rather than a date range — the listing simply runs
 * forward from today until the venue's last announced screening.
 *
 * Title/date/time all live in ONE product-title string, no separate fields:
 *   „<Title>”  – D <miesiąc genitive> – <weekday>, godz. HH:MM
 * e.g. „Pojedynek”  – 25 września – piątek, godz. 17:00. The title is
 * everything between the curly quotes (a nested „…” inside, as in the Andre
 * Rieu concert broadcast, is kept — there is only one closing quote, so the
 * capture runs to it); the date/time is parsed from what follows.
 *
 * No booking platform: "Zarezerwuj bilet" links to the product's own
 * `/bilet/<slug>/` page (WooCommerce cart/checkout), which doubles as the
 * `filmUrl`. The poster is served lazy-loaded — the real URL is the `<img>`'s
 * `data-src`, not its placeholder `src`.
 */
class KinoZaRogiemSiedlecClient(
  http:             HttpFetch,
  override val cinema: Cinema = KinoZaRogiemSiedlec,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoZaRogiemSiedlecClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    val slots = fetchPages(http).flatMap(parsePage(_, today))
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.bookingUrl)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = head.bookingUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoZaRogiemSiedlecClient {

  val BaseUrl       = "https://goksiedlec.pl"
  val RepertoireUrl = s"$BaseUrl/kino/"

  // A page beyond this is either a runaway loop or a genuinely huge programme;
  // 21 films across 2 pages is the observed size, so 15 pages (~180 products)
  // is ample headroom.
  private val MaxPages = 15

  // „<Title>” — the opening mark is U+201E ("low" double quote), the closing
  // U+201D; jsoup's `.text` already decodes the `&#8222;`/`&#8221;` entities.
  private val TitlePat = "„([^”]+)”".r

  private[cinemas] case class RawSlot(
    title:      String,
    dateTime:   LocalDateTime,
    bookingUrl: Option[String],
    poster:     Option[String]
  )

  /** Every product page from 1 until one comes back with no `li.product`
   *  entries (or [[MaxPages]] is hit). */
  private[cinemas] def fetchPages(http: HttpFetch): Seq[String] = {
    def loop(page: Int, acc: List[String]): List[String] =
      if (page > MaxPages) acc
      else {
        val url  = if (page == 1) RepertoireUrl else s"$RepertoireUrl?product-page=$page"
        val html = http.get(url)
        if (Jsoup.parse(html, BaseUrl).select("ul.products li.product").isEmpty) acc
        else loop(page + 1, html :: acc)
      }
    loop(1, Nil).reverse
  }

  private[cinemas] def parsePage(html: String, today: LocalDate): Seq[RawSlot] =
    Jsoup.parse(html, BaseUrl).select("ul.products li.product").asScala.toSeq.flatMap(parseProduct(_, today))

  private def parseProduct(li: Element, today: LocalDate): Option[RawSlot] = {
    val rawTitle = Option(li.selectFirst("h2.woocommerce-loop-product__title")).map(_.text.trim).getOrElse("")
    for {
      m        <- TitlePat.findFirstMatchIn(rawTitle)
      title     = m.group(1).trim
      remainder = rawTitle.substring(m.end)
      dayMonth <- ScraperParse.parseDayMonth(remainder)
      time     <- ScraperParse.parseHHmm(remainder)
      date     <- ScraperParse.upcomingDate(dayMonth, today)
    } yield RawSlot(
      title      = title,
      dateTime   = date.atTime(time),
      bookingUrl = Option(li.selectFirst("a.ast-loop-product__link")).map(_.attr("abs:href")).filter(_.nonEmpty),
      poster     = Option(li.selectFirst("img[data-src]")).map(_.attr("data-src")).filter(_.nonEmpty)
    )
  }
}
