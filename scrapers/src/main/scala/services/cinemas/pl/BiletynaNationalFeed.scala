package services.cinemas.pl

import com.github.benmanes.caffeine.cache.{Cache, Expiry, Ticker}
import play.api.libs.json._
import tools.{HttpFetch, HttpRead}

import java.net.URI
import scala.concurrent.duration._
import scala.util.Try

/**
 * Every biletyna.pl event in the country, read off the site's own `/ajax/events`
 * feed a few pages at a time, and handed to each venue's [[BiletynaClient]] as the
 * slice filed under its place page.
 *
 * Why: biletyna 403s our datacenter IP, so every request it gets goes through the
 * paid residential proxy. One place page per venue — plus the feed pages a busy
 * venue needs past the page's 50-event cap — came to ~110 proxied requests a pass
 * for 82 venues; the national feed is ~7 (16,254 events, 14.6 MB, measured
 * 2026-09-27). Each record names its hall by the same path as our place page
 * (`v2_hall_seo_url` = `/Czluchow/Kino-Uciecha`), so the attribution is exact:
 * three venues compared screening for screening that day matched 20/20, 49/49 and
 * 33/33 by booking id.
 *
 * All categories are read, not only films: biletyna files some screened broadcasts
 * (André Rieu) as concerts, which the client keeps and the live stage programme
 * it drops — the same records either way, so the same decision.
 *
 * The whole programme is fetched at most once per [[ttl]], however many venue
 * scrapes ask in that window, and only the records of `halls` (our venues) are
 * kept. A failed read throws, so the caller falls back to the venue's own place
 * page rather than reading empty — and is held for [[FailureHold]], so the other
 * venues scraped meanwhile go straight to their place pages instead of each
 * re-reading a feed that just failed through the paid proxy.
 */
class BiletynaNationalFeed(http: HttpFetch, halls: Set[BiletynaPlacePage],
                           ttl: FiniteDuration = BiletynaNationalFeed.DefaultTtl,
                           ticker: Ticker = Ticker.systemTicker()) {
  import BiletynaNationalFeed._

  private val wanted: Set[HallPath] = halls.map(pathOf)

  private val programme: Cache[Unit, Try[Map[HallPath, Vector[JsValue]]]] =
    // One key (the feed is one document), so a bound of one says what it holds.
    tools.BoundedCache.ofSize(1)
      .expireAfter(Expiry.creating[Unit, Try[Map[HallPath, Vector[JsValue]]]]((_, read) =>
        java.time.Duration.ofNanos((if (read.isSuccess) ttl else FailureHold.min(ttl)).toNanos)))
      .ticker(ticker)
      .build()

  /** The feed's records for `page`'s hall, or `None` when the feed files nothing
   *  under it — a venue the feed doesn't know is read off its own page instead. */
  def eventsAt(page: BiletynaPlacePage): Option[Seq[JsValue]] =
    programme.get((), _ => Try(load())).get.get(pathOf(page))

  private def load(): Map[HallPath, Vector[JsValue]] = {
    def records(feedPage: Int): Seq[JsValue] =
      BiletynaClient.feedRecords(s"national feed page $feedPage", HttpRead.page(http, pageUrl(feedPage)))
    def ours(page: Seq[JsValue]): Seq[(HallPath, JsValue)] =
      page.flatMap(r => (r \ "v2_hall_seo_url").asOpt[String].map(HallPath(_)).filter(wanted).map(_ -> r))

    // The walk ends on a page shorter than the page size. If the server served fewer per page
    // than `ipp` asked — a clamp — the FIRST page is already short, and stopping there kept a
    // slice of the country as the whole of it. So a short first page is checked against the
    // second: anything there means the server's own page size is the first page's.
    val first    = records(1)
    val second   = Option.when(first.size < PageSize && first.nonEmpty)(records(2))
    val pageSize = if (second.exists(_.nonEmpty)) first.size else PageSize
    @annotation.tailrec
    def fetchFrom(feedPage: Int, page: Seq[JsValue], acc: Vector[(HallPath, JsValue)]): Vector[(HallPath, JsValue)] = {
      val merged = acc ++ ours(page)
      if (page.size < pageSize) merged
      else if (feedPage >= MaxPages)
        throw new IllegalStateException(s"biletyna's national event feed did not end after $MaxPages pages of $pageSize")
      else fetchFrom(feedPage + 1, if (feedPage == 1) second.getOrElse(records(2)) else records(feedPage + 1), merged)
    }
    fetchFrom(1, first, Vector.empty).groupMap(_._1)(_._2)
  }
}

object BiletynaNationalFeed {
  /** Short against the venue scrape cadence, so a new screening shows within a
   *  pass or two; long enough that one fetch serves every venue scraped in it. */
  val DefaultTtl: FiniteDuration = 20.minutes

  /** How long a failed read is answered from memory: long enough to spare the venues
   *  scraped right after it, short enough that a blip costs a pass, not the TTL. */
  val FailureHold: FiniteDuration = 5.minutes

  // 16,254 events in 7 pages; the ceiling is headroom, not a guess at growth.
  private[pl] val PageSize = 2500
  private val MaxPages     = 20

  def pageUrl(feedPage: Int): String = s"https://biletyna.pl/ajax/events?ipp=$PageSize&page=$feedPage"

  /** A hall's path, as the feed's `v2_hall_seo_url` spells it. */
  final case class HallPath(value: String) extends AnyVal

  private[pl] def pathOf(page: BiletynaPlacePage): HallPath =
    HallPath(URI.create(page.url).getPath.stripSuffix("/"))
}
