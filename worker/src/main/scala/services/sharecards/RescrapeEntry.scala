package services.sharecards

import java.time.Instant

/**
 * THE FLEET'S FACEBOOK RE-SCRAPES — one queue and one request quota shared by every country's
 * worker, because every one of them asks Facebook as the same app, and Facebook limits the app.
 *
 * A re-scrape used to be a per-country task asking for all of a film's city pages back to back:
 * spaced 10 s apart per FILM, so a film in 265 US cities was 265 requests in two minutes, and a
 * failure re-sent all of them. On 2026-09-25 one such film (Doctor Who: The Satan Pit) drew 668
 * HTTP 403s in two minutes and left DE and PL refused for the next hour.
 *
 * Now a film is one entry ([[RescrapeTarget.FilmPages]]); the drain expands it into one entry per
 * page ([[RescrapeTarget.Page]]), and each page is sent on its own slot of the fleet-wide quota
 * ([[FacebookRescrapeDrain.Spacing]] apart on average, whichever country sends). An entry already
 * waiting absorbs a second request for the same film or page, and waits for the later of the two. A page that fails retries alone; a rate
 * limit pushes the quota's next slot out for the whole fleet.
 */
sealed trait RescrapeTarget { def key: String }
object RescrapeTarget {
  /** A film whose pages are to be re-scraped — expanded into [[Page]]s when due. */
  final case class FilmPages(country: String, filmId: String) extends RescrapeTarget { val key = s"film|$country|$filmId" }
  /** One page URL; `previewed` when Facebook fetched it within its preview's lifetime
   *  ([[services.facebook.FacebookFetches]]) — so holds a preview a card change made stale — and
   *  sent before every page it did not. One entry per URL, whichever: a page waiting as never
   *  fetched becomes previewed when it is asked for as previewed, and never goes back. */
  final case class Page(url: String, previewed: Boolean = true) extends RescrapeTarget { val key = s"page|$url" }
}

/** The kinds of waiting entry, as the store filters on them. */
sealed abstract class RescrapeKind(val name: String)
object RescrapeKind {
  case object Film extends RescrapeKind("film")
  /** A page Facebook holds a preview of. */
  case object Page extends RescrapeKind("page")
  /** A page Facebook never fetched: sent with what the allowance leaves after [[Page]]s. */
  case object UnfetchedPage extends RescrapeKind("unfetched_page")
  /** The page kinds, in the order the drain sends them. */
  val Pages: Seq[RescrapeKind] = Seq(Page, UnfetchedPage)
  def of(target: RescrapeTarget): RescrapeKind = target match {
    case _: RescrapeTarget.FilmPages                  => Film
    case RescrapeTarget.Page(_, previewed) if previewed => Page
    case _: RescrapeTarget.Page                       => UnfetchedPage
  }
}

/** A waiting re-scrape. `attempts` counts claims — what [[FacebookRescrapeStore.complete]] and
 *  [[FacebookRescrapeStore.retry]] match on, so a claim whose lease ran out and was claimed again
 *  cannot settle the newer claim's entry. */
final case class RescrapeEntry(country: String, target: RescrapeTarget, notBefore: Instant, attempts: Int = 0) {
  def key: String = target.key
  def kind: RescrapeKind = RescrapeKind.of(target)

  /** The address Facebook is asked to fetch again: a page's URL, else the key. */
  def url: String = target match { case RescrapeTarget.Page(u, _) => u; case other => other.key }
}
