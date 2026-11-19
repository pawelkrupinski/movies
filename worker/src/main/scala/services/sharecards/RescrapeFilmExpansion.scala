package services.sharecards

import play.api.Logging
import services.facebook.FacebookFetches

import java.time.Clock
import scala.util.{Failure, Success, Try}

/** A country's due films in the fleet's re-scrape queue expanded into their pages ([[RescrapeTarget.Page]], no quota:
 *  `pages` is a read-model read), each marked with whether Facebook holds a preview of it ([[FacebookFetches]]), so
 *  those go first. A film whose pages cannot be read waits and tries again, and is dropped after
 *  [[RescrapePacing.MaxAttempts]]. */
final class RescrapeFilmExpansion(store: FacebookRescrapeStore, pages: String => Seq[String], fetches: FacebookFetches,
                                  country: String, clock: Clock) extends Logging {
  import RescrapePacing.*

  /** Expand the films due into their pages: [[tools.TickRun.Incomplete]] when one film's pages could not be read. */
  def expandDue(): tools.TickRun =
    Iterator.continually(store.claim(country, RescrapeKind.Film, clock.instant(), Lease)).take(FilmsPerTick)
      .takeWhile(_.isDefined).flatten.map(expand)
      .foldLeft(tools.TickRun.Completed)((all, one) => if (one == tools.TickRun.Incomplete) one else all)

  private def expand(film: RescrapeEntry): tools.TickRun = {
    val filmId = film.target match { case RescrapeTarget.FilmPages(_, id) => id; case other => other.key }
    Try(previewed(pages(filmId))) match {
      case Success(urls) =>
        store.add(urls.map { case (url, seen) => RescrapeEntry(country, RescrapeTarget.Page(url, seen), clock.instant()) })
        store.complete(film)
        tools.TickRun.Completed
      case Failure(e) if film.attempts >= MaxAttempts =>
        logger.warn(s"share card: re-scrape of $filmId dropped, its pages could not be read ${film.attempts} times: ${e.getMessage}")
        store.complete(film)
        tools.TickRun.Incomplete
      case Failure(e) =>
        logger.info(s"share card: re-scrape of $filmId deferred, its pages could not be read: ${e.getMessage}")
        store.retry(film, clock.instant().plusMillis(backoff(film.attempts).toMillis), countAttempt = true)
        tools.TickRun.Incomplete
    }
  }

  /** Each of `urls` with whether Facebook holds a preview of it — fetched within its preview's lifetime. */
  private def previewed(urls: Seq[String]): Seq[(String, Boolean)] = {
    val seen = fetches.fetchedSince(urls, clock.instant().minusMillis(FacebookFetches.PreviewLifetime.toMillis))
    urls.map(url => url -> seen(url))
  }
}
