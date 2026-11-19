package services.sharecards

import play.api.Logging
import java.time.Instant
import scala.util.Try

/** What a share-card render asks for when a film's previews on Facebook are out of date. */
trait ShareCardRescrapes {
  /** Re-scrape the film's pages, no sooner than `notBefore`. Never throws: a render must not
   *  fail because the queue could not be written. */
  def request(filmId: String, notBefore: Instant): Unit
}

object ShareCardRescrapes {
  /** No re-scrapes — the worker has no Facebook credentials, or no fleet database. */
  def disabled(metrics: ShareCardMetrics): ShareCardRescrapes =
    (_: String, _: Instant) => metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Disabled)
}

/** A country's requests into the fleet queue: one [[RescrapeTarget.FilmPages]] entry per film. */
class FacebookRescrapeQueue(store: FacebookRescrapeStore, country: String) extends ShareCardRescrapes with Logging {
  def request(filmId: String, notBefore: Instant): Unit =
    Try(store.add(Seq(RescrapeEntry(country, RescrapeTarget.FilmPages(country, filmId), notBefore)))).failed.foreach { e =>
      logger.warn(s"share card: re-scrape of $filmId not queued: ${e.getClass.getSimpleName}: ${e.getMessage}")
    }
}
