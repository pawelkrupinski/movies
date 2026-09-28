package services.identity

import models.Cinema
import services.scrapes.{ArchivedScrape, BarrenAttempt, ScrapeArchiveRepository, ScrapeAttempt, SuccessfulScrape}

import java.time.Instant

/** The scrape archive, every venue's archived listing also handed to the identity model as the
 *  venue's listings now ([[IdentityModelService.venueScraped]]) — the listings the model holds are
 *  exactly the archive's latest per venue (`ArchiveListings`), as the whole shadow read them. A
 *  barren attempt keeps the archived listing, so it moves nothing. Every read and archiving rule
 *  is the wrapped archive's. */
final class IdentityModelFeed(underlying: ScrapeArchiveRepository, model: IdentityModelService) extends ScrapeArchiveRepository {

  override def enabled: Boolean = underlying.enabled

  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = {
    underlying.record(ScrapeAttempt(cinema, city, scrape.at, scrape.listingComplete, scrape.films))
    model.venueScraped(cinema, scrape.films)
  }

  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit =
    underlying.record(ScrapeAttempt(cinema, city, attempt.at, listingComplete = true, films = Nil, error = attempt.error))

  override def find(cinema: Cinema): Option[ArchivedScrape]      = underlying.find(cinema)
  override def scan(consume: Seq[ArchivedScrape] => Unit): Boolean = underlying.scan(consume)
  override def lastContentAt(): Map[String, Option[Instant]]    = underlying.lastContentAt()
  override def close(): Unit                                    = underlying.close()
}
