package services.scrapes

import models.Cinema

/** A scrape archive that files every attempt in `underlying` and answers every read from it — the
 *  base of the archives that WATCH the scrapes going by (the identity model's feed, the gone-venue
 *  pager, the observation capture, the copied-feed detector) without storing anything themselves.
 *  A watcher overrides the hook it needs and calls `super` to file the attempt. */
abstract class ForwardingScrapeArchive(underlying: ScrapeArchiveRepository) extends ScrapeArchiveRepository {
  override def enabled: Boolean = underlying.enabled

  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit =
    underlying.record(ScrapeAttempt(cinema, city, scrape.at, scrape.listingComplete, scrape.films))

  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit =
    underlying.record(ScrapeAttempt(cinema, city, attempt.at, listingComplete = true, films = Nil, error = attempt.error,
      noScheduleListed = attempt.noScheduleListed))

  override def find(cinema: Cinema): Option[ArchivedScrape]       = underlying.find(cinema)
  override def scan(consume: Seq[ArchivedScrape] => Unit): Boolean = underlying.scan(consume)
  override def scanVenues(keep: Cinema => Boolean)(consume: Seq[ArchivedScrape] => Unit): Boolean =
    underlying.scanVenues(keep)(consume)
  override def contentStamps(): Map[String, ContentStamp]         = underlying.contentStamps()
  override def close(): Unit                                      = underlying.close()
}
