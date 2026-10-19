package services.cinemas.roster

import models.Cinema
import services.scrapes.{ForwardingScrapeArchive, ScrapeArchiveRepository, SuccessfulScrape}

/** The scrape archive, every venue's archived listing also handed to [[CopiedFeedDetector]] as it
 *  lands: the one path every scrape takes, whether or not its country writes `movies`. */
final class CopiedFeedArchive(underlying: ScrapeArchiveRepository, detector: CopiedFeedDetector)
    extends ForwardingScrapeArchive(underlying) {

  override protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = {
    super.storeSuccess(cinema, city, scrape)
    detector.venueScraped(cinema, scrape.films)
  }
}
