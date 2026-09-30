package services.identity

import models.Cinema
import services.scrapes.{ForwardingScrapeArchive, ScrapeArchiveRepository, SuccessfulScrape}

/** The scrape archive, every venue's archived listing also handed to the identity model as the
 *  venue's listings now ([[IdentityModelService.venueScraped]]) — the listings the model holds are
 *  exactly the archive's latest per venue (`ArchiveListings`), as the whole shadow read them. A
 *  barren attempt keeps the archived listing, so it moves nothing. Every read and archiving rule
 *  is the wrapped archive's. */
final class IdentityModelFeed(underlying: ScrapeArchiveRepository, model: IdentityModelService)
    extends ForwardingScrapeArchive(underlying) {

  override protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = {
    super.storeSuccess(cinema, city, scrape)
    model.venueScraped(cinema, scrape.films)
  }
}
