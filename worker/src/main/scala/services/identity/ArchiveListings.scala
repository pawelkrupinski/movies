package services.identity

import models.Cinema
import services.movies.TitleNormalizer
import services.scrapes.ScrapeArchiveRepository

/** The shadow run's listing set: every listing of the scrape archive's latest scrape per live venue
 *  (`Listing.corpus` over the archive's live rows).
 *
 *  Built a page at a time: each page of archive rows becomes its listings before the next is read,
 *  so the rows' parsed films — every showtime of every venue, ~580 MB of live heap on the US corpus
 *  against ~70 MB of listings — are never all held at once. Empty when the archive could not be
 *  read whole: a partial archive is not a smaller one. */
object ArchiveListings {

  def read(archive: ScrapeArchiveRepository, live: Cinema => Boolean, normalizer: TitleNormalizer): Seq[Listing] = {
    val listings = Seq.newBuilder[Listing]
    val complete = archive.scan { page =>
      listings ++= Listing.distinct(Listing.all(page.filter(row => live(row.cinema)).map(row => row.cinema -> row.films), normalizer))
    }
    if (complete) Listing.distinct(listings.result()) else Seq.empty
  }
}
