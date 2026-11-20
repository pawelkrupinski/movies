package services.closure

import java.time.Instant

import models.ArchivedScrape
import services.fallback.FallbackStore
import services.scrapes.ScrapeArchiveReader
import services.scrapes.VenueClosure.SourceDrop

/** What the scrape archive says of each candidate: whether its sources dropped it ([[ClosureCandidate.dropped]]),
 *  and whether the gone-venue alert already paged its current page-gone spell. Read with `scan`, never `findAll`:
 *  only the verdicts are kept, not every venue's parsed listing. */
final class ClosureDrops(archive: ScrapeArchiveReader, fallbacks: FallbackStore) {

  /** Each candidate's drop and the names whose spell was paged as gone -- `None` when the archive could not be read in
   *  full: a venue the read never reached is not a venue with no evidence. */
  def read(byName: Map[String, ClosureCandidate], now: Instant): Option[(Map[String, Either[String, SourceDrop]], Set[String])] = {
    val drops     = collection.mutable.Map.empty[String, Either[String, SourceDrop]]
    val pagedGone = collection.mutable.Set.empty[String]
    val complete  = archive.scan(_.foreach { (row: ArchivedScrape) =>
      byName.get(row.cinema.displayName).foreach { c =>
        drops(c.name) = c.dropped(row, fallbacks.get(c.name), now)
        if (c.pagedAsGone(row)) pagedGone += c.name
      }
    })
    Option.when(complete.isComplete)((drops.toMap, pagedGone.toSet))
  }
}
