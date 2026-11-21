package services.tasks

import services.cinemas.common.{ChunkPlan, ChunkedCinemaScraper, CinemaScraper, ListingStructure, PreScrapedCinemaScraper}

/**
 * A chunked venue's listing — the reduce of its run, or the empty one a plan with no days in the horizon
 * makes — PUBLISHED through the same recording/fallback path a normal scrape uses (`publishScrape` =
 * `CinemaScrapeRunner.run` ∘ the uptime/Filmweb-fallback decorator), so uptime classification, the fallback
 * and the archive write are all reused unchanged, and stamped by the [[ScrapeFreshnessPolicy]] every
 * chunked outcome shares, so it advances the due schedule by one rule.
 */
final class ChunkedListingPublisher(publishScrape: CinemaScraper => Unit, scrapeFreshness: ScrapeFreshnessPolicy) {

  /** Publish `scraper`'s reduced listing `movies`, the WHOLE listing when `whole`, and stamp it with the
   *  runway it leaves. A publish failure is raised, and nothing is stamped. */
  def publishReduced(scraper: ChunkedCinemaScraper, movies: Seq[models.CinemaMovie], whole: Boolean): Unit = {
    publishScrape(PreScrapedCinemaScraper.of(scraper, () => movies, structure = ListingStructure.wholeWhen(whole)))
    scrapeFreshness.scraped(ScrapeFreshnessPolicy.scrapeKey(scraper.cinema), scraper.cinema, movies)
  }

  /** Publish `scraper`'s empty `plan`, read whole when `complete`: a venue that advertised no days in the
   *  horizon. It bypasses the terminal reduce, so it records the outcome against the cinema's due schedule
   *  ITSELF — an empty plan is a successful scrape of an empty repertoire and stamps freshness like any
   *  other. Omitting that was the German outage: a venue advertising zero showtime days never got stamped,
   *  so it read as never-fetched, sorted to the front of every [[ScrapeReaper]] tick, and ~40 of them ate the
   *  entire per-tick budget for days while ~1000 working cinemas were never enqueued. (Uptime still records
   *  it white — empty is visible, just not overdue.) Whether the page SAID it has nothing on travels with it
   *  to the archive (`emptyListing`). As for [[publishReduced]], a publish failure is raised and nothing is
   *  stamped: the venue stays due for the next tick. */
  def publishEmpty(scraper: ChunkedCinemaScraper, plan: ChunkPlan, complete: Boolean): Unit = {
    publishScrape(PreScrapedCinemaScraper.of(scraper, () => Seq.empty, ListingStructure.wholeWhen(complete), plan.emptyListing))
    // Deliberately NOT fed to VenueScrapeCadence: that mechanism is for a venue
    // with SOME showtimes about to run dry, not one advertising nothing at all.
    // A seasonally-closed venue (a drive-in shut all winter, a site between
    // programmes) can sit here for weeks; the MinInterval floor would hammer its
    // site every 30 minutes for as long as that lasts, for no benefit — the country
    // default already re-checks it often enough to notice when it reopens.
    scrapeFreshness.succeeded(ScrapeFreshnessPolicy.scrapeKey(scraper.cinema))
  }
}
