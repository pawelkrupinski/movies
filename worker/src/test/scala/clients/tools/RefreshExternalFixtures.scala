package clients.tools

import tools.HttpFetch

import scala.util.Try

/** Refreshes ONLY the external-metadata fixtures (Filmweb / Metacritic / Rotten
 *  Tomatoes / IMDb / TMDB) whose request URL CHANGED after the title→search-query
 *  pipeline change — without re-scraping or drifting the pinned `08-06-2026`
 *  cinema corpus.
 *
 *  Mechanism ([[RecordMissingFetch]]): the cinema scrapes replay from the pinned
 *  fixtures (a missing cinema fixture still throws, so the corpus can't grow or
 *  drift), the NEW pipeline computes each row's `searchTitle`, and the enrichment
 *  sync fires the resulting queries. A query that already has a fixture replays
 *  byte-identical (untouched — no drift to today's ratings); only a query with NO
 *  fixture, and only on an external-metadata host, hits the live API and is
 *  recorded. So the diff is exactly the handful of new-query fixtures.
 *
 *  Run (needs `TMDB_API_KEY` in the env; the other four are key-less):
 *
 *    eval "$(grep -E '^TMDB_API_KEY=' /path/to/.env.local | sed 's/^/export /')"
 *    sbt 'worker/Test/runMain clients.tools.RefreshExternalFixtures'
 *
 *  Then regenerate the snapshots (`PageSnapshotSpec`, `FilmScheduleEndToEndSpec`)
 *  and commit the new external fixtures alongside the production change. */
object RefreshExternalFixtures extends tools.FixtureTestWiring("08-06-2026") {
  // Live-record on a fixture miss ONLY for these external-metadata hosts; every
  // cinema host stays strict replay (a miss throws), pinning the corpus.
  private val ExternalHosts = Set(
    "themoviedb.org", "filmweb.pl", "rottentomatoes.com", "imdb.com", "metacritic.com"
  )

  override lazy val httoFetch: HttpFetch      = new RecordMissingFetch(fixture, ExternalHosts)
  override lazy val multikinoFetch: HttpFetch = httoFetch
  override lazy val biletynaFetch: HttpFetch  = httoFetch
  override lazy val tmdbClient: clients.TmdbClient =
    new clients.TmdbClient(httoFetch, apiKey = sys.env.get("TMDB_API_KEY"))

  def main(args: Array[String]): Unit = {
    scrapeAndDrainToCache()
    val rows = movieCache.snapshot()
    println(s"Refresh: enriching ${rows.size} rows (live only for missing external queries)…")
    val done = new java.util.concurrent.atomic.AtomicInteger(0)
    rows.foreach { row =>
      Try(fullySyncOne(row.title, row.year)) match {
        case scala.util.Success(_)  => done.incrementAndGet()
        case scala.util.Failure(ex) => println(s"  FAIL '${row.title}' (${row.year.getOrElse("?")}): ${ex.getMessage}")
      }
    }
    println(s"Refresh: done — ${done.get}/${rows.size} rows enriched.")
    movieRepository.close()
  }
}
