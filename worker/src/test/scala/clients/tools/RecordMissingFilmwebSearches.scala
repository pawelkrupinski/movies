package clients.tools

import tools.{FixtureTestWiring, HttpFetch, RealHttpFetch}

/**
 * Targeted fixture recorder for the Filmweb `live/search` GAP only.
 *
 * Changing the Filmweb search query to a case/diacritic-folded form (FilmwebClient
 * `deburr`) re-fingerprints EVERY `live/search.<hash>` fixture, orphaning the old
 * mixed-case ones. `RefreshExternalFixtures` can't refill them — it refreshes
 * ratings by the STORED `film/{id}` URL and never searches. `RecordAllDataToFixture`
 * would, but it re-records the WHOLE corpus (TMDB/IMDb/MC/RT + every cinema scrape)
 * with today's data — a huge, needless drift.
 *
 * This boots the corpus through the SAME replay path the e2e snapshot uses
 * (`FixtureTestWiring.bootStartup` + `converge`, so the Filmweb SEARCH path actually
 * fires per row) but over a `RecordMissingFetch` scoped to `filmweb.pl`: every
 * non-Filmweb call AND every by-id `film/{id}/…` call is served from the existing
 * 08-06 fixtures untouched; ONLY a missing `live/search` (the new normalized query)
 * hits the network and lands on disk. Ratings (by-id) stay pinned; only the
 * search→id mapping is refreshed.
 *
 * Usage (delete the orphaned search fixtures first so they're "missing"):
 *   find test/resources/fixtures/08-06-2026/www.filmweb.pl -path '*live/search*' -delete
 *   KINOWO_FIXTURE_DIR=08-06-2026 sbt 'worker/Test/runMain clients.tools.RecordMissingFilmwebSearches'
 */
object RecordMissingFilmwebSearches {
  def main(args: Array[String]): Unit = {
    val dir = tools.Env.get("KINOWO_FIXTURE_DIR").getOrElse("08-06-2026")
    val recording = new RecordMissingFetch(dir, Set("filmweb.pl"), new RealHttpFetch())
    val w = new FixtureTestWiring(dir) {
      override lazy val httoFetch:      HttpFetch = recording
      override lazy val multikinoFetch: HttpFetch = recording
      override lazy val biletynaFetch:  HttpFetch = recording
    }
    println("Filmweb-search recorder: booting corpus (replay) + converge (re-enrich → search)…")
    w.bootStartup()
    w.converge()
    w.movieRepository.close()
    println("Filmweb-search recorder: done.")
  }
}
