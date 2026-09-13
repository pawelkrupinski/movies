package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.FalenicaClient

/**
 * One-off, narrowly-scoped refresh of ONLY the Falenica slice of the frozen
 * `08-06-2026` whole-corpus fixture (see `RecordAllDataToFixture`), after the
 * site's 2026-09 `falenica3` theme redesign made the old capture unparseable
 * by the updated `FalenicaClient` selectors. Unlike `RecordAllDataToFixture`,
 * this does NOT run the full production pipeline (no TMDB/IMDb/enrichment,
 * no proxy chain) — it only re-fetches this one cinema's own pages, which are
 * plain unprotected HTTP, directly reachable without Zyte/Decodo.
 */
object RefreshFalenicaCorpusFixture {
  def main(args: Array[String]): Unit = {
    new FalenicaClient(new RecordingHttpFetch("08-06-2026", new RealHttpFetch())).fetch().foreach(println)
  }
}
