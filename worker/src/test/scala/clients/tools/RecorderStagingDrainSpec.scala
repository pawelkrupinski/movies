package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.FixtureTestWiring

/** Regression for the fixture recorder capturing ONLY cinema scrapes.
 *
 *  Staging ingest is always-on, so on a cold cache `recordCinemaScrape` diverts
 *  every scraped film to `pending_movies`. `RecordAllDataToFixture` once ran
 *  scrape → detail → drainServices and went straight to its enrichment sync-pass
 *  — but `movies` was still EMPTY (everything sat in staging), so the sync-pass
 *  had nothing to enrich and the recorder captured no TMDB/IMDb/MC/RT/Filmweb
 *  fixtures, only the cinema scrapes.
 *
 *  The fix folds the recorder and the replay harness onto one shared boot
 *  (`TestWiring.scrapeAndDrainToCache`), whose final `drainStaging` graduates
 *  newcomers into `movies`. This replays the canonical corpus through that exact
 *  sequence and asserts the cache ends up populated AND enriched — both fail if
 *  the staging drain is ever dropped from the shared sequence. */
class RecorderStagingDrainSpec extends AnyFlatSpec with Matchers {

  "scrapeAndDrainToCache (the recorder's shared boot)" should
    "graduate always-on-staging newcomers into an enriched movies cache" in {
    // One city off the canonical corpus — enough to exercise scrape → divert to
    // staging → drain → enrich, without booting all ~300 cinemas.
    val wiring = new FixtureTestWiring("08-06-2026") {
      override def scrapeCities: Set[String] = Set("poznan")
    }
    wiring.scrapeAndDrainToCache()

    val rows = wiring.movieCache.snapshot()
    // Without the drainStaging step the whole corpus stays in pending_movies and
    // this is empty — the exact "recorder captured only scrapes" symptom.
    rows should not be empty
    // The drained rows are actually enriched (promotion ran TMDB resolve), i.e.
    // the sync-pass the recorder runs next has real rows to record fixtures for.
    rows.count(_.record.tmdbId.isDefined) should be > 0
  }
}
