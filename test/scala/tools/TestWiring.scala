package tools

import clients.TmdbClient
import modules.Wiring
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
import services.Stoppable

trait TestWiring extends Wiring {
  override val controllerComponents: ControllerComponents = stubControllerComponents()

  override def environmentMode: Mode = Mode.Test

  // Inject a stub TMDB API key so the test doesn't depend on a
  // `TMDB_API_KEY` env var. `TmdbClient.search` short-circuits to `None`
  // when the key is absent — without an override, every CI runner (and
  // any local box without `.env.local`) sees no TMDB resolution, no
  // TmdbResolved events, no enrichment cascade, and an empty
  // `pradaSchedules`. The fixture replay doesn't actually need a real
  // key (the URL's `api_key` query param is stripped from the fixture
  // fingerprint via `RecordingHttpFetch.stableQueryFingerprint`), so
  // any non-empty string works.
  override lazy val tmdbClient: TmdbClient = new TmdbClient(httoFetch, apiKey = Some("test-api-key"))

  def quiesce(stoppables: Stoppable*): Unit =
    stoppables.foreach(_.stop())

  /** Drain the event-cascade worker pools so every `TmdbResolved` /
   *  `ImdbIdMissing` / `ImdbIdResolved` published during the scrape is
   *  processed end to end (URL discovery + rating scrape for MC/RT/FW,
   *  IMDb GraphQL + suggestion lookup, TMDB search + external_ids /
   *  credits / details). Uses the same `cascadeDrainOrder` production
   *  shutdown does — single source of truth for the producer→consumer
   *  ordering. */
  def drainServices(): Unit = quiesce(cascadeDrainOrder: _*)
}
