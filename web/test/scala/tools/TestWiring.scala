package tools

import clients.TmdbClient
import modules.Wiring
import play.api.Mode
import play.api.mvc.ControllerComponents
import play.api.test.Helpers.stubControllerComponents
import services.{MongoConnection, Stoppable}

trait TestWiring extends Wiring {
  override val controllerComponents: ControllerComponents = stubControllerComponents()

  override def environmentMode: Mode = Mode.Test

  // Scrape every city in tests. The recorded fixtures, page snapshots, and the
  // coverage spec all cover the full catalogue, so the production
  // KINOWO_SCRAPE_CITIES gate (default Poznań-only) must not narrow what tests
  // see — otherwise the snapshots/coverage fail for the gated-out cinemas.
  override def scrapeCities: Set[String] = Set("poznan", "wroclaw", "warszawa")

  // Pin a DISABLED Mongo connection. Tests get their movie data from
  // `InMemoryMovieRepo` / fixtures and don't exercise the user repos, so a real
  // Mongo is never needed — but the production `fromEnv` would still CONNECT to
  // whatever `MONGODB_URI` (`.env.local`) is reachable. With a developer's
  // `flyctl proxy 27017` tunnel up, that hydrated real PRODUCTION enrichment
  // into otherwise-hermetic snapshot / end-to-end specs, so a row's ratings
  // differed depending on whether the tunnel happened to be open — a flake that
  // never reproduced on CI (no tunnel there). Disabling it here makes every
  // test wiring deterministic regardless of the local environment.
  override lazy val mongoConnection: MongoConnection =
    new MongoConnection(uri = None, dbName = "kinowo", required = false)

  override implicit def materializer: org.apache.pekko.stream.Materializer = null

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
  def drainServices(): Unit = quiesce(cascadeDrainOrder*)
}
