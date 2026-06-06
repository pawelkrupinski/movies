package tools

import clients.TmdbClient
import models.City
import modules.WorkerWiring
import services.{MongoConnection, Stoppable}

/** Test seam over the worker's [[WorkerWiring]] composition root: pins a
 *  disabled Mongo, a stub TMDB key, and the full city catalogue so fixture
 *  replay and the coverage spec see every cinema. The serving-side seams
 *  (controllerComponents, materializer, environmentMode) are gone — the worker
 *  is a plain `def main` app, not Play, so they no longer exist to override. */
trait TestWiring extends WorkerWiring {

  // Scrape every city in tests. The recorded fixtures and the coverage spec
  // cover the full catalogue, so the production KINOWO_SCRAPE_CITIES gate
  // (default Poznań-only) must not narrow what tests see — otherwise the
  // coverage spec fails for the gated-out cinemas. Derived from `City.all` so a
  // newly modelled city is covered automatically, without re-spelling the list.
  override def scrapeCities: Set[String] = City.all.map(_.slug).toSet

  // Pin a DISABLED Mongo connection. Tests get their movie data from
  // `InMemoryMovieRepo` / fixtures and don't exercise the user repos, so a real
  // Mongo is never needed — but the production `fromEnv` would still CONNECT to
  // whatever `MONGODB_URI` (`.env.local`) is reachable. With a developer's
  // `flyctl proxy 27017` tunnel up, that hydrated real PRODUCTION enrichment
  // into otherwise-hermetic end-to-end specs, so a row's ratings differed
  // depending on whether the tunnel happened to be open — a flake that never
  // reproduced on CI (no tunnel there). Disabling it here makes every test
  // wiring deterministic regardless of the local environment.
  override lazy val mongoConnection: MongoConnection =
    new MongoConnection(uri = None, dbName = "kinowo", required = false)

  // Inject a stub TMDB API key so the test doesn't depend on a `TMDB_API_KEY`
  // env var. `TmdbClient.search` short-circuits to `None` when the key is
  // absent — without an override, every CI runner (and any local box without
  // `.env.local`) sees no TMDB resolution, no TmdbResolved events, no
  // enrichment cascade. The fixture replay doesn't need a real key (the URL's
  // `api_key` query param is stripped from the fixture fingerprint via
  // `RecordingHttpFetch.stableQueryFingerprint`), so any non-empty string works.
  override lazy val tmdbClient: TmdbClient = new TmdbClient(httoFetch, apiKey = Some("test-api-key"))

  /** Synchronously force one title all the way through the enrichment cascade:
   *  TMDB resolve → IMDb id recovery → the four `*Ratings.refreshOneSync` URL
   *  discovery + rating scrapes. Idempotent — safe to call after the bus-driven
   *  path; already-resolved rows re-hit the same URLs (so `RecordingHttpFetch`
   *  overwrites each fixture with byte-identical content). Test/tooling-only:
   *  the fixture recorder uses it as a belt-and-braces pass so no row is left
   *  half-enriched by an async retry that outlived the drain. */
  def fullySyncOne(title: String, year: Option[Int]): Unit = {
    if (movieService.get(title, year).flatMap(_.tmdbId).isEmpty) movieService.reEnrichSync(title, year)
    for {
      row <- movieService.get(title, year)
      _   <- row.tmdbId if row.imdbId.isEmpty
    } imdbIdResolver.resolveSync(title, year, row.originalTitle.getOrElse(title))
    imdbRatings.refreshOneSync(title, year)
    rottenTomatoesRatings.refreshOneSync(title, year)
    metascoreRatings.refreshOneSync(title, year)
    filmwebRatings.refreshOneSync(title, year)
  }

  def quiesce(stoppables: Stoppable*): Unit =
    stoppables.foreach(_.stop())

  /** Drain the event-cascade worker pools so every `TmdbResolved` /
   *  `ImdbIdMissing` / `ImdbIdResolved` published during the scrape is processed
   *  end to end. Uses the same `cascadeDrainOrder` production shutdown does —
   *  single source of truth for the producer→consumer ordering. */
  def drainServices(): Unit = quiesce(cascadeDrainOrder*)
}
