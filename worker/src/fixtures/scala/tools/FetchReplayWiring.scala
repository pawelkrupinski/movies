package tools

import clients.TmdbClient
import models.Country
import services.scrapes.ArchivedScrape

import java.time.{Clock, ZoneOffset}

/**
 * The archive replay over ONE given fetch — every cinema detail and every external lookup
 * answered by `fetch` and nothing else — seeded with `rows`. Shared by the specs that replay a
 * corpus against recorded answers: the hard-cluster convergence spec and the identity query
 * coverage gate, which must ask exactly the questions the replay asks.
 */
object FetchReplayWiring {

  def apply(country: Country, storage: ConvergenceStorage, rows: Seq[ArchivedScrape], fetch: HttpFetch,
            fixtureRoot: settings.FixtureRoot = settings.FixtureRoot.RepositoryRelative,
            clock: Clock = Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC),
            retrySleep: Long => Unit = Thread.sleep, environment: Env = Env.of()): ArchiveReplayWiring = {
    CorpusFixture.seedInto(storage.archive, rows)
    val language = country.language
    val fixed    = clock
    // The tree is never reached (every answer comes from `fetch`); it is named only because the
    // wiring requires one.
    new ArchiveReplayWiring(country, storage.archive, None, storage, s"enrichment-${country.code}", fixtureRoot,
      environment = environment) {
      override lazy val clock: java.time.Clock = fixed
      // Ordering, not timing: the whole cascade on the calling thread, so the only
      // nondeterminism left is the seeded arrival order.
      override lazy val backgroundBudget: ExecutionBudget = new SameThreadExecutionBudget
      override lazy val httoFetch: HttpFetch       = fetch
      override lazy val enrichmentFetch: HttpFetch = fetch
      // Held in memory: its daemon flusher outlives the pass and would re-create the
      // pass's database after `afterAll` dropped it. Uptime has no part in the claims.
      override lazy val uptimeMonitor = new services.UptimeMonitor(None, clock = clock)
      // A stub key: the answers are replayed, and a keyless client short-circuits
      // before it reaches the fetch at all.
      override def tmdbClientOver(http: HttpFetch): TmdbClient =
        new TmdbClient(http, apiKey = Some(settings.TmdbApiKey("replay")), language = language, retrySleep = retrySleep)
    }
  }
}
