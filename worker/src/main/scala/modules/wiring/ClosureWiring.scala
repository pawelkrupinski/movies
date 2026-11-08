package modules.wiring

import modules.WorkerWiring

/** ── Venue closures ─────────────────────────────────────────────────────────
 *  The daily sweep that asks every venue its sources dropped whether it has closed. */
trait ClosureWiring { self: WorkerWiring =>

  // Once a day, every venue through `VenueClosure`. A venue its sources dropped is asked about
  // itself (its own site, a second listing): only one that shows it has closed pages once as a
  // closure and, for a data-driven roster (DE/ES/US), starts the retire-venues workflow, which
  // re-checks it live and opens the PR removing it. Without KINOWO_GITHUB_DISPATCH_TOKEN the
  // page asks for a hand retirement instead. One with no such evidence pages once as a coverage
  // gap and is listed on /uptime; it is never retired.
  lazy val closureLedger: services.closure.ClosureLedger =
    mongoConnection.database.fold[services.closure.ClosureLedger](new services.closure.InMemoryClosureLedger)(new services.closure.MongoClosureLedger(_))
  lazy val closureSweep = new services.closure.ClosureSweep(() => closureCandidates, scrapeArchive, filmwebFallbackStore,
    new services.closure.VenueChecks(services.scrapes.VenueWebsites.checkedIn, httpFetch), closureLedger, fallbackPager(services.alerts.TelegramAlertKind.VenueClosure),
    configuration.githubDispatchToken.map(token => new services.closure.GitHubRetirementDispatch(token,
      java.net.http.HttpClient.newBuilder().connectTimeout(java.time.Duration.ofSeconds(10)).sslContext(tlsContext).build())),
    clock)
  lazy val closureSchedule = managedResources.stopping(new services.tasks.ClaimedPeriodicTask("venue-closure", () => closureSweep.sweep(),
    WorkerWiring.ClosureSweepInterval, WorkerWiring.ClosureSweepInitialDelay, scheduledRunStore, clock, ticks))
}
