package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.readmodel.ReadModelProjectionMetrics

/** The read-model projection's series: the documents it writes, prunes, retires, heals and re-projects,
 *  and how long a projection takes. Every series is seeded to 0 for each country at construction. */
final class ReadModelSeries(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val readModelWrites = Counter.builder()
    .name("kinowo_worker_readmodel_writes")
    .help("Denormalised read-model documents (re)written since boot, by country, target (movie|screening) and op (upsert|delete). rate() is the reprojection churn the worker pushes through the web's web_movies/web_screenings change streams.")
    .labelNames("country", "target", "op")
    .register(registry)

  private val readModelFilmsPruned = Counter.builder()
    .name("kinowo_worker_readmodel_films_pruned")
    .help("Cards the read-model PRUNE sweep removed since boot, by country and reason: row-gone = no movies document projects to that id any more; variant-gone = the row lives but no longer projects to that variant id (a decorated listing that vanished). The rule (2026-09-07) is that this stays at ZERO — the change-stream path retires every card it no longer produces (see readmodel_cards_retired) — so a sustained rate is a projection defect to chase by its reason, never accepted churn. Pair with kinowo_worker_merges_total for a row-gone cause.")
    .labelNames("country", "reason")
    .register(registry)

  private val readModelCardsRetired = Counter.builder()
    .name("kinowo_worker_readmodel_cards_retired")
    .help("Cards the read-model CHANGE-STREAM path retired on its own since boot, by country and reason: variant-gone = a re-projected row no longer produces that variant card (its decorated listing vanished); row-deleted = the row was deleted or merged away (a delete event); row-unready = a re-projected row lost its readiness. These are the removals the prune used to do 30 minutes late; the prune counter is what is LEFT after them.")
    .labelNames("country", "reason")
    .register(registry)

  private val readModelCatchUpRows = Counter.builder()
    .name("kinowo_worker_readmodel_catchup_rows")
    .help("Source rows the read-model prune sweep re-projected because they were written AFTER the movies change-stream cursor's last delivered event, by country, since boot. The catch-up for a cursor that is open and silent: a live cursor delivers a write within seconds, so this stays at zero; a sustained rate is the read-model side of ChangeStreamMoviesCursorSilent and means the site was up to 30 minutes stale between sweeps.")
    .labelNames("country")
    .register(registry)

  private val readModelHeals = Counter.builder()
    .name("kinowo_worker_readmodel_heals")
    .help("Ready source rows re-projected because the read model lacked one of their cards or venues, by country and trigger (sweep=the 30-min prune sweep, boot=the check start() runs, refile=a boot row whose every missing venue the read model serves under ANOTHER city: a deploy moved the venue, no write was lost — not counted as boot). Each is a row the change-stream path should already have written — a card it retired and never restored, a slot event it missed — so ZERO is the healthy reading and a recurring rate is a stream-path defect: on 2026-09-22 a TMDB re-try made rows briefly unready (readmodel_cards_retired{reason=row-unready} ~500/day) and only these heals, ~26 a day and 30 minutes late each, put the cards back. Alerted by ReadModelHealsRecurring.")
    .labelNames("country", "trigger")
    .register(registry)

  private val readModelCardWrites = Counter.builder()
    .name("kinowo_worker_readmodel_card_writes")
    .help("web_movies card documents written since boot, by country and cause. new=no card existed; one of title|poster|facts|synopsis|synopsis-by-city|ratings|trailers|age-rating=exactly that part of the card moved (facts = runtime, year, genres, countries, directors, cast); multiple=more than one part moved (see readmodel_card_rewrite_parts for which). The synopsis-by-city line answers whether moving that map (44% of card bytes in the fixture read model) to its own collection would spare the read model any card rewrites: near zero means the split saves bytes per document but not writes.")
    .labelNames("country", "cause")
    .register(registry)

  private val readModelCardRewriteParts = Counter.builder()
    .name("kinowo_worker_readmodel_card_rewrite_parts")
    .help("Card parts that differed from the card written before, one increment per part per rewrite, by country and part — the decomposition of readmodel_card_writes{cause=multiple}. A rewrite that moved ratings and the poster counts once under each.")
    .labelNames("country", "part")
    .register(registry)

  private val readModelDriftWrites = Counter.builder()
    .name("kinowo_worker_readmodel_drift_writes")
    .help("Read-model documents the rolling CONTENT check rewrote since boot, by country. Every other sweep compares IDS — the prune removes a card whose row is gone, the heal writes one that is missing — and none of them can see a row that exists and is WRONG: three UK films held showtimes from August until 2026-09-08, served to users and unreachable by the change stream because their source had stopped changing. One slice of the corpus is re-projected per prune sweep (one of 48 slices, so the whole corpus once a day, plus a whole-corpus pass when the projection's derivation version changes) and the projection's own diff writes only what drifted. Zero is the healthy reading; a sustained rate means the incremental path is losing writes. A rewrite of a row the change stream then applies itself — its event in flight behind the sweep's lock or the stream's debounce — is not drift and is not counted (logged at INFO as 'not drift'); until 2026-10-07 it was, and most of the US's 10-50 a day were that race. cause says what drifted: a card's readmodel_card_writes cause (new, the one part that moved, or multiple) or screening-upsert / screening-delete for a venue's row; the worker logs each drifted row's card and screening ids on a 'read-model content check: row … had drifted' line.")
    .labelNames("country", "cause")
    .register(registry)

  private val readModelUnlistedVenues = Counter.builder()
    .name("kinowo_worker_readmodel_unlisted_venues")
    .help("Venue rows a prune sweep found under a live card whose film no longer lists the venue, by country and outcome: `pruned`, or `withheld` when one sweep found more than its cap (20, or 1% of the venue rows) and pruned none -- a disagreement between the slots-only view and the projection being likelier than that many missed removals. Each is a removal the change stream did not apply; before this sweep only the daily content check found them (Dracula: A Love Tale served at the Music Box Chicago for 22 hours after leaving it, 2026-09-30). Zero is the healthy reading; any `withheld` is a bug to look at.")
    .labelNames("country", "outcome")
    .register(registry)

  private val readModelProjectDuration = Histogram.builder()
    .name("kinowo_worker_readmodel_project_duration_seconds")
    .help("Wall-clock of one pure ReadModelProjection.projectAll per source row since boot, by country — the LATENCY signal (percentiles, the duration heatmap). NOT a CPU share: concurrent projections make rate(_sum) exceed one core-second per second, and steal on a throttled box inflates it further. Use kinowo_worker_readmodel_project_cpu_seconds_total for CPU attribution.")
    .labelNames("country")
    .classicUpperBounds(WorkerTaskMetrics.ProjectBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val readModelProjectCpu = Counter.builder()
    .name("kinowo_worker_readmodel_project_cpu_seconds")
    .help("Thread CPU seconds burned inside pure ReadModelProjection.projectAll since boot, by country. rate()*100 is projection's true share of worker CPU in centi-cores — the credit-floor driver the CPU-drivers dashboard stacks against JIT, GC and everything else. Unlike the wall-clock histogram this is immune to concurrency and to steal, so it can be compared against process_cpu_seconds_total.")
    .labelNames("country")
    .register(registry)

  private val readModelWriteBurst = Histogram.builder()
    .name("kinowo_worker_readmodel_write_burst_seconds")
    .help("Wall-clock of the WRITE half of one project() call since boot, by country — writer.upsertMovie/diffScreenings for a single source row, once its variants are computed. Separate from readmodel_project_duration_seconds, which times only the computation before any write. On 2026-09-08 a wide UK release took up to ~40 minutes to reach all 73 of its cities after its source row turned ready, with nothing distinguishing computation, writing, or change-stream delivery as the slow part — this histogram is the write-phase half of that answer; a long write burst against a short project_duration for the same call points at the write loop (city count, Mongo round-trips), not at resolve/synopsisByCity/ratings. trigger is readmodel_project_calls' trigger: a heal, a derivation pass or the backfill writes a film's venues whole, so read the incremental path's cost on trigger=stream (ReadModelProjectionWritePerCallHigh does).")
    .labelNames("country", "trigger")
    .classicUpperBounds(WorkerTaskMetrics.DurationBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val readModelProjectCalls = Counter.builder()
    .name("kinowo_worker_readmodel_project_calls")
    .help("Source rows projected (projectAll invoked) since boot, by country and trigger — the throughput denominator for readmodel_project_duration_seconds. trigger=stream is a change-stream event (any of the three cursors) and the only share ReadModelProjectionTriggerUnaccounted compares against the cursors' events; heal|catch-up|content|derivation|share-card|hold-release|reproject re-project rows no event asked for, by design. derivation is the whole corpus once per worker after a new derivation (ReadModelDerivation.History — a change to what the projection derives, cards-only or full); until 2026-09-26 every read-model snapshot regeneration counted, and five such deploys inside an hour held DE/UK/PL at 1-2/s over their events for an hour.")
    .labelNames("country", "trigger")
    .register(registry)

  private val readModelMetadataProjections = Counter.builder()
    .name("kinowo_worker_readmodel_metadata_projections")
    .help("Projections by country and whether the metadata half (resolve/synopsisByCity/ratingsFor) was REUSED from the per-film cache (outcome=reused — a showtime-only change at an already-present cinema, only the cheap screenings half re-ran) or RECOMPUTED (outcome=recomputed — a rating/synopsis/new-cinema change, or a first projection). rate(reused) / rate(reused+recomputed) is opt-1's hit ratio — high reuse under reproject/enrich showtime churn is the CPU win. trigger is readmodel_project_calls' trigger: read the ratio on trigger=stream, because the sweeps (content above all — 1/48 of the corpus every 30-minute sweep) re-project rows a worker restarted by the last deploy has never cached, so they recompute by construction and, unsplit, drew half-hourly spikes that read as reuse collapsing.")
    .labelNames("country", "trigger", "outcome")
    .register(registry)

  private val readModelVenueProjections = Counter.builder()
    .name("kinowo_worker_readmodel_venue_projections")
    .help("Venues' screenings rows a projection considered, by country and whether the row was REBUILT (outcome=rebuilt — the venue's slots moved since the row was written, or this process never wrote it) or kept unbuilt (outcome=reused). A showtime change at one venue of a wide release is one rebuilt against thousands reused.")
    .labelNames("country", "outcome")
    .register(registry)

  private val readModelReconcileSweeps = Counter.builder()
    .name("kinowo_worker_readmodel_reconcile_sweeps")
    .help("Read-model orphan-prune sweeps since boot (kind=prune, the cheap id-only prune that removes deleted/re-keyed rows), by country and did_work (true=pruned >=1 doc, false=no-op). A prune with did_work=true is the deletes/re-keys the change stream can't deliver. (The full re-projection sweep was retired, so kind is always prune now.)")
    .labelNames("country", "kind", "did_work")
    .register(registry)

  countryCodes.foreach { c =>
    ReadModelProjectionMetrics.Targets.foreach(t =>
      ReadModelProjectionMetrics.Ops.foreach(o => readModelWrites.labelValues(c, t, o)))
    // Materialize at 0 so Grafana draws a continuous line — and for the prune, so the
    // line that MUST stay at zero is visibly at zero rather than absent (the rule).
    ReadModelProjectionMetrics.PruneReasons.foreach(r => readModelFilmsPruned.labelValues(c, r).inc(0.0))
    ReadModelProjectionMetrics.RetireReasons.foreach(r => readModelCardsRetired.labelValues(c, r).inc(0.0))
    ReadModelProjectionMetrics.DriftCauses.foreach(cause => readModelDriftWrites.labelValues(c, cause).inc(0.0)) // zero is the healthy reading, so it must be drawn
    Seq("pruned", "withheld").foreach(o => readModelUnlistedVenues.labelValues(c, o).inc(0.0)) // ditto
    readModelCatchUpRows.labelValues(c).inc(0.0) // ditto — zero is the healthy reading, so it must be drawn
    ReadModelProjectionMetrics.HealTriggers.foreach(t => readModelHeals.labelValues(c, t).inc(0.0)) // ditto
    // Every trigger at 0 from boot: ReadModelProjectionTriggerUnaccounted reads `stream` alone,
    // and a series that does not exist yet makes it silent rather than compare against zero.
    ReadModelProjectionMetrics.ProjectTrigger.values.foreach(t => readModelProjectCalls.labelValues(c, t.label).inc(0.0))
    readModelProjectCpu.labelValues(c).inc(0.0)       // ditto — the CPU-attribution counter the drivers panel stacks
    readModelProjectDuration.labelValues(c).observe(0.0) // materialize the histogram (_sum/_count/_bucket) from boot — no Grafana gap
    ReadModelProjectionMetrics.ProjectTrigger.values.foreach(t => readModelWriteBurst.labelValues(c, t.label).observe(0.0)) // ditto — the write-phase half of that same answer
    ReadModelProjectionMetrics.ProjectTrigger.values.foreach(t =>
      ReadModelProjectionMetrics.MetadataOutcomes.foreach(o => readModelMetadataProjections.labelValues(c, t.label, o)))
    ReadModelProjectionMetrics.VenueOutcomes.foreach(o => readModelVenueProjections.labelValues(c, o))
    ReadModelProjectionMetrics.ReconcileKinds.foreach(k =>
      Seq("true", "false").foreach(w => readModelReconcileSweeps.labelValues(c, k, w)))
    ReadModelProjectionMetrics.CardWriteCauses.foreach(cause => readModelCardWrites.labelValues(c, cause))
    ReadModelProjectionMetrics.CardParts.foreach(part => readModelCardRewriteParts.labelValues(c, part))
  }

  def recordWrite(country: String, target: String, op: String, count: Int): Unit =
    if (count > 0) readModelWrites.labelValues(country, target, op).inc(count.toDouble)

  def recordFilmPruned(country: String, reason: String, count: Int): Unit =
    if (count > 0) readModelFilmsPruned.labelValues(country, reason).inc(count.toDouble)

  def recordCardRetired(country: String, reason: String): Unit =
    readModelCardsRetired.labelValues(country, reason).inc()

  def recordUnlistedVenues(country: String, rows: Int, withheld: Boolean): Unit =
    if (rows > 0) readModelUnlistedVenues.labelValues(country, if (withheld) "withheld" else "pruned").inc(rows.toDouble)

  def recordDrift(country: String, cause: String, documents: Int): Unit =
    if (documents > 0) readModelDriftWrites.labelValues(country, cause).inc(documents.toDouble)

  def recordCatchUp(country: String, rows: Int): Unit =
    if (rows > 0) readModelCatchUpRows.labelValues(country).inc(rows.toDouble)

  def recordHeal(country: String, trigger: String, rows: Int): Unit =
    if (rows > 0) readModelHeals.labelValues(country, trigger).inc(rows.toDouble)

  def recordCardWrite(country: String, changed: Set[String]): Unit = {
    readModelCardWrites.labelValues(country, ReadModelProjectionMetrics.cardWriteCause(changed)).inc()
    changed.foreach(part => readModelCardRewriteParts.labelValues(country, part).inc())
  }

  def recordProject(country: String, trigger: ReadModelProjectionMetrics.ProjectTrigger, wallSeconds: Double, cpuSeconds: Double): Unit = {
    readModelProjectDuration.labelValues(country).observe(math.max(0.0, wallSeconds))
    readModelProjectCpu.labelValues(country).inc(math.max(0.0, cpuSeconds))
    readModelProjectCalls.labelValues(country, trigger.label).inc()
  }

  def recordWriteBurst(country: String, trigger: ReadModelProjectionMetrics.ProjectTrigger, seconds: Double): Unit =
    readModelWriteBurst.labelValues(country, trigger.label).observe(math.max(0.0, seconds))

  def recordMetadataProjection(country: String, trigger: ReadModelProjectionMetrics.ProjectTrigger, reused: Boolean): Unit =
    readModelMetadataProjections.labelValues(country, trigger.label,
      if (reused) ReadModelProjectionMetrics.MetadataOutcome.Reused
      else ReadModelProjectionMetrics.MetadataOutcome.Recomputed).inc()

  def recordVenueProjection(country: String, rebuilt: Int, reused: Int): Unit = {
    if (rebuilt > 0) readModelVenueProjections.labelValues(country, ReadModelProjectionMetrics.VenueOutcome.Rebuilt).inc(rebuilt.toDouble)
    if (reused > 0)  readModelVenueProjections.labelValues(country, ReadModelProjectionMetrics.VenueOutcome.Reused).inc(reused.toDouble)
  }

  def recordReconcileSweep(country: String, kind: String, didWork: Boolean): Unit =
    readModelReconcileSweeps.labelValues(country, kind, didWork.toString).inc()
}