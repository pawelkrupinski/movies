package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.movies.{ChangeStreamLiveness, ChangeStreamMetrics, ScreeningsMetrics}

import java.time.Instant

/** The change-stream series: each cursor's liveness (last delivery, apply backlog and lag), and what
 *  the `movies`, `screenings` and `movie_slots` cursors delivered and applied. Every series is seeded
 *  to 0 for each country at construction. */
final class ChangeStreamSeries(countryCodes: Seq[String], registry: PrometheusRegistry) {

  // A GAUGE COMPUTED AT SCRAPE TIME, never a stored age: the value is `now - lastDelivered`
  // on every scrape, so a cursor that stalls draws a straight diagonal instead of freezing
  // at whatever it last said. The event COUNTERS beside it cannot tell a stalled cursor
  // from a quiet night — both are a flat rate — and a dead stream after a Mongo migration
  // once sat behind them for hours. See [[services.movies.ChangeStreamLiveness]].
  private val changeStreamLastEventAge = Gauge.builder()
    .name("kinowo_worker_change_stream_last_event_age_seconds")
    .help("Seconds since each change-stream cursor (collection=movies|screenings|movie_slots) last DELIVERED an event to the worker, by country; since boot when it never has. Recomputed on every scrape, so an open-but-silent cursor climbs in a straight line. Read against the country's scrape cadence: the movies cursor is quiet for at most one sweep while ratings and scrapes are being written; longer than that with kinowo_worker_corpus_movies moving means the stream is not delivering the writes.")
    .labelNames("country", "collection")
    .register(registry)

  // The APPLY side of the same three cursors, also computed at scrape time: an event delivered
  // on time can still wait behind the single apply thread, and until 2026-09-23 the resume token
  // was saved at delivery, so a restart skipped whatever was waiting — with nothing to show how
  // much that was. See [[services.movies.ChangeStreamLiveness.queued]].
  private val changeStreamApplyPending = Gauge.builder()
    .name("kinowo_worker_change_stream_apply_pending")
    .help("Change events each cursor (collection=movies|screenings|movie_slots) handed to the worker's apply thread that have not been applied yet, by country. Bounded by the cursor's demand window (256); near zero in steady state because an apply is one stitch read. Pinned near the window means the apply thread cannot keep up, and every one of these is a change the read model has not seen. Coalesced events ride an apply already queued and are not counted twice.")
    .labelNames("country", "collection")
    .register(registry)

  private val changeStreamApplyLag = Gauge.builder()
    .name("kinowo_worker_change_stream_apply_lag_seconds")
    .help("Seconds the OLDEST not-yet-applied change event of each cursor (collection=movies|screenings|movie_slots) has waited since delivery, by country; 0 when nothing is waiting. Recomputed on every scrape, so an apply thread that is stuck climbs in a straight line. The delivered-vs-applied lag: the read model is at least this stale for that change. Alerted by ChangeStreamApplyLagging (over 10 minutes for 10 minutes).")
    .labelNames("country", "collection")
    .register(registry)

  private val changeEvents = Counter.builder()
    .name("kinowo_worker_movie_change_events")
    .help("Movie change-stream events the shared cursor consumed since boot, by country and op (insert|update|replace|delete). rate() is the change-stream volume the cache + read-model projector reproject from.")
    .labelNames("country", "op")
    .register(registry)

  private val changeUpdateKinds = Counter.builder()
    .name("kinowo_worker_movie_change_update_kinds")
    .help("For UPDATE events, which field kind changed since boot, by country: source_data=a cinema slot (scrape write), rating=a rating value/url, identity=tmdb/imdb id + resolution lifecycle, updated_at_only=a no-op that touched only updatedAt (a redundant-write canary — should stay ~0). A multi-field update counts under each kind it touched.")
    .labelNames("country", "kind")
    .register(registry)

  private val movieCoalesced = Counter.builder()
    .name("kinowo_worker_movie_coalesced_changes")
    .help("Movies-doc change events that rode an apply already queued for their film instead of buying their own, by country — the movies cursor's twin of screenings/movie_slots_coalesced_changes, sharing the SAME pending set as those two. `dropCinemaSlots` writes `retainedSynopses` to `movies` in the same tick it deletes the dropped venue's screenings/movie_slots rows, so one logical slot-drop used to buy this cursor its own re-projection on top of the side cursors' (coalesced) one. coalesced/(coalesced+readmodel_project_calls) is the share now folded away. All GENUINE changes; no write guard can remove them.")
    .labelNames("country")
    .register(registry)

  private val changeApplies = Counter.builder()
    .name("kinowo_worker_change_apply")
    .help("Film applies the change stream made since boot, by country, path and reason. path=venues: a burst of showtime changes at a few venues applied from those venues' rows alone (reason=applied) — the other venues of the film neither read nor re-projected. path=film: the whole film re-read, because a movies or movie_slots change rode the burst (not_showtimes), it touched too many venues (too_many_venues), the film is failing a re-read (failing), the venues could not be read alone (venue_read_failed), a listener could not take them alone — a venue appearing or vanishing, a card unpublished or held (declined) — or the store cannot read venues alone (unsupported). A rising declined share is the projector or cache refusing what it should take: each is a whole re-read of a film that changed at one venue.")
    .labelNames("country", "path", "reason")
    .register(registry)

  private val venueDeclines = Counter.builder()
    .name("kinowo_worker_venue_apply_declines")
    .help("Why a listener declined a film's changed venues alone, sending that film down the whole re-read (kinowo_worker_change_apply{path=film,reason=declined}), by country and reason. projector_*: the read-model projector could not vouch its venue rows equal a whole-film projection — the row not projected by this process, a slot under an unknown title group, two slots at a venue, a venue appearing or vanishing, a venue whose row unioned slots, a held or unpublished card, rows remembered without their inputs (seeded at boot). cache_*: the movie cache does not hold the film, or holds other slots at the venue. One apply can count under several listeners.")
    .labelNames("country", "reason")
    .register(registry)

  private val screeningsChangeEvents = Counter.builder()
    .name("kinowo_worker_screenings_change_events")
    .help("Screenings change-stream events the SECOND cursor consumed since boot, by country and op (insert|update|replace|delete). The read-model projection's larger trigger: this cursor rings once per changed screenings DOCUMENT — one per (film, cinema slot) — and each ring costs a stitch read plus a full projection. Read readmodel_project_calls_total against the SUM of this and movie_change_events; against movie_change_events alone it reads as an unexplained 55:1, which is what a 2026-09-04 projection climb looked like while this half of the input had no counter.")
    .labelNames("country", "op")
    .register(registry)

  private val screeningsWrites = Counter.builder()
    .name("kinowo_worker_screenings_writes")
    .help("Slot writes the screenings store was asked to make since boot, by country and outcome. written=the row's showtimes moved and it was written; unchanged=the row already held exactly what the caller asked for, so the write was DROPPED — it never reached the oplog, never rang the screenings change stream and never bought a read-model projection. A high unchanged SHARE is the guard working, not a fault: `replaceFilm` is film-wide while its callers' change is one venue, so a wide release rewrote every row it had (297 of 298 redundantly on prod DE, 2026-09-04). Watch the WRITTEN rate against the scrape rate instead — written climbing under a flat scrape rate is a caller that has started rewriting rows it did not change.")
    .labelNames("country", "outcome")
    .register(registry)

  private val screeningsCoalesced = Counter.builder()
    .name("kinowo_worker_screenings_coalesced_changes")
    .help("Screenings change events that rode an apply already queued for their film instead of buying their own, by country. The screenings cursor rings once per (film, cinema slot), so a film that changes at every venue rings once per venue — and one re-read after the burst sees all of it, because the apply reads the film's CURRENT state. This is the saving made visible: coalesced/(coalesced+readmodel_project_calls) is the share of the fan-out collapsed, ~0 where films move one venue at a time and approaching 1 on a wide release (the widest US film carries 3,327 slots). Unlike screenings_writes{outcome=unchanged} these are all GENUINE changes; no write guard can remove them.")
    .labelNames("country")
    .register(registry)

  private val slotsChangeEvents = Counter.builder()
    .name("kinowo_worker_movie_slots_change_events")
    .help("movie_slots change-stream events the THIRD cursor consumed since boot, by country and op (insert|update|replace|delete). One event per changed slot DOCUMENT — one per (film, cinema slot) — and each ring costs a stitch read plus a full projection, like the screenings cursor it mirrors. A venue's slot lands WITHOUT a movies write whenever the film document is unchanged (the usual case under the split), and the projection cannot emit that venue's row until the slot exists; before this cursor 63 UK and 33 PL (film, venue) pairs whose slot landed after the film's last projection were never projected again (prod, 2026-09-07). Read readmodel_project_calls_total against the SUM of this, screenings_change_events and movie_change_events.")
    .labelNames("country", "op")
    .register(registry)

  private val slotsCoalesced = Counter.builder()
    .name("kinowo_worker_movie_slots_coalesced_changes")
    .help("movie_slots change events that rode an apply already queued for their film instead of buying their own, by country — the slots twin of screenings_coalesced_changes, and the two cursors share ONE pending set, so a film's screenings row and slot row arriving together are one re-read. All GENUINE changes; no write guard can remove them.")
    .labelNames("country")
    .register(registry)

  countryCodes.foreach { c =>
    ChangeStreamMetrics.Ops.foreach(o => changeEvents.labelValues(c, o))
    movieCoalesced.labelValues(c)
    ChangeStreamMetrics.Apply.Series.foreach { case (path, reason) => changeApplies.labelValues(c, path, reason) }
    ChangeStreamMetrics.VenueDecline.All.foreach(reason => venueDeclines.labelValues(c, reason))
    ChangeStreamMetrics.Ops.foreach(o => screeningsChangeEvents.labelValues(c, o))
    ScreeningsMetrics.Outcomes.foreach(o => screeningsWrites.labelValues(c, o))
    screeningsCoalesced.labelValues(c)
    ChangeStreamMetrics.Ops.foreach(o => slotsChangeEvents.labelValues(c, o))
    slotsCoalesced.labelValues(c)
    ChangeStreamMetrics.Kinds.foreach(k => changeUpdateKinds.labelValues(c, k))
    ChangeStreamLiveness.Collections.foreach { coll =>
      changeStreamLastEventAge.labelValues(c, coll).set(0.0)
      changeStreamApplyPending.labelValues(c, coll).set(0.0)
      changeStreamApplyLag.labelValues(c, coll).set(0.0)
    }
  }

  def recordEvent(country: String, op: String): Unit       = changeEvents.labelValues(country, op).inc()

  def recordUpdateKind(country: String, kind: String): Unit = changeUpdateKinds.labelValues(country, kind).inc()

  def recordMovieCoalesced(country: String): Unit          = movieCoalesced.labelValues(country).inc()

  def recordChangeApply(country: String, path: String, reason: String): Unit = changeApplies.labelValues(country, path, reason).inc()

  def recordVenueDecline(country: String, reason: String): Unit            = venueDeclines.labelValues(country, reason).inc()

  def recordScreeningsChangeEvent(country: String, op: String): Unit = screeningsChangeEvents.labelValues(country, op).inc()

  def recordScreeningsWrite(country: String, outcome: String, count: Int): Unit =
    if (count > 0) screeningsWrites.labelValues(country, outcome).inc(count.toDouble)

  def recordScreeningsCoalesced(country: String): Unit = screeningsCoalesced.labelValues(country).inc()

  def recordSlotsChangeEvent(country: String, op: String): Unit = slotsChangeEvents.labelValues(country, op).inc()

  def recordSlotsCoalesced(country: String): Unit               = slotsCoalesced.labelValues(country).inc()

  /** Recompute each cursor's liveness gauges at scrape time, so a silent cursor's age and a stuck
   *  apply's lag keep climbing. */
  def refreshLiveness(country: String, liveness: ChangeStreamLiveness, now: Instant): Unit =
    ChangeStreamLiveness.Collections.foreach { coll =>
      changeStreamLastEventAge.labelValues(country, coll).set(liveness.ageSeconds(coll, now))
      changeStreamApplyPending.labelValues(country, coll).set(liveness.pendingApplies(coll).toDouble)
      changeStreamApplyLag.labelValues(country, coll).set(liveness.applyLagSeconds(coll, now))
    }
}