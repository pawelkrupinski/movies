package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{Regroupings, SuspectTakes}
import services.identity.adapters.IdentityProjectionMetrics

/**
 * Each country's identity projection (`IdentityProjection`, docs/design/identity-resolver.md §8 phase 5), each series
 * labelled by `country` and written by that country's projection.
 *
 * KEEP THE SERIES NAMES. Every country is cut over and the class no longer says "cutover", but the worker-diagnostics
 * dashboard and the identity-matching alert rules (infra/nix/files/monitoring/) read `kinowo_worker_identity_cutover_*`
 * by name: renaming a series blanks them.
 *
 *  - `kinowo_worker_identity_cutover_films{path}` / `kinowo_worker_identity_cutover_listings{path}` —
 *    the films the last projection stored and the listings it resolved. `path` is always `projection`: it once told a
 *    cut-over country from one on the old landing / staging / settle path, which no longer exists, so the label says
 *    nothing now; it stays because the dashboard and the alerts select and group by it;
 *  - `kinowo_worker_identity_regroupings_total{kind}` — merges, splits, moved listings, fresh ids and
 *    retired ids, per projection. §10's churn acceptance: after the first projection of a cutover,
 *    merges / splits / moves stay at 0 on an unchanged listing set (P2); fresh and retired follow the
 *    venues' programmes;
 *  - `kinowo_worker_identity_projection_refusals_total{reason}` — projections refused: a family
 *    crossing, an unreadable FilmId map, a shrink past `ProjectionGuard`'s shares, a model not ready,
 *    or a projection that threw (`failed`);
 *  - `kinowo_worker_identity_projection_seconds` — how long the last projection took;
 *  - `kinowo_worker_identity_projection_drift_total` — films an hourly projection of the whole corpus changed that the
 *    scoped projections between (`ProjectionScope`: only the films their changes reach) would have left as they were.
 *    Zero, always: anything else is a film a scope missed, which the whole projection has just put right.
 *  - `kinowo_worker_identity_take_changes_total{kind}` — listings still published whose TMDB film a written projection
 *    moved: `lost` (had one, has none), `changed` (another), `gained` (had none). A listing leaving the schedule, a new
 *    one, and a re-key or merge that keeps the TMDB film are churn and never counted ([[services.identity.TakeChange]]);
 *    the alarm on match changes reads this. Each change is also logged, its facts as fields
 *    (`identity take change: …`, [[services.identity.TakeChangeLines]]).
 *  - `kinowo_worker_identity_take_reversals_total` — take changes that move a listing BACK to the film (or no-film) it held
 *    before its last change ([[services.identity.TakeReversals]]): two rules undoing each other, an oscillation. UK
 *    Chichester's "The Ties That Bind Us" flipped between two films on every pod start of 2026-10-13/14;
 *  - `kinowo_worker_identity_billing_reversals_total{billing}` — written projections that reversed a take of the BILLING
 *    (its title folded, [[services.identity.BillingReversals]]), once a projection however many venues bill it: what the
 *    per-billing oscillation alert reads. Bounded: the [[services.identity.BillingReversals.Capacity]] billings that
 *    reversed most recently keep a series, the least recent past it is dropped;
 *  - `kinowo_worker_identity_suspect_takes{signal}` — the billings whose take a suspect-take signal finds against its own
 *    evidence (`services.identity.SuspectTakes`: a fragment, an era, a runtime, a sequel, a feed's id, a fill alone), on the
 *    last projection of the whole corpus. A rise between recordings is a wrong take the convergence legs will name;
 *  - `kinowo_worker_identity_projection_reconciles_total{measured}` — hourly projections of the whole corpus, `measured`
 *    when an earlier projection gave one a scope to check drift against: an hour of `false` alone is drift not measured.
 *
 * Nothing is seeded: a country whose projection has not run exports no films / listings series.
 */
final class IdentityProjectionGauges(registry: PrometheusRegistry) {

  private val films: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_cutover_films")
    .help("Films the last identity projection stored, per country and path (always projection).")
    .labelNames("country", "path")
    .register(registry)

  private val listings: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_cutover_listings")
    .help("Listings the last identity projection resolved, per country and path.")
    .labelNames("country", "path")
    .register(registry)

  private val regroupings: Counter = Counter.builder()
    .name("kinowo_worker_identity_regroupings_total")
    .help("Identity projection regroupings: merge, split, move (listings), fresh (ids), retire (ids).")
    .labelNames("country", "kind")
    .register(registry)

  private val refusals: Counter = Counter.builder()
    .name("kinowo_worker_identity_projection_refusals_total")
    .help("Identity projections refused (crossing / unreadablemap / shrink / notready / failed); the stored films kept serving.")
    .labelNames("country", "reason")
    .register(registry)

  private val seconds: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_projection_seconds")
    .help("Wall-clock seconds the last identity projection took.")
    .labelNames("country")
    .register(registry)

  private val reconciles: Counter = Counter.builder()
    .name("kinowo_worker_identity_projection_reconciles_total")
    .help("Hourly whole-corpus identity projections; measured=true when an earlier projection gave one a scope to check drift against.")
    .labelNames("country", "measured")
    .register(registry)

  private val drift: Counter = Counter.builder()
    .name("kinowo_worker_identity_projection_drift_total")
    .help("Films a reconciling whole-corpus identity projection changed that the scoped projections before it would have left; 0 unless a scope missed one.")
    .labelNames("country")
    .register(registry)

  private val takeChanges: Counter = Counter.builder()
    .name("kinowo_worker_identity_take_changes_total")
    .help("Published listings whose TMDB film a written identity projection moved: lost / changed / gained. Churn (a listing leaving, a re-key keeping the film) is not counted.")
    .labelNames("country", "kind")
    .register(registry)

  private val takeReversals: Counter = Counter.builder()
    .name("kinowo_worker_identity_take_reversals_total")
    .help("Published listings a written identity projection moved back to the film (or no-film) they held before their last take change: an oscillation.")
    .labelNames("country")
    .register(registry)

  private val billingReversals: Counter = Counter.builder()
    .name("kinowo_worker_identity_billing_reversals_total")
    .help("Written identity projections that moved a billing's listings back to the film (or no-film) they held before: once a projection per billing (title folded), the 25 most recently reversed billings labelled.")
    .labelNames("country", "billing")
    .register(registry)

  private val suspectTakes: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_suspect_takes")
    .help("Billings whose identity take a suspect-take signal finds against its own evidence, per signal, on the last written whole-corpus projection.")
    .labelNames("country", "signal")
    .register(registry)

  /** Where a country's suspect-take count goes ([[services.identity.adapters.SuspectTakeCount]]): one series per signal. */
  def suspects(country: String): Map[SuspectTakes.Signal, Int] => Unit =
    counts => counts.foreach { case (signal, n) => suspectTakes.labelValues(country, signal.label).set(n.toDouble) }

  def forCountry(country: String): IdentityProjectionMetrics = new IdentityProjectionMetrics {
    // Exported at 0 from boot: no drift reads 0, not absent, and an unmeasured hour shows as reconciles{measured=false}.
    drift.labelValues(country)
    Seq("true", "false").foreach(reconciles.labelValues(country, _))
    // Every kind exported at 0 from boot, so a first change is an increase a rate sees, not a series appearing.
    services.identity.TakeChange.Kind.values.foreach(k => takeChanges.labelValues(country, k.label))
    takeReversals.labelValues(country)
    private val reversals = new services.identity.TakeReversals
    private val billings  = new services.identity.BillingReversals
    def takesChanged(changes: Seq[services.identity.TakeChange]): Unit = {
      changes.groupMapReduce(_.kind)(_ => 1)(_ + _).foreach { case (kind, n) => takeChanges.labelValues(country, kind.label).inc(n.toDouble) }
      val reversed = reversals.note(changes)
      takeReversals.labelValues(country).inc(reversed.size.toDouble)
      val noted = billings.note(reversed)
      noted.forgotten.foreach(billing => billingReversals.remove(country, billing))
      noted.reversed.foreach(billing => billingReversals.labelValues(country, billing).inc())
    }
    def reconciled(measured: Boolean): Unit = reconciles.labelValues(country, measured.toString).inc()
    def projected(filmCount: Int, listingCount: Int, moved: Regroupings, took: Double): Unit = {
      films.labelValues(country, "projection").set(filmCount.toDouble)
      listings.labelValues(country, "projection").set(listingCount.toDouble)
      Seq("merge" -> moved.merges, "split" -> moved.splits, "move" -> moved.moves, "fresh" -> moved.fresh, "retire" -> moved.retired)
        .foreach { case (kind, n) => regroupings.labelValues(country, kind).inc(n.toDouble) }
      seconds.labelValues(country).set(took)
    }
    def refused(reason: IdentityProjectionMetrics.Refusal): Unit = refusals.labelValues(country, reason.label).inc()
    def drifted(films: Int): Unit = drift.labelValues(country).inc(films.toDouble)
  }
}
