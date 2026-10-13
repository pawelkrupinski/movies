package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{IdentityProjectionMetrics, Regroupings, ShadowRelation, SuspectTakes}

/**
 * The identity projection of a cut-over country (`IdentityProjection`, docs/design/identity-resolver.md
 * §8 phase 5), each series labelled by `country` and written by that country's projection:
 *
 *  - `kinowo_worker_identity_cutover_films{path}` / `kinowo_worker_identity_cutover_listings{path}` —
 *    the films the last projection stored and the listings it resolved. `path` is `projection`: a
 *    country on the old landing / staging / settle path exports none (its films are
 *    `kinowo_worker_corpus_movies`), so the pair says which countries are cut over and how big each is;
 *  - `kinowo_worker_identity_regroupings_total{kind}` — merges, splits, moved listings, fresh ids and
 *    retired ids, per projection. §10's churn acceptance: after the first projection of a cutover,
 *    merges / splits / moves stay at 0 on an unchanged listing set (P2); fresh and retired follow the
 *    venues' programmes;
 *  - `kinowo_worker_identity_cutover_canary{relation}` — the resolver's clusters against the films stored
 *    BEFORE the projection (the shadow diff's relations, `ShadowRelation`). On the first projection
 *    of a cutover that is the old path's films, so it is the old-vs-new canary; afterwards anything
 *    but `identical` is a regrouping the projection is about to write;
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
 *    the alarm on match changes reads this. Each change is also logged (`identity take change: …`).
 *  - `kinowo_worker_identity_suspect_takes{signal}` — the billings whose take a suspect-take signal finds against its own
 *    evidence (`services.identity.SuspectTakes`: a fragment, an era, a runtime, a sequel, a feed's id, a fill alone), on the
 *    last projection of the whole corpus. A rise between recordings is a wrong take the convergence legs will name;
 *  - `kinowo_worker_identity_projection_reconciles_total{measured}` — hourly projections of the whole corpus, `measured`
 *    when an earlier projection gave one a scope to check drift against: an hour of `false` alone is drift not measured.
 *
 * Nothing is seeded: a country not cut over exports no series.
 */
final class IdentityCutoverMetrics(registry: PrometheusRegistry) {

  private val films: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_cutover_films")
    .help("Films the last identity projection stored, per country and path (projection only; old-path countries export none).")
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

  private val canary: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_cutover_canary")
    .help("The last projection's clusters by relation to the films stored before it (identical / split / merged / moved).")
    .labelNames("country", "relation")
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

  private val suspectTakes: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_suspect_takes")
    .help("Billings whose identity take a suspect-take signal finds against its own evidence, per signal, on the last written whole-corpus projection.")
    .labelNames("country", "signal")
    .register(registry)

  /** Where a country's suspect-take count goes ([[services.identity.SuspectTakeCount]]): one series per signal. */
  def suspects(country: String): Map[SuspectTakes.Signal, Int] => Unit =
    counts => counts.foreach { case (signal, n) => suspectTakes.labelValues(country, signal.label).set(n.toDouble) }

  def forCountry(country: String): IdentityProjectionMetrics = new IdentityProjectionMetrics {
    // Exported at 0 from boot: no drift reads 0, not absent, and an unmeasured hour shows as reconciles{measured=false}.
    drift.labelValues(country)
    Seq("true", "false").foreach(reconciles.labelValues(country, _))
    // Every kind exported at 0 from boot, so a first change is an increase a rate sees, not a series appearing.
    services.identity.TakeChange.Kind.values.foreach(k => takeChanges.labelValues(country, k.label))
    def takesChanged(changes: Seq[services.identity.TakeChange]): Unit =
      changes.groupMapReduce(_.kind)(_ => 1)(_ + _).foreach { case (kind, n) => takeChanges.labelValues(country, kind.label).inc(n.toDouble) }
    def reconciled(measured: Boolean): Unit = reconciles.labelValues(country, measured.toString).inc()
    def projected(filmCount: Int, listingCount: Int, moved: Regroupings, relations: Map[ShadowRelation, Int], took: Double): Unit = {
      films.labelValues(country, "projection").set(filmCount.toDouble)
      listings.labelValues(country, "projection").set(listingCount.toDouble)
      Seq("merge" -> moved.merges, "split" -> moved.splits, "move" -> moved.moves, "fresh" -> moved.fresh, "retire" -> moved.retired)
        .foreach { case (kind, n) => regroupings.labelValues(country, kind).inc(n.toDouble) }
      ShadowRelation.values.foreach(r => canary.labelValues(country, r.label).set(relations.getOrElse(r, 0).toDouble))
      seconds.labelValues(country).set(took)
    }
    def refused(reason: IdentityProjectionMetrics.Refusal): Unit = refusals.labelValues(country, reason.label).inc()
    def drifted(films: Int): Unit = drift.labelValues(country).inc(films.toDouble)
  }
}
