package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{IdentityProjectionMetrics, Regroupings, ShadowRelation}

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

  private val drift: Counter = Counter.builder()
    .name("kinowo_worker_identity_projection_drift_total")
    .help("Films a reconciling whole-corpus identity projection changed that the scoped projections before it would have left; 0 unless a scope missed one.")
    .labelNames("country")
    .register(registry)

  def forCountry(country: String): IdentityProjectionMetrics = new IdentityProjectionMetrics {
    def projected(filmCount: Int, listingCount: Int, moved: Regroupings, relations: Map[ShadowRelation, Int], took: Double): Unit = {
      films.labelValues(country, "projection").set(filmCount.toDouble)
      listings.labelValues(country, "projection").set(listingCount.toDouble)
      Seq("merge" -> moved.merges, "split" -> moved.splits, "move" -> moved.moves, "fresh" -> moved.fresh, "retire" -> moved.retired)
        .foreach { case (kind, n) => regroupings.labelValues(country, kind).inc(n.toDouble) }
      ShadowRelation.values.foreach(r => canary.labelValues(country, r.label).set(relations.getOrElse(r, 0).toDouble))
      seconds.labelValues(country).set(took)
    }
    def refused(reason: IdentityProjectionMetrics.Refusal): Unit = refusals.labelValues(country, reason.label).inc()
    // Touched at 0 too, so the series exists from the first reconciliation and a rate over it reads 0, not absent.
    def drifted(films: Int): Unit = drift.labelValues(country).inc(films.toDouble)
  }
}
