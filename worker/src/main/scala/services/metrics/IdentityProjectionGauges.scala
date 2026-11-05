package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.{Regroupings, ResolverDecision, SuspectTakes}
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
 *  - `kinowo_worker_identity_take_changes_total{basis,kind}` — each written projection's change list
 *    ([[services.identity.TakeChange]]), by the basis that holds the listing after (`ResolverDecision.Basis`): listings
 *    still published whose TMDB film it moved — `lost` (had one, has none), `changed` (another), `gained` (had none) —
 *    and listings new to the films: `decided` (a new cluster, matched or not) or `joined` (a cluster already on them). A
 *    listing leaving the schedule, and a re-key or merge that keeps the TMDB film, are churn and never counted; the alarm
 *    on match changes reads the moves, the worker dashboard's "Identity takes" panels all five. Each move is also logged,
 *    its facts as fields (`identity take change: …`, [[services.identity.TakeChangeLines]]).
 *  - `kinowo_worker_identity_take_causes_total{kind,cause}` — the same moves (`lost`, `changed`, `gained`) by why they
 *    moved ([[services.identity.TakeCauses]]): `evidence` (what the take stood on moved), `code` (it did not, and the
 *    decision did) or `regrouped` (the store moved the listing). The worker dashboard shows the losses by cause; the build
 *    alert reads the same cause off the logged lines.
 *  - `kinowo_worker_identity_take_reversals_total` — take changes that move a listing BACK to the film (or no-film) it held
 *    before its last change at most three projections earlier ([[services.identity.TakeReversals]]'s oscillations): two
 *    rules undoing each other. UK Chichester's "The Ties That Bind Us" flipped between two films on every pod start of
 *    2026-10-13/14;
 *  - `kinowo_worker_identity_take_flips_total{pattern,class,dropped_by,restored_by,lasted}` — every reversal within a
 *    day ([[services.identity.TakeReversals]]), by `scripts/flip-timeline`'s `class` (`DEPLOY/RESTART`, `LATE-ANSWER`,
 *    `SIBLING/POPULATION`, `RULE`); all but `DEPLOY/RESTART` are the flip backtest's count (195 in the week to 2026-11-05
 *    undone within a day): the `pattern` (`film-none-film`, `none-film-none`, `film-film-film`), the stage that took the
 *    listing off its take and the one that put it back (`services.identity.DecidedBy.stage`: model, relay, agreement,
 *    fill, correction, poster, …, `none`), and how long the undone take stood (`lasted`: `1m` under a minute, `1h` under
 *    an hour, `24h` under a day). A deploy's before/after flip rate; `kinowo_worker_identity_take_flip_seconds{pattern}`
 *    the flips' (not `DEPLOY/RESTART`) time to reverse, its `_count` exported at 0 from boot. A flip whose two changes straddle a worker
 *    restart is not counted (the memory is the worker's);
 *  - `kinowo_worker_identity_billing_reversals_total{billing}` — written projections that reversed a take of the BILLING
 *    (its title folded, [[services.identity.BillingReversals]]), once a projection however many venues bill it: what the
 *    per-billing oscillation alert reads. Bounded: the [[services.identity.BillingReversals.Capacity]] billings that
 *    reversed most recently keep a series, the least recent past it is dropped;
 *  - `kinowo_worker_identity_suspect_takes{signal}` — the billings whose take a suspect-take signal finds against its own
 *    evidence (`services.identity.SuspectTakes`: a fragment, an era, a runtime, a sequel, a feed's id, a fill alone), on the
 *    last projection of the whole corpus, but for the billings a hand judgement judges (`identity-judgements.tsv`, bundled:
 *    [[services.identity.TakeJudgements]]). A rise between recordings is a wrong take the convergence legs will name;
 *  - `kinowo_worker_identity_suspect_takes_wrong` — the billings taken as a film a hand judgement calls wrong, on the same
 *    projection: wrong takes still served, 0 once each is corrected;
 *  - `kinowo_worker_identity_rule_fires_total{rule}` — per rule id (a title rule's `title:<title-rules.tsv id>`, the
 *    model's `accept:` / `pooled:` / `veto:` / `apart:` / `join:` / `refused:` ids, a later stage's `stage:<rule>`), the
 *    live listings it fired on, added up over each written whole-corpus projection
 *    ([[services.identity.adapters.RuleFiringCount]]); `kinowo_worker_identity_rule_counted_listings_total` the listings
 *    those counts were over, so a rule's share of the corpus is the ratio of the two's increases. A rule with no series,
 *    or no increase over a week, fired on no live listing: dead in prod. Rule ids only, a few hundred series a country;
 *  - `kinowo_worker_identity_agreement_phase_seconds{mode}` / `kinowo_worker_identity_agreement_phase_allocated_bytes{mode}` —
 *    every projection's agreement phase ([[services.identity.adapters.ProjectionPhase]], the "agreement 69.1s/9579MB" of
 *    its log line), by its mode (`periodic`, `whole`, `changed`): the distribution `kinowo_worker_identity_agreement_seconds`,
 *    the last stage run's wall clock alone, cannot give. `IdentityAgreementCostRegression` compares its hour's median with
 *    its week's;
 *  - `kinowo_worker_identity_projection_reconciles_total{measured}` — hourly projections of the whole corpus, `measured`
 *    when an earlier projection gave one a scope to check drift against: an hour of `false` alone is drift not measured.
 *
 * Nothing is seeded: a country whose projection has not run exports no films / listings series.
 */
final class IdentityProjectionGauges(countryCodes: Seq[String], registry: PrometheusRegistry) {

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
    .help("Published listings whose TMDB film a written identity projection moved (lost / changed / gained) or that it placed first (decided: a new cluster; joined: an existing one), by the basis that holds them after. Churn (a listing leaving, a re-key keeping the film) is not counted.")
    .labelNames("country", "basis", "kind")
    .register(registry)

  private val takeCauses: Counter = Counter.builder()
    .name("kinowo_worker_identity_take_causes_total")
    .help("Published listings whose TMDB film a written identity projection moved (lost / changed / gained), by why: evidence (what the take stood on moved), code (it did not, and the decision did) or regrouped (the store moved the listing).")
    .labelNames("country", "kind", "cause")
    .register(registry)

  private val takeReversals: Counter = Counter.builder()
    .name("kinowo_worker_identity_take_reversals_total")
    .help("Published listings a written identity projection moved back to the film (or no-film) they held before their last take change: an oscillation.")
    .labelNames("country")
    .register(registry)

  private val takeFlips: Counter = Counter.builder()
    .name("kinowo_worker_identity_take_flips_total")
    .help("Published listings a written identity projection moved back to the film (or no-film) they held before their last take change, within a day (the flip backtest's reversal), by pattern, scripts/flip-timeline's class (DEPLOY/RESTART is a deploy's, which the backtest leaves out), the stage that dropped and that restored the take, and how long the undone take stood (1m, 1h, 24h).")
    .labelNames("country", "pattern", "class", "dropped_by", "restored_by", "lasted")
    .register(registry)

  private val takeFlipSeconds: Histogram = Histogram.builder()
    .name("kinowo_worker_identity_take_flip_seconds")
    .help("Seconds the take a flip undid stood (kinowo_worker_identity_take_flips_total), by pattern.")
    .labelNames("country", "pattern")
    .classicUpperBounds(IdentityProjectionGauges.FlipSecondsBuckets*)
    .classicOnly()
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

  private val suspectTakesWrong: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_suspect_takes_wrong")
    .help("Billings taken as a film a hand judgement (identity-judgements.tsv) calls wrong, on the last written whole-corpus projection.")
    .labelNames("country")
    .register(registry)

  private val agreementSeconds: Histogram = Histogram.builder()
    .name("kinowo_worker_identity_agreement_phase_seconds")
    .help("Wall clock of each identity projection's agreement phase, by projection mode (periodic, whole, changed).")
    .labelNames("country", "mode")
    .classicUpperBounds(IdentityProjectionGauges.AgreementSecondsBuckets*)
    .classicOnly()
    .register(registry)

  private val agreementBytes: Histogram = Histogram.builder()
    .name("kinowo_worker_identity_agreement_phase_allocated_bytes")
    .help("Bytes each identity projection's agreement phase allocated on the projecting thread, by projection mode (periodic, whole, changed).")
    .labelNames("country", "mode")
    .classicUpperBounds(IdentityProjectionGauges.AgreementBytesBuckets*)
    .classicOnly()
    .register(registry)

  private val ruleFires: Counter = Counter.builder()
    .name("kinowo_worker_identity_rule_fires_total")
    .help("Live listings each identity rule fired on (title rule, model rule, later stage's rule), added up over each written whole-corpus projection.")
    .labelNames("country", "rule")
    .register(registry)

  private val ruleCountedListings: Counter = Counter.builder()
    .name("kinowo_worker_identity_rule_counted_listings_total")
    .help("Listings the identity rule-firing counts were over, added up over each written whole-corpus projection: the rule shares' denominator.")
    .labelNames("country")
    .register(registry)

  /** Where a country's rule-firing count goes ([[services.identity.adapters.RuleFiringCount]]): each rule's listings, and
   *  how many listings were counted. */
  def ruleFirings(country: String): (Map[String, Int], Int) => Unit = { (fired, listings) =>
    fired.foreach { case (rule, n) => ruleFires.labelValues(country, rule).inc(n.toDouble) }
    ruleCountedListings.labelValues(country).inc(listings.toDouble)
  }

  /** Where a country's suspect-take count goes ([[services.identity.adapters.SuspectTakeCount]]): one series per signal. */
  def suspects(country: String): Map[SuspectTakes.Signal, Int] => Unit =
    counts => counts.foreach { case (signal, n) => suspectTakes.labelValues(country, signal.label).set(n.toDouble) }

  /** Where a country's count of billings taken as a film judged wrong goes. */
  def judgedWrong(country: String): Int => Unit = n => suspectTakesWrong.labelValues(country).set(n.toDouble)

  private def seed(country: String): Unit = {
    // Exported at 0 from boot: no drift reads 0, not absent, and an unmeasured hour shows as reconciles{measured=false}.
    drift.labelValues(country)
    Seq("true", "false").foreach(reconciles.labelValues(country, _))
    // Every basis and kind exported at 0 from boot, so a first change is an increase a rate sees, not a series appearing.
    for (basis <- ResolverDecision.Basis.values; kind <- services.identity.TakeChange.Kind.values)
      takeChanges.labelValues(country, basis.toString, kind.label)
    for (kind <- services.identity.TakeChange.Kind.Moves; cause <- services.identity.TakeChange.Cause.values)
      takeCauses.labelValues(country, kind.label, cause.label)
    takeReversals.labelValues(country)
    services.identity.TakeReversals.Patterns.foreach(takeFlipSeconds.labelValues(country, _))
  }

  // Every country's series from boot: the counters at 0, the measured gauges at NaN until the first projection (a 0
  // would read as an empty corpus or a clean one). Not the billing reversals (one series per billing, removed when
  // forgotten), the rule firings (per rule fired), the flips (per pattern and stages; their histogram is seeded per pattern)
  // or the phase histograms (per run mode).
  countryCodes.foreach { c =>
    seed(c)
    Seq(films, listings).foreach(_.labelValues(c, "projection").set(Double.NaN))
    seconds.labelValues(c).set(Double.NaN)
    IdentityProjectionGauges.RegroupingKinds.foreach(regroupings.labelValues(c, _))
    IdentityProjectionMetrics.Refusal.values.foreach(r => refusals.labelValues(c, r.label))
    SuspectTakes.Signal.values.foreach(signal => suspectTakes.labelValues(c, signal.label).set(Double.NaN))
    suspectTakesWrong.labelValues(c).set(Double.NaN)
    ruleCountedListings.labelValues(c)
  }

  def forCountry(country: String): IdentityProjectionMetrics = new IdentityProjectionMetrics {
    seed(country)
    private val billings  = new services.identity.BillingReversals
    def takesChanged(changes: Seq[services.identity.TakeChange]): Unit = {
      changes.groupMapReduce(t => (t.basisAfter.fold(IdentityProjectionGauges.NoDecision)(_.toString), t.kind.label))(_ => 1)(_ + _)
        .foreach { case ((basis, kind), n) => takeChanges.labelValues(country, basis, kind).inc(n.toDouble) }
      changes.flatMap(t => t.cause.filter(_ => t.moved).map(t.kind.label -> _.label)).groupMapReduce(identity)(_ => 1)(_ + _)
        .foreach { case ((kind, cause), n) => takeCauses.labelValues(country, kind, cause).inc(n.toDouble) }
    }
    override def takesReversed(reversals: Seq[services.identity.TakeReversals.Reversal]): Unit = {
      val oscillations = reversals.filter(_.oscillation).map(_.change)
      takeReversals.labelValues(country).inc(oscillations.size.toDouble)
      val noted = billings.note(oscillations)
      noted.forgotten.foreach(billing => billingReversals.remove(country, billing))
      noted.reversed.foreach(billing => billingReversals.labelValues(country, billing).inc())
      reversals.foreach { r =>
        takeFlips.labelValues(country, r.pattern, r.cause, r.droppedBy, r.restoredBy, r.lasted).inc()
        if (r.flip) takeFlipSeconds.labelValues(country, r.pattern).observe(r.seconds)
      }
    }
    def reconciled(measured: Boolean): Unit = reconciles.labelValues(country, measured.toString).inc()
    def projected(filmCount: Int, listingCount: Int, moved: Regroupings, took: Double): Unit = {
      films.labelValues(country, "projection").set(filmCount.toDouble)
      listings.labelValues(country, "projection").set(listingCount.toDouble)
      IdentityProjectionGauges.RegroupingKinds.zip(Seq(moved.merges, moved.splits, moved.moves, moved.fresh, moved.retired))
        .foreach { case (kind, n) => regroupings.labelValues(country, kind).inc(n.toDouble) }
      seconds.labelValues(country).set(took)
    }
    def refused(reason: IdentityProjectionMetrics.Refusal): Unit = refusals.labelValues(country, reason.label).inc()
    def drifted(films: Int): Unit = drift.labelValues(country).inc(films.toDouble)
    def agreementPhase(mode: String, phase: services.identity.adapters.ProjectionPhase): Unit = {
      agreementSeconds.labelValues(country, mode).observe(phase.seconds)
      agreementBytes.labelValues(country, mode).observe(phase.allocatedBytes.toDouble)
    }
  }
}

object IdentityProjectionGauges {
  /** The `basis` of a change no decision names. */
  val NoDecision = "none"

  /** The `kind` values of `kinowo_worker_identity_regroupings_total`, in the order a projection reports them. */
  val RegroupingKinds: Seq[String] = Seq("merge", "split", "move", "fresh", "retire")

  /** A flip's time to reverse: most a later stage's restore in the next projection, 5-7 s on (the backtest's week), the
   *  rest up to the day [[services.identity.TakeReversals.Horizon]] holds a change for. */
  val FlipSecondsBuckets: Seq[Double] = Seq(5, 10, 30, 60, 300, 900, 3600, 4 * 3600, 12 * 3600, 24 * 3600)

  /** The agreement phase's seconds: a scoped projection's is about a second, a whole one's past a minute on a large corpus. */
  val AgreementSecondsBuckets: Seq[Double] = Seq(0.25, 0.5, 1, 2.5, 5, 10, 20, 40, 60, 90, 120, 180, 300, 600)

  /** Its allocation: tens of MB scoped, ~10 GB for a large whole corpus (prod, 2026-10-25: "agreement 69.1s/9579MB"). */
  val AgreementBytesBuckets: Seq[Double] = Seq(1e7, 3e7, 1e8, 3e8, 1e9, 2e9, 4e9, 8e9, 16e9, 32e9, 64e9)
}
