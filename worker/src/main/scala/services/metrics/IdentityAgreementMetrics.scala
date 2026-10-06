package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.AgreementQuestionMetrics
import services.identity.agreement.{AgreementStage, VoterFamily}

/**
 * The identity agreement's series (`agreement.AgreementStage`, `AgreementQuestions`) — the no-matches ≥3 other film
 * databases are asked about:
 *  - `kinowo_worker_identity_agreement_clusters{state}` — after the stage's last pass: clusters `waiting` on a family's
 *    answer, `verdicts` kept, those `agreed` on a film, and those whose agreed film a venue poster vetoed
 *    (`poster-vetoed`, `PosterEvidence.veto`); and the model's own takes read against the evidence that can correct them
 *    (`correcting`, `agreement.Correction`);
 *  - `kinowo_worker_identity_agreement_taken{as}` — the decisions the last pass took, as a `tmdb` film or an IMDb
 *    `fallback` the families agreed on, a `poster` vote (`ResolverDecision.Basis.Poster`), a `broadcast` the screening days
 *    named (`ResolverDecision.Basis.Broadcast`), a `filled` rule's film, or the `catalogue` film a listing's own catalogue id
 *    names (`ResolverDecision.Basis.Catalogue`): the cards the stage identified; and the model's takes it `withdrawn`
 *    (`ResolverDecision.Basis.Withdrawn`) or `corrected` to another film (`ResolverDecision.Basis.Corrected`);
 *  - `kinowo_worker_identity_agreement_open_questions{family}` — questions no answer is filed for yet, per family
 *    (`tmdb-find`: agreed IMDb ids TMDB was not asked about; `poster`: posters not hashed yet; `catalogue`: catalogue ids
 *    not mapped and venue pages whose links are not read yet; `correction-poster`: the posters the corrections wait on,
 *    handed to the queue a few at a time) — falling to zero is the
 *    backlog asked;
 *  - `kinowo_worker_identity_agreement_resolves_total` / `_seconds` — clusters the stage resolved again, and its last
 *    pass's wall time: what it costs a projection (a quiet tick resolves none);
 *  - `kinowo_worker_identity_agreement_enqueued_total{family,result}` — questions put on the task queue (`added`, or
 *    `duplicate`: one already queued);
 *  - `kinowo_worker_identity_agreement_answers_total{family,outcome}` — questions asked: answered, nothing (filed as no
 *    film), fresh (skipped), deferred (the host's breaker open), failed (asked again later).
 * Every series is exported at 0 from boot.
 */
final class IdentityAgreementMetrics(registry: PrometheusRegistry) {

  private val clusters: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_clusters")
    .help("Agreement clusters after the stage's last pass: waiting on a family's answer, verdicts kept, agreed on a film.")
    .labelNames("country", "state").register(registry)

  private val taken: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_taken")
    .help("Decisions the agreement's last pass took, as a TMDB film or an IMDb fallback.")
    .labelNames("country", "as").register(registry)

  private val open: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_open_questions")
    .help("Agreement questions with no answer filed yet, per family (tmdb-find: agreed IMDb ids TMDB was not asked about).")
    .labelNames("country", "family").register(registry)

  private val resolves: Counter = Counter.builder()
    .name("kinowo_worker_identity_agreement_resolves_total")
    .help("Clusters the agreement stage resolved again over the families' answers; a quiet projection resolves none.")
    .labelNames("country").register(registry)

  private val daysUnreadTotal: Counter = Counter.builder()
    .name("kinowo_worker_identity_relay_days_unread_total")
    .help("Lean listing reads whose stage relays' screening days could not be read: those relays' days are unknown, and the broadcast take waits, until a next read.")
    .labelNames("country", "archive").register(registry)

  /** Counts, for `country`'s `archive` (`accepted` or `archive`), a lean read whose stage relays' days were not read. */
  def daysUnread(country: String, archive: String): () => Unit = {
    val series = daysUnreadTotal.labelValues(country, archive)
    () => series.inc()
  }

  private val seconds: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_seconds")
    .help("Wall-clock seconds the agreement stage's last pass that read anything took.")
    .labelNames("country").register(registry)

  private val enqueued: Counter = Counter.builder()
    .name("kinowo_worker_identity_agreement_enqueued_total")
    .help("Agreement questions put on the task queue: added, or duplicate (already queued).")
    .labelNames("country", "family", "result").register(registry)

  private val answers: Counter = Counter.builder()
    .name("kinowo_worker_identity_agreement_answers_total")
    .help("Agreement questions asked, by family and outcome: answered, nothing, fresh, deferred, failed.")
    .labelNames("country", "family", "outcome").register(registry)

  private val families: Seq[String] = VoterFamily.values.toSeq.map(_.label) :+ AgreementQuestionMetrics.TmdbFind :+ AgreementQuestionMetrics.TmdbRecord :+
    AgreementQuestionMetrics.Poster :+ AgreementQuestionMetrics.Catalogue

  /** The stage's series for `country`, every label touched at 0. */
  def stage(country: String): AgreementStage.Metrics = {
    Seq("waiting", "verdicts", "agreed", "poster-vetoed", "correcting").foreach(clusters.labelValues(country, _))
    Seq("tmdb", "fallback", "poster", "broadcast", "filled", "catalogue", "withdrawn", "corrected").foreach(taken.labelValues(country, _))
    (families :+ CorrectionPoster).foreach(open.labelValues(country, _))
    resolves.labelValues(country); seconds.labelValues(country)
    applied => {
      clusters.labelValues(country, "waiting").set(applied.waiting.toDouble)
      clusters.labelValues(country, "verdicts").set(applied.verdicts.toDouble)
      clusters.labelValues(country, "agreed").set(applied.agreed.toDouble)
      clusters.labelValues(country, "poster-vetoed").set(applied.posterVetoed.toDouble)
      clusters.labelValues(country, "correcting").set(applied.correcting.toDouble)
      taken.labelValues(country, "withdrawn").set(applied.withdrawn.toDouble)
      taken.labelValues(country, "corrected").set(applied.corrected.toDouble)
      open.labelValues(country, CorrectionPoster).set(applied.correctionPosters.toDouble)
      taken.labelValues(country, "poster").set(applied.takenPoster.toDouble)
      taken.labelValues(country, "broadcast").set(applied.takenBroadcast.toDouble)
      taken.labelValues(country, "filled").set(applied.takenFilled.toDouble)
      taken.labelValues(country, "catalogue").set(applied.takenCatalogue.toDouble)
      open.labelValues(country, AgreementQuestionMetrics.Catalogue).set(applied.catalogue.toDouble)
      open.labelValues(country, AgreementQuestionMetrics.Poster).set(applied.posters.toDouble)
      taken.labelValues(country, "tmdb").set(applied.takenTmdb.toDouble)
      taken.labelValues(country, "fallback").set(applied.takenFallback.toDouble)
      VoterFamily.values.foreach(f => open.labelValues(country, f.label).set(applied.open.getOrElse(f, 0).toDouble))
      open.labelValues(country, AgreementQuestionMetrics.TmdbFind).set(applied.finds.toDouble)
      open.labelValues(country, AgreementQuestionMetrics.TmdbRecord).set(applied.undated.toDouble)
      resolves.labelValues(country).inc(applied.resolves.toDouble)
      seconds.labelValues(country).set(applied.seconds)
    }
  }

  /** The open series' label for the posters the model takes' corrections wait on. */
  private val CorrectionPoster = "correction-poster"

  /** The questions' series for `country`, every label touched at 0. */
  def questions(country: String): AgreementQuestionMetrics = {
    families.foreach { family =>
      Seq("added", "duplicate").foreach(enqueued.labelValues(country, family, _))
      AgreementQuestionMetrics.Outcomes.foreach(answers.labelValues(country, family, _))
    }
    new AgreementQuestionMetrics {
      def enqueued(family: String, added: Boolean): Unit = IdentityAgreementMetrics.this.enqueued.labelValues(country, family, if (added) "added" else "duplicate").inc()
      def asked(family: String, outcome: String): Unit   = answers.labelValues(country, family, outcome).inc()
    }
  }
}
