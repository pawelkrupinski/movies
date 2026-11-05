package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.identity.adapters.AgreementQuestionMetrics
import services.identity.agreement.{AgreementStage, VoterFamily}

/**
 * The identity agreement's series (`agreement.AgreementStage`, `AgreementQuestions`) — the no-matches ≥3 other film
 * databases are asked about:
 *  - `kinowo_worker_identity_agreement_clusters{state}` — after the stage's last pass: clusters `waiting` on a family's
 *    answer, `verdicts` kept, those `agreed` on a film, and those whose agreed film a venue poster vetoed
 *    (`poster-vetoed`, `PosterEvidence.veto`); and the model's own takes read against the evidence that can correct them
 *    (`correcting`, `agreement.Correction`); and those no family is asked about: an `event`, not a film
 *    (`ResolverDecision.Basis.Event`, `agreement.NonFilmEvents`);
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
 *  - `kinowo_worker_identity_agreement_validated_total{result}` — kept verdicts the stage's full validation read again
 *    (`read`), and of them those the dirty index called standing whose receipt had moved (`drifted`: ZERO IS THE HEALTHY
 *    READING, the proof the index equals full validation; `services.identity.Memo`);
 *  - `kinowo_worker_identity_agreement_enqueued_total{family,result}` — questions put on the task queue (`added`, or
 *    `duplicate`: one already queued);
 *  - `kinowo_worker_identity_agreement_answers_total{family,outcome}` — questions asked: answered, nothing (filed as no
 *    film), fresh (skipped), deferred (the host's breaker open or its pace turned it away: remembered as failed), failed
 *    (asked again later);
 *  - `kinowo_worker_identity_agreement_inline_ask_seconds{family,part}` — each inline ask of a new cluster's families
 *    (`InlineFamilyAsks`), in two parts: `queued` (waiting for the inline pool) and `asking` (the host's pace, the wire and
 *    its retries, the filing) -- what an inline ask past the stage's 5 s wait (`AgreementStage.AskNowWithin`) spent its time on.
 * Every series is exported at 0 from boot.
 */
final class IdentityAgreementMetrics(countryCodes: Seq[String], registry: PrometheusRegistry) {

  private val clusters: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_clusters")
    .help("Agreement clusters after the stage's last pass: waiting on a family's answer, verdicts kept, agreed on a film, verdicts still to decide again (incomplete).")
    .labelNames("country", "state").register(registry)

  private val taken: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_taken")
    .help("Decisions the agreement's last pass took, as a TMDB film or an IMDb fallback.")
    .labelNames("country", "as").register(registry)

  private val open: Gauge = Gauge.builder()
    .name("kinowo_worker_identity_agreement_open_questions")
    .help("Agreement questions with no answer filed yet, per family (tmdb-find: agreed IMDb ids TMDB was not asked about).")
    .labelNames("country", "family").register(registry)

  private val inlineAsks: Histogram = Histogram.builder()
    .name("kinowo_worker_identity_agreement_inline_ask_seconds")
    .help("Each inline ask of a new cluster's family questions, by part: queued (waiting for the inline pool), asking (the host's pace, the wire and its retries, the filing).")
    .labelNames("country", "family", "part")
    .classicUpperBounds(IdentityAgreementMetrics.InlineAskBuckets*)
    .classicOnly()
    .register(registry)

  private val resolves: Counter = Counter.builder()
    .name("kinowo_worker_identity_agreement_resolves_total")
    .help("Clusters the agreement stage resolved again over the families' answers; a quiet projection resolves none.")
    .labelNames("country").register(registry)

  private val validated: Counter = Counter.builder()
    .name("kinowo_worker_identity_agreement_validated_total")
    .help("Kept agreement verdicts the stage's full validation read again (read), and those of them the dirty index called standing whose receipt had moved (drifted). Drifted must stay 0.")
    .labelNames("country", "result").register(registry)

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
  private def seedStage(country: String): Unit = {
    Seq("waiting", "verdicts", "agreed", "poster-vetoed", "correcting", "event", "incomplete").foreach(clusters.labelValues(country, _))
    Seq("tmdb", "fallback", "poster", "broadcast", "filled", "catalogue", "withdrawn", "corrected").foreach(taken.labelValues(country, _))
    (families :+ CorrectionPoster).foreach(open.labelValues(country, _))
    resolves.labelValues(country); seconds.labelValues(country)
    Seq("read", "drifted").foreach(validated.labelValues(country, _))
  }

  def stage(country: String): AgreementStage.Metrics = {
    seedStage(country)
    applied => {
      clusters.labelValues(country, "waiting").set(applied.waiting.toDouble)
      clusters.labelValues(country, "verdicts").set(applied.verdicts.toDouble)
      clusters.labelValues(country, "agreed").set(applied.agreed.toDouble)
      clusters.labelValues(country, "poster-vetoed").set(applied.posterVetoed.toDouble)
      clusters.labelValues(country, "correcting").set(applied.correcting.toDouble)
      taken.labelValues(country, "withdrawn").set(applied.withdrawn.toDouble)
      taken.labelValues(country, "corrected").set(applied.corrected.toDouble)
      open.labelValues(country, CorrectionPoster).set(applied.correctionPosters.toDouble)
      clusters.labelValues(country, "event").set(applied.events.toDouble)
      // a deploy's backfill: the verdicts decided under other rules still to be decided again, down to 0
      clusters.labelValues(country, "incomplete").set(applied.incomplete.toDouble)
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
      validated.labelValues(country, "read").inc(applied.validated.toDouble)
      validated.labelValues(country, "drifted").inc(applied.drifted.toDouble)
      seconds.labelValues(country).set(applied.seconds)
    }
  }

  /** The open series' label for the posters the model takes' corrections wait on. */
  private val CorrectionPoster = "correction-poster"

  /** The questions' series for `country`, every label touched at 0. */
  private def seedQuestions(country: String): Unit =
    families.foreach { family =>
      Seq("added", "duplicate").foreach(enqueued.labelValues(country, family, _))
      AgreementQuestionMetrics.Outcomes.foreach(answers.labelValues(country, family, _))
    }
    VoterFamily.values.foreach(family => Seq("queued", "asking").foreach(inlineAsks.labelValues(country, family.label, _)))

  def questions(country: String): AgreementQuestionMetrics = {
    seedQuestions(country)
    new AgreementQuestionMetrics {
      def enqueued(family: String, added: Boolean): Unit = IdentityAgreementMetrics.this.enqueued.labelValues(country, family, if (added) "added" else "duplicate").inc()
      def asked(family: String, outcome: String): Unit   = answers.labelValues(country, family, outcome).inc()
      override def inlineAsk(family: String, queuedSeconds: Double, askingSeconds: Double): Unit = {
        inlineAsks.labelValues(country, family, "queued").observe(queuedSeconds)
        inlineAsks.labelValues(country, family, "asking").observe(askingSeconds)
      }
    }
  }

  // Every country's series from boot, before its wiring builds the stage and the question queue — which seed the
  // same sets — and every relay archive's unread days at 0.
  countryCodes.foreach { c =>
    seedStage(c); seedQuestions(c)
    IdentityAgreementMetrics.RelayArchives.foreach(daysUnreadTotal.labelValues(c, _))
  }
}

object IdentityAgreementMetrics {
  /** The inline ask buckets: around the stage's 5 s wait, to the 35 s one request may take and the minutes a backlog can. */
  val InlineAskBuckets: Seq[Double] = Seq(0.1, 0.25, 0.5, 1, 2, 5, 10, 20, 35, 60, 120, 300, 600)
  /** The `archive` values of `kinowo_worker_identity_relay_days_unread_total`: the scrape archive and the accepted relay. */
  val RelayArchives: Seq[String] = Seq("archive", "accepted")
}
