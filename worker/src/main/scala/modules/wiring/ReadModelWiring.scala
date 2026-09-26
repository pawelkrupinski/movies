package modules.wiring

import settings.ShareCardFirstHold

import modules.WorkerWiring
import services.identity.{RatingGate, ShadowDecisions}
import services.readmodel.{MongoReadModelDerivationMarker, MongoReadModelRepository, ReadModelProjector, ReadModelReader, ReadModelWriter}

/** ── Denormalised read model (web_movies + web_screenings) ───────────────────
 *  The worker projects every `movies` write into the two read-model collections
 *  the serving app consumes. One impl is both reader (boot-seed the diff state)
 *  and writer (upsert/delete the derived documents). */
trait ReadModelWiring { self: WorkerWiring =>

  // Typed as the read+write intersection so test wirings can swap in
  // `InMemoryReadModelRepository` (Mongo-free fixture replay).
  lazy val readModelRepository: ReadModelReader & ReadModelWriter = new MongoReadModelRepository(mongoConnection.database, decodeFailures = taskMetrics)
  lazy val readModelProjector = new ReadModelProjector(movieRepository, readModelRepository, readModelRepository, taskMetrics,
    shareCards = shareCardLedger, firstCardHold = configuration.shareCardFirstHold(ShareCardFirstHold(ReadModelProjector.DefaultFirstCardHold)), clock = clock,
    pruneInterval  = configuration.readModelPruneInterval(ReadModelProjector.DefaultPruneInterval),
    pruneBootDelay = configuration.readModelPruneBootDelay(ReadModelProjector.DefaultPruneBootDelay),
    derivationMarker = new MongoReadModelDerivationMarker(mongoConnection.database, clock),
    ratingGate = ratingGate)

  // ── Identity phase 3: confidence-gated ratings (docs/design/identity-resolver.md) ──────
  // A staged-migration switch, off by default: on, a card whose identity decision is below the
  // threshold calibrated from the shadow diff is served without ratings. The resolver's shadow
  // output has not landed, so `shadowDecisions` is empty and even a switched-on gate withholds
  // nothing yet. The gate snapshots the shadow at boot.
  lazy val shadowDecisions: ShadowDecisions = ShadowDecisions.none
  lazy val ratingGate: RatingGate =
    if (configuration.identityRatingGate.value) RatingGate.fromShadow(shadowDecisions) else RatingGate.off
}
