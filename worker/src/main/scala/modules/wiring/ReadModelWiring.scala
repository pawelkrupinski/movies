package modules.wiring

import settings.ShareCardFirstHold

import modules.WorkerWiring
import services.identity.{IdentityCalibration, RatingGate}
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

  // ── Identity phase 3: confidence-gated ratings (docs/design/identity-resolver.md §15) ──────
  // A staged-migration switch, off by default: on, a card whose stored evidence the calibration
  // artefact scores below its display threshold is served without ratings.
  lazy val ratingGate: RatingGate =
    if (configuration.identityRatingGate.value) RatingGate.fromEvidence(IdentityCalibration.default) else RatingGate.off
}
