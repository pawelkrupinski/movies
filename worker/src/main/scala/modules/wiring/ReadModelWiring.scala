package modules.wiring

import settings.ShareCardFirstHold

import modules.WorkerWiring
import services.identity.{EvidenceRatingGate, IdentityCalibration}
import services.readmodel.RatingGate
import services.readmodel.{BootCorpusStudy, MongoReadModelDerivationMarker, MongoReadModelRepository, ReadModelProjector, ReadModelReader, ReadModelWriter}

/** ── Denormalised read model (web_movies + web_screenings) ───────────────────
 *  The worker projects every `movies` write into the two read-model collections
 *  the serving app consumes. One impl is both reader (boot-seed the diff state)
 *  and writer (upsert/delete the derived documents). */
trait ReadModelWiring { self: WorkerWiring =>

  // Typed as the read+write intersection so test wirings can swap in
  // `InMemoryReadModelRepository` (Mongo-free fixture replay).
  lazy val readModelRepository: ReadModelReader & ReadModelWriter = new MongoReadModelRepository(mongoConnection.database, decodeFailures = taskMetrics)
  lazy val readModelProjector = managedResources.stopping(new ReadModelProjector(movieRepository, readModelRepository, readModelRepository, taskMetrics,
    shareCards = shareCardLedger, firstCardHold = configuration.shareCardFirstHold(ShareCardFirstHold(ReadModelProjector.DefaultFirstCardHold)), clock = clock,
    pruneInterval  = configuration.readModelPruneInterval(ReadModelProjector.DefaultPruneInterval),
    pruneBootDelay = configuration.readModelPruneBootDelay(ReadModelProjector.DefaultPruneBootDelay),
    derivationMarker = new MongoReadModelDerivationMarker(mongoConnection.database, clock),
    ratingGate = ratingGate, bootStudy = Some(readModelBootStudy)))
  // What the projector's boot reads take from the cache's boot hydrate (see BootCorpusStudy).
  lazy val readModelBootStudy: BootCorpusStudy = new BootCorpusStudy(titleNormalizer)

  // ── Identity phase 3: confidence-gated ratings (docs/design/identity-resolver.md §15) ──────
  // A staged migration, country by country ([[ReadModelWiring.RatingGateCountries]]): on, a card whose
  // stored evidence the calibration artefact scores below its display threshold is served without ratings.
  lazy val ratingGate: RatingGate =
    if (ReadModelWiring.ratingGateOn(country, configuration.identityRatingGate)) EvidenceRatingGate(IdentityCalibration.ratingGate)
    else RatingGate.off
}

object ReadModelWiring {

  /** The countries whose cards are served through the rating gate: the two the deploy overlays switched on with
   *  `KINOWO_IDENTITY_RATING_GATE=true` before the choice moved here. */
  val RatingGateCountries: Set[models.Country] = Set(models.Country.Germany, models.Country.Spain)

  /** Whether `country`'s cards go through the rating gate: its own choice, or `KINOWO_IDENTITY_RATING_GATE`
   *  set — still read, so an overlay that sets it keeps the gate on wherever it does. */
  def ratingGateOn(country: models.Country, switched: settings.IdentityRatingGateEnabled): Boolean =
    RatingGateCountries(country) || switched.value
}
