package controllers

import services.movies.LatestTitleYear


import services.movies.SingleCountryNormalizer

import play.api.Mode
import play.api.test.Helpers
import services.movies.InMemoryMovieRepository
import services.readmodel.{TestReadModel, WebReadModel}
import services.tasks.{InMemoryTaskQueue, TaskQueue}

/** Shared builder for a fully-wired [[DebugController]]: an in-memory corpus
 *  and task queue behind a single-country [[DebugStack]], and the read model
 *  projected from the same `records` (also what /debug/readmodel lists, unless
 *  `readModels` says otherwise). Returns the concrete [[WebReadModel]] too, so a
 *  spec can `reload()` or inspect what `rehydrate` reloaded. */
object TestDebugController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    cinemaSourceUrls: Map[String, String] = Map.empty,
    adminAction: AdminAction = TestAdminAction(),
    taskQueue: TaskQueue = new InMemoryTaskQueue,
    // The on-demand corpus dump's source; defaults to an in-memory repo holding
    // the same `records`. Override to drive the /debug read path (e.g. to assert
    // the two scans run concurrently).
    movieRepository: Option[services.movies.MovieRepository] = None,
    ratingCadenceReader: services.cadence.RatingCadenceReader = services.cadence.RatingCadenceReader.empty,
    attemptReader: services.attempts.EnrichmentAttemptReader = services.attempts.EnrichmentAttemptReader.empty,
    // The per-country /debug stacks. Defaults to a single-country holder wrapping
    // the collaborators above; a spec exercising the Dev country switch injects a
    // multi-country `DebugCountries` instead.
    debugCountries: Option[DebugCountries] = None,
    // What /debug/readmodel lists; the serving country's projected `records` by default.
    readModels: Option[DebugReadModels] = None,
    clock: java.time.Clock = java.time.Clock.fixed(java.time.Instant.parse("2026-10-03T12:00:00Z"), java.time.ZoneOffset.UTC),
  )(using latestYear: LatestTitleYear = LatestTitleYear(java.time.Clock.fixed(java.time.Instant.parse("2026-10-03T12:00:00Z"), java.time.ZoneOffset.UTC))): (DebugController, WebReadModel) = {
    given play.api.i18n.Messages = testsupport.TestMessages.deployment
    val readModel = TestReadModel.fromRecords(records)
    val ctrl = new DebugController(
      cc               = Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi),
      debugCountries   = debugCountries.getOrElse(DebugCountries.single(new DebugStack(
        models.Country.default,
        movieRepository.getOrElse(new InMemoryMovieRepository(records, normalizer = SingleCountryNormalizer.titleNormalizer)),
        taskQueue, ratingCadenceReader, attemptReader))),
      readModel        = readModel,
      adminAction      = adminAction,
      environment      = mode,
      cinemaSourceUrls = () => cinemaSourceUrls,
      servingCountry   = models.Country.default,
      readModels       = readModels.getOrElse(DebugReadModels.of(models.Country.default, TestReadModel.store(records), mirror = None)),
      clock            = clock,
    )
    (ctrl, readModel)
  }
}
