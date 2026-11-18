package controllers

import services.movies.LatestTitleYear


import services.movies.SingleCountryNormalizer

import play.api.Mode
import play.api.test.Helpers
import services.movies.InMemoryMovieRepository
import services.readmodel.{TestReadModel, WebReadModel}

/** Shared builders for the /debug pages' controllers: a fully-wired [[DebugController]]
 *  over an in-memory corpus behind a single-country [[DebugStack]], the
 *  [[DebugReadModelController]] over the read model projected from the same `records`
 *  (unless `readModels` says otherwise), the [[TuneController]] and the
 *  [[RehydrateController]] with the [[WebReadModel]] it reloads. */
object TestDebugController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    cinemaSourceUrls: Map[String, String] = Map.empty,
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
    clock: java.time.Clock = java.time.Clock.fixed(java.time.Instant.parse("2026-10-03T12:00:00Z"), java.time.ZoneOffset.UTC),
  )(using latestYear: LatestTitleYear = LatestTitleYear(java.time.Clock.fixed(java.time.Instant.parse("2026-10-03T12:00:00Z"), java.time.ZoneOffset.UTC))): (DebugController, WebReadModel) = {
    val readModel = TestReadModel.fromRecords(records)
    val ctrl = new DebugController(
      cc               = Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi),
      debugCountries   = debugCountries.getOrElse(DebugCountries.single(new DebugStack(
        models.Country.default,
        movieRepository.getOrElse(new InMemoryMovieRepository(records, normalizer = SingleCountryNormalizer.titleNormalizer)),
        ratingCadenceReader, attemptReader))),
      environment      = mode,
      cinemaSourceUrls = () => cinemaSourceUrls,
      clock            = clock,
    )
    (ctrl, readModel)
  }

  private val Pinned = java.time.Clock.fixed(java.time.Instant.parse("2026-10-03T12:00:00Z"), java.time.ZoneOffset.UTC)

  /** /debug/readmodel over `readModels`, else the serving country's read model projected from `records`. */
  def readModelPages(records: Seq[(String, Option[Int], models.MovieRecord)], mode: Mode = Mode.Test,
                     readModels: Option[DebugReadModels] = None, clock: java.time.Clock = Pinned): DebugReadModelController =
    new DebugReadModelController(Helpers.stubControllerComponents(), mode,
      readModels.getOrElse(DebugReadModels.of(models.Country.default, TestReadModel.store(records), mirror = None)), clock)

  /** The serving country's city routes over `readModel`, as the composition root shares them. */
  private def cityRequests(readModel: WebReadModel): CityRequests =
    new CityRequests(testsupport.TestMessages.messagesApi, models.Country.default, readModel, TestResponseCache(),
      now = () => TestResponseCache.FixedNow, build = settings.CommitSha("unknown"))

  def tunePages(mode: Mode = Mode.Test): TuneController =
    new TuneController(Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi), mode,
      cityRequests(TestReadModel.fromRecords(Seq.empty)))

  /** `rehydrate` over the read model projected from `records`, returned too so a spec can see what it reloaded. */
  def rehydrate(records: Seq[(String, Option[Int], models.MovieRecord)],
                adminAction: AdminAction = TestAdminAction()): (RehydrateController, WebReadModel) = {
    val readModel = TestReadModel.fromRecords(records)
    (new RehydrateController(Helpers.stubControllerComponents(), cityRequests(readModel), readModel, adminAction), readModel)
  }
}
