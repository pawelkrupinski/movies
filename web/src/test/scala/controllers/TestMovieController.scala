package controllers

import play.api.Mode
import play.api.test.Helpers
import services.readmodel.{TestReadModel, WebReadModel}

/** Shared builder for a fully-wired [[MovieController]] backed by an in-memory
 *  read model and no live HTTP — the seam every controller spec needs. Pass the
 *  records the read model should hold; get back the controller and the concrete
 *  [[WebReadModel]] so a test can `reload()` to bump the mtime. The dev-only
 *  `/debug*` pages have their own builder, [[TestDebugController]]. */
object TestMovieController {

  /** The instant every controller built here runs at, unless a spec passes its own. */
  val clock: java.time.Clock = java.time.Clock.fixed(java.time.Instant.parse("2026-06-10T10:00:00Z"), java.time.ZoneOffset.UTC)

  /** [[clock]]'s wall-clock time in Poland — where a spec's fixture screenings are placed from. */
  def now: java.time.LocalDateTime = java.time.LocalDateTime.now(clock.withZone(java.time.ZoneId.of("Europe/Warsaw")))

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    // Injectable so a spec can assert what the blob cache ended up holding —
    // the "filter variants mint no blob" bound.
    responseCache: EncodedResponseCache = new EncodedResponseCache,
    // Which country's host this controller pretends to be. Defaults to Poland,
    // matching an unset KINOWO_COUNTRY; a spec exercising another country's
    // deployment passes it here rather than mutating the shared process env.
    //
    // This is now also what decides the UI language: `MovieController`
    // always renders `servingCountry.language`'s bundle (no per-request
    // Accept-Language/cookie resolution any more — every visitor gets the
    // same, edge-cacheable HTML), so a spec asserting on, say, the UK
    // deployment's English copy gets it for free just by passing
    // `servingCountry = Country.UnitedKingdom` — no separate `messages`
    // parameter to keep in sync any more.
    servingCountry: models.Country = models.Country.default,
    // A read model the caller built itself — and therefore still holds the
    // backing store for, so a spec can push INCREMENTAL change-stream events
    // (one city's showtime moving) rather than only a whole-corpus `reload()`.
    // Defaults to projecting `records`, which is what most specs want.
    readModel: Option[WebReadModel] = None,
    clock: java.time.Clock = clock,
    // The deployment's `FB_APP_ID`, read per render — None skips `fb:app_id`.
    fbAppId: () => Option[String] = () => None,
  ): (MovieController, WebReadModel) = {
    val readModel_ = readModel.getOrElse(TestReadModel.fromRecords(records))
    val ctrl  = new MovieController(
      // The REAL bundles, not Play's empty stub default — `MovieController`
      // resolves its fixed deployment `Messages` off `cc.messagesApi`, so a
      // spec asserting on actual rendered copy needs the checked-in
      // `messages`/`messages.en`/… behind it.
      cc                     = Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi),
      movieControllerService = new MovieControllerService(readModel_, clock),
      readModel              = readModel_,
      oauthProviders         = Set.empty,
      environment            = mode,
      responseCache          = responseCache,
      servingCountry         = servingCountry,
      normalizer             = services.movies.TitleNormalizer.forCountry(servingCountry),
      minifier               = tools.Minifier.forMode(mode),
      fbAppId                = fbAppId,
    )
    (ctrl, readModel_)
  }
}
