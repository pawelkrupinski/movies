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

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    // Injectable so a spec can assert what the blob cache ended up holding —
    // the "filter variants mint no blob" bound.
    responseCache: EncodedResponseCache = new EncodedResponseCache,
    // Which country's host this controller pretends to be. Defaults to Poland,
    // matching an unset KINOWO_COUNTRY; a spec exercising another country's
    // deployment passes it here rather than mutating the shared process env.
    servingCountry: models.Country = models.Country.default,
    // The UI language the views render in. Defaults to the deployment's Polish,
    // matching `servingCountry`'s default; a spec exercising another country's
    // host passes that country's `Lang` so the copy it asserts on is the copy
    // that host actually serves.
    messages: play.api.i18n.Messages = testsupport.TestMessages.deployment,
    // A read model the caller built itself — and therefore still holds the
    // backing store for, so a spec can push INCREMENTAL change-stream events
    // (one city's showtime moving) rather than only a whole-corpus `reload()`.
    // Defaults to projecting `records`, which is what most specs want.
    readModel: Option[WebReadModel] = None,
  ): (MovieController, WebReadModel) = {
    given play.api.i18n.Messages = messages
    val readModel_ = readModel.getOrElse(TestReadModel.fromRecords(records))
    val ctrl  = new MovieController(
      cc                     = Helpers.stubControllerComponents(),
      movieControllerService = new MovieControllerService(readModel_),
      readModel              = readModel_,
      oauthProviders         = Set.empty,
      environment            = mode,
      responseCache          = responseCache,
      // No live HTTP: a poster fetch that returns nothing decodes to None, so
      // the OG card falls back to text-only — fine for controller specs that
      // don't assert on the card image itself.
      ogCardService          = new tools.OgCardService((_: String) => None),
      cityOgCardService      = new tools.CityOgCardService((_: String) => None),
      servingCountry         = servingCountry,
    )
    (ctrl, readModel_)
  }
}
