package controllers

import play.api.Mode
import play.api.mvc.*

/** The dev-only `/debug/readmodel` pages: the read model (`web_movies` plus per-film
 *  `web_screenings` counts) of `?country=` (the serving country by default) — the
 *  projection's output, so a film in the corpus but missing here is a projection gap,
 *  not a scrape gap — and one film's screenings, fetched when its row is expanded. */
class DebugReadModelController(cc: ControllerComponents, environment: Mode,
                               // What `/debug/readmodel` lists, per country.
                               readModels: DebugReadModels,
                               // What "now" is for the mirror badge's age reading.
                               clock: java.time.Clock) extends AbstractController(cc) {

  /** The dump. A row's screenings come from [[debugReadModelScreenings]] on expand:
   *  inlining every showtime of the US read model (~100k screening docs) OOM'd the dev server. */
  def debugReadModel(): Action[AnyContent] = Action { request =>
    DevMode.gate(environment) {
      val country   = readModels.resolve(request)
      val readModel = readModels(country)
      readModel.dump() match {
        case Some(dump) =>
          implicit val c: models.City = models.City.all.head   // only for the shared debug navbar's city link
          Ok(views.html.debugReadModel(dump, country, readModel.source, sameOrigin = readModels.countries.sizeIs > 1,
            mirror = readModel.mirrorAge(clock.instant())))
        // Not a smaller listing: that reads as films missing from the read model.
        case None       => ServiceUnavailable(s"could not read the ${country.code} read model whole (${readModel.source})")
      }
    }
  }

  /** ONE read-model film's screening docs, every field of every showtime. */
  def debugReadModelScreenings(id: String): Action[AnyContent] = Action { request =>
    DevMode.gate(environment) {
      readModels(readModels.resolve(request)).screeningsOf(id) match {
        case Some(screenings) => Ok(views.html.debugReadModelScreenings(id, screenings))
        // Not an empty list: that reads as "this film has no screenings".
        case None             => ServiceUnavailable(s"could not read the screenings of $id")
      }
    }
  }
}
