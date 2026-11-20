package controllers

import models.*
import play.api.Mode
import play.api.mvc.*

/** The dev-only rating cadence page (`/debug/cadence`) of the country `debugCountries` selects. */
class CadenceController(cc: ControllerComponents, debugCountries: DebugCountries, environment: Mode,
                        // What "now" is for the page's own age readings (the mirror badge, the cadence).
                        clock: java.time.Clock) extends AbstractController(cc) {

  /** Dev-only: the per-(rating source, film) adaptive refresh cadence. Films are
   *  grouped by their current refresh interval, slowest (most backed-off / stable)
   *  first, with the last two displayed-value changes shown on hover. Reads the
   *  worker-written `rating_cadence` collection + names the films from the same
   *  corpus-listing snapshot `/debug` renders. */
  def cadence(): Action[AnyContent] = Action { request =>
    DevMode.gate(environment) {
      val country = debugCountries.resolve(request)
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val (report, mirror) = CadenceSnapshots.report(debugCountries.stackFor(country), clock.instant())
      implicit val c: City = City.all.head   // only for the shared debug navbar's city link
      Ok(views.html.cadence(report, clock.instant(), current = country, sameOrigin = debugCountries.switchable, mirror = mirror))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }
}
