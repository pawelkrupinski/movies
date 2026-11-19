package controllers

import models._
import play.api.mvc._
import play.api.Mode

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

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
      val stack   = debugCountries.stackFor(country)
      implicit val ec: scala.concurrent.ExecutionContext = cc.executionContext
      val recordsFuture = Future(stack.ratingCadence())
      val listingFuture = Future(stack.listing())
      val (records, listing) = Await.result(recordsFuture.zip(listingFuture), 70.seconds)
      implicit val c: City = City.all.head   // only for the shared debug navbar's city link
      Ok(views.html.cadence(services.cadence.CadenceReport.build(records.value, listing.value.titleByTmdb.get), clock.instant(),
        // The OLDER of the two snapshots' stamps: the page is as behind as its stalest half.
        current = country, sameOrigin = debugCountries.switchable,
        mirror = Seq(records, listing).minBy(_.mirrorNewest.getOrElse(java.time.Instant.MAX)).mirrorAge(clock.instant())))
        .withCookies(debugCountries.selectionCookie(request).toSeq*)
    }
  }
}
