package modules

import controllers.{RetiredSiteController, WellKnownController}
import models.Country
import testsupport.TestMessages.given

import org.scalatest.Assertions.fail
import play.api.mvc.{Action, AnyContent, Handler, Result, Results}
import play.api.test.{FakeRequest, Helpers}

import scala.concurrent.Future

/** A retired host's real routes ([[AppLoader.retiredRoutes]]) over stub
 *  components, and a way to send them a request — the harness both the page
 *  ([[RetiredSiteSpec]]) and the access-log ([[RetiredAccessLogSpec]]) specs drive. */
final class RetiredRouter(country: Country) {

  private val router = AppLoader.retiredRoutes(
    new RetiredSiteController(
      Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi), country),
    new WellKnownController(Helpers.stubControllerComponents()),
    file => Helpers.stubControllerComponents().actionBuilder(Results.Ok(s"asset:$file")))

  /** The action `method path` routes to, applied — failing the test when it routes nowhere. */
  def respond(method: String, path: String, headers: Seq[(String, String)] = Seq.empty): Future[Result] = {
    val request = FakeRequest(method, path).withHeaders(headers*)
    router.routes.lift(request) match {
      case Some(action: Action[?]) => action.asInstanceOf[Action[AnyContent]].apply(request)
      case Some(other: Handler)    => fail(s"$method $path routed to a non-action handler: $other")
      case None                    => fail(s"$method $path is not routed at all")
    }
  }
}
