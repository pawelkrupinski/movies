package controllers

import play.api.mvc.*
import services.readmodel.WebReadModel

/** `POST /{city}/debug/rehydrate`: reload the in-memory read-model caches from Mongo.
 *  Available in every mode (unlike the dev-only /debug pages) so a production
 *  instance whose caches drifted from the derived collections can be reconciled
 *  without a redeploy — but since it runs in prod and mutates state, it's gated
 *  by [[AdminAction]] (login session + ADMIN_ALLOWLIST) rather than left open. */
class RehydrateController(cc: ControllerComponents, cityRequests: CityRequests, readModel: WebReadModel,
                          adminAction: AdminAction) extends AbstractController(cc) {

  def rehydrate(city: String): Action[AnyContent] = adminAction { request =>
    cityRequests.withCity(city, request) { _ =>
      val count = readModel.reload()
      Ok(s"rehydrated $count rows\n").as("text/plain; charset=utf-8")
    }
  }
}
