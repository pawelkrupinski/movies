package controllers

import play.api.Mode
import play.api.test.Helpers
import services.readmodel.{TestReadModel, WebReadModel}

/** Shared builder for a [[DebugController]]: the read model projected from
 *  `records`. Returns the concrete [[WebReadModel]] too, so a spec can `reload()`
 *  or inspect what `rehydrate` reloaded. */
object TestDebugController {

  def build(
    records: Seq[(String, Option[Int], models.MovieRecord)],
    mode: Mode = Mode.Test,
    adminAction: AdminAction = TestAdminAction(),
  ): (DebugController, WebReadModel) = {
    given play.api.i18n.Messages = testsupport.TestMessages.deployment
    val readModel = TestReadModel.fromRecords(records)
    val ctrl = new DebugController(
      cc             = Helpers.stubControllerComponents(messagesApi = testsupport.TestMessages.messagesApi),
      readModel      = readModel,
      adminAction    = adminAction,
      environment    = mode,
      servingCountry = models.Country.default,
    )
    (ctrl, readModel)
  }
}
