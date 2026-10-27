package modules.webwiring

import controllers.DebugController
import modules.Wiring

/** ── /:city/debug ──────────────────────────────────────────────────────────
 *  The dev-only card / film tuning pages and the admin-gated read-model rehydrate. */
trait DebugWiring { self: Wiring =>

  lazy val debugController = new DebugController(controllerComponents, webReadModel, adminAction, environmentMode,
    servingCountry = country)
}
