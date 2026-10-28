package modules.webwiring

import controllers.{DebugController, DebugReadModels}
import modules.Wiring
import play.api.Mode
import services.MongoConnection
import services.readmodel.MongoReadModelRepository

/** ── /debug ────────────────────────────────────────────────────────────────
 *  The dev-only card / film tuning pages and read-model dump, and the admin-gated
 *  read-model rehydrate. */
trait DebugWiring { self: Wiring =>

  // The local read-mirror (`MONGODB_MOVIES_MIRROR_URI`), which holds every country's read
  // model: /debug/readmodel reads it in Dev. One client for every country's database; only in
  // Dev, so a spec's wiring (Test) never dials whatever cluster its environment names.
  private lazy val debugMirrorClient: Option[org.mongodb.scala.MongoClient] =
    Option.when(environmentMode == Mode.Dev)(processConfiguration.mirrorMongoUri).flatten.map { mirror =>
      managedResources.register("debug read-model mirror", MongoConnection.sharedClientFor(mirror.asMongoUri,
        Some(MongoConnection.ServerSelectionTimeout(MongoConnection.LocalMirrorTimeout))))(_.close())
    }

  lazy val debugReadModels: DebugReadModels = DebugReadModels.of(country, readModelRepository,
    debugMirrorClient.map(client => db => new MongoReadModelRepository(Some(client.getDatabase(db)), stopwatch = stopwatch)))

  lazy val debugController = new DebugController(controllerComponents, webReadModel, adminAction, environmentMode,
    servingCountry = country, readModels = debugReadModels)
}
