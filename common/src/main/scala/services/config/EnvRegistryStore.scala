package services.config

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** The cross-process catalogue of discovered config knobs. Each process
 *  [[publish]]es the (non-secret) knobs it has read, with the value it's
 *  currently using; the admin page (in web) reads [[all]] to list EVERY knob —
 *  including worker-only ones the web process never reads. This is what makes
 *  the page self-populating: a newly added `Env.*` call surfaces on its owning
 *  process's next publish, no manifest needed.
 *
 *  Real impl: Mongo `env_registry`, `_id = "app|key"`. Test impl: in-memory. */
trait EnvRegistryStore {
  /** Replace this `app`'s published knob set (so a removed key disappears). */
  def publish(app: String, knobs: Seq[RegisteredKnob]): Unit
  /** Every knob every app has published. */
  def all(): Seq[RegisteredKnob]
}

class InMemoryEnvRegistryStore extends EnvRegistryStore {
  // keyed by app -> (key -> knob)
  private val byApp = new java.util.concurrent.ConcurrentHashMap[String, Map[String, RegisteredKnob]]()
  def publish(app: String, knobs: Seq[RegisteredKnob]): Unit = {
    byApp.put(app, knobs.map(k => k.key -> k).toMap); ()
  }
  def all(): Seq[RegisteredKnob] = {
    import scala.jdk.CollectionConverters._
    byApp.values().asScala.flatMap(_.values).toVector
  }
}

class MongoEnvRegistryStore(db: MongoDatabase) extends EnvRegistryStore with Logging {
  private val coll: MongoCollection[Document] = db.getCollection[Document]("env_registry")

  def publish(app: String, knobs: Seq[RegisteredKnob]): Unit = Try {
    // Replace the whole app slice each tick: drop stale rows then re-insert, so a
    // key no longer read (or renamed) stops appearing on the page.
    Await.result(coll.deleteMany(Filters.eq("app", app)).toFuture(), 10.seconds)
    if (knobs.nonEmpty)
      Await.result(coll.insertMany(knobs.map(toDoc)).toFuture(), 10.seconds)
    ()
  }.recover { case e => logger.warn(s"EnvRegistryStore.publish($app) failed: ${e.getMessage}") }

  def all(): Seq[RegisteredKnob] =
    Try(Await.result(coll.find().toFuture(), 10.seconds)).toOption.getOrElse(Seq.empty).flatMap(fromDoc)

  private def toDoc(k: RegisteredKnob): Document =
    Document(
      "_id"     -> s"${k.app}|${k.key}",
      "app"     -> k.app,
      "key"     -> k.key,
      "kind"    -> k.kind.toString,
      "default" -> k.default.orNull,
      "current" -> k.current.orNull
    )

  private def fromDoc(d: Document): Option[RegisteredKnob] =
    for {
      app  <- str(d, "app")
      key  <- str(d, "key")
      kind <- str(d, "kind").flatMap(s => Try(Env.Kind.valueOf(s)).toOption)
    } yield RegisteredKnob(app, key, kind, str(d, "default"), str(d, "current"))

  private def str(d: Document, key: String): Option[String] =
    d.get(key).filter(_.isString).map(_.asString().getValue)
}
