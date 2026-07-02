package services.config

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
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

object EnvRegistryStore {

  /** The minimal set of writes to make a stored app-slice match `desired`.
   *  Empty (`isNoOp`) when they already agree — so an unchanged registry issues
   *  no writes at all. The previous publish blindly `deleteMany` + `insertMany`
   *  the whole slice every config-refresh tick (~30s), which was ~46% of the
   *  worker's change-stream traffic and 100% redundant (every re-insert was
   *  byte-identical to the row it replaced). */
  final case class ReconcilePlan(deleteIds: Seq[String], upserts: Seq[RegisteredKnob]) {
    def isNoOp: Boolean = deleteIds.isEmpty && upserts.isEmpty
  }

  private def idOf(k: RegisteredKnob): String = s"${k.app}|${k.key}"

  def reconcile(current: Seq[RegisteredKnob], desired: Seq[RegisteredKnob]): ReconcilePlan = {
    val cur = current.map(k => idOf(k) -> k).toMap
    val des = desired.map(k => idOf(k) -> k).toMap
    val deleteIds = (cur.keySet -- des.keySet).toSeq.sorted
    val upserts   = des.toSeq.sortBy(_._1).collect { case (id, k) if !cur.get(id).contains(k) => k }
    ReconcilePlan(deleteIds, upserts)
  }
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

class MongoEnvRegistryStore(sharedDb: Option[MongoDatabase]) extends EnvRegistryStore with Logging {
  private val coll: Option[MongoCollection[Document]] =
    sharedDb.map(_.getCollection[Document]("env_registry"))

  def publish(app: String, knobs: Seq[RegisteredKnob]): Unit = coll.foreach { c =>
    Try {
      // Reconcile against what's already stored so an unchanged registry writes
      // nothing. Only rows that actually changed get an upsert, and a key no
      // longer read (or renamed) is deleted — the page stays correct without the
      // per-tick delete-all + insert-all churn.
      val current = Await.result(c.find(Filters.eq("app", app)).toFuture(), 10.seconds).flatMap(fromDoc)
      val plan    = EnvRegistryStore.reconcile(current, knobs)
      if (plan.deleteIds.nonEmpty)
        Await.result(c.deleteMany(Filters.in("_id", plan.deleteIds*)).toFuture(), 10.seconds)
      plan.upserts.foreach { k =>
        Await.result(
          c.replaceOne(Filters.eq("_id", idOf(k)), toDoc(k), ReplaceOptions().upsert(true)).toFuture(),
          10.seconds)
      }
      ()
    }.recover { case e => logger.warn(s"EnvRegistryStore.publish($app) failed: ${e.getMessage}") }
  }

  private def idOf(k: RegisteredKnob): String = s"${k.app}|${k.key}"

  def all(): Seq[RegisteredKnob] =
    coll.flatMap(c => Try(Await.result(c.find().toFuture(), 10.seconds)).toOption)
      .getOrElse(Seq.empty).flatMap(fromDoc)

  // Build the doc with only the fields that are present. A `null` value can't go
  // through the Scala Mongo `Document` builder (it throws), and an unset knob has
  // a None current (and a get-knob has a None default) — so the previous
  // `.orNull` blew up the whole insertMany batch, leaving env_registry empty.
  private[config] def toDoc(k: RegisteredKnob): Document = {
    val base = Document(
      "_id"  -> s"${k.app}|${k.key}",
      "app"  -> k.app,
      "key"  -> k.key,
      "kind" -> k.kind.toString)
    base ++
      k.default.map(v => Document("default" -> v)).getOrElse(Document()) ++
      k.current.map(v => Document("current" -> v)).getOrElse(Document())
  }

  private[config] def fromDoc(d: Document): Option[RegisteredKnob] =
    for {
      app  <- str(d, "app")
      key  <- str(d, "key")
      kind <- str(d, "kind").flatMap(s => Try(Env.Kind.valueOf(s)).toOption)
    } yield RegisteredKnob(app, key, kind, str(d, "default"), str(d, "current"))

  private def str(d: Document, key: String): Option[String] =
    d.get(key).filter(_.isString).map(_.asString().getValue)
}
