package services

import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Single Mongo connection shared across every repository. Prior to this,
 * each `Mongo*Repo` built its own `MongoClient` in its lazy init — Movies +
 * Users + UserStates added up to three independent connection pools, three
 * Netty event loops, three replica-set monitor threads, and three Mongo
 * driver memory footprints on the same Atlas cluster. That tipped the JVM
 * RSS past the 512 MB Fly cgroup ceiling (the OOM-restart cycle
 * `b03hleb3x` chased down — bisected to `65c9368` Phase A, which added the
 * second + third clients).
 *
 * Standard Mongo driver usage is one `MongoClient` per process. This class
 * is that single client; repos take its `database` as an
 * `Option[MongoDatabase]` constructor arg, apply their own codec registry
 * via `.withCodecRegistry(...)`, and `getCollection[T]` from the resulting
 * view. The underlying connection pool / IO threads / monitor threads are
 * shared.
 *
 * Disabled silently when `MONGODB_URI` is unset (local dev / tests).
 * `close()` is idempotent. Wiring constructs one of these and calls
 * `close()` in its shutdown hook; each repo's own `close()` becomes a
 * no-op when it borrowed the database from here.
 */
class MongoConnection extends Logging {

  private lazy val initResult: (Option[MongoClient], Option[MongoDatabase]) = init()

  /** The shared `MongoDatabase` view — pre-bound to `MONGODB_DB` (or
   *  `kinowo` as the default). Repos `.withCodecRegistry(...)` it. */
  def database: Option[MongoDatabase] = initResult._2

  def close(): Unit = initResult._1.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoDatabase]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoConnection disabled.")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val db     = client.getDatabase(dbName)
          // Touch the database to surface connectivity errors at boot
          // (same `countDocuments`-against-a-known-collection probe the
          // old per-repo init used). Picking `movies` here because it
          // exists in every environment; an empty collection still
          // round-trips fine.
          Await.result(db.getCollection("movies").countDocuments().toFuture(), 10.seconds)
          logger.info(s"MongoConnection connected to $dbName")
          (client, db)
        }.recover {
          case ex: Throwable =>
            logger.error(s"MongoConnection init failed (${ex.getMessage}) — disabled.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, d)) => (Some(c), Some(d))
          case None         => (None, None)
        }
    }
}
