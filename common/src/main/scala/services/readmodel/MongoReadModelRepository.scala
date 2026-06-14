package services.readmodel

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument, OperationType}
import models.{CityScreening, ResolvedMovie}
import org.mongodb.scala.model.{Filters, IndexOptions, Indexes}
import org.mongodb.scala.{MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import play.api.Logging

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Try

/**
 * MongoDB-backed read model. Persists the denormalised projection to two
 * collections — `web_movies` ([[ResolvedMovie]]) and `web_screenings`
 * ([[CityScreening]]) — and streams their changes (incl. deletes) to consumers.
 *
 * Both case classes carry their `_id` directly, so writes filter on `_id` and
 * change-stream delete events hand us that same id (no ObjectId→id mapping).
 * The `web_screenings` collection is indexed on `city` (the web's per-city
 * query) and `filmId` (the projector's per-film prune).
 *
 * When `sharedDb` is `None` (Mongo disabled in local dev / tests without a
 * cluster) the repository is a silent no-op, mirroring `MongoMovieRepository`.
 */
class MongoReadModelRepository(sharedDb: Option[MongoDatabase]) extends ReadModelReader with ReadModelWriter with Logging {

  // Relaxed write concern (w:1, j:false): the read model is a pure projection of
  // `movies` — every document is re-derived by the projector's reconcile, so a write
  // lost to a crash is self-healing. Skipping the journal sync cuts per-write cost
  // on the shared-CPU Mongo, which the worker's projection cascade was saturating.
  private val RelaxedWrites = WriteConcern.W1.withJournal(false)
  private val movies: Option[MongoCollection[ResolvedMovie]] =
    sharedDb.map(_.withCodecRegistry(ReadModelCodecs.registry).getCollection[ResolvedMovie]("web_movies").withWriteConcern(RelaxedWrites))
  private val screenings: Option[MongoCollection[CityScreening]] =
    sharedDb.map(_.withCodecRegistry(ReadModelCodecs.registry).getCollection[CityScreening]("web_screenings").withWriteConcern(RelaxedWrites))

  // Best-effort index creation. The web's per-city read filters on `city`; the
  // projector's per-film prune filters on `filmId`. Idempotent — re-creating an
  // existing index is a no-op.
  screenings.foreach { c =>
    Try {
      Await.result(c.createIndex(Indexes.ascending("city"), new IndexOptions().background(true)).toFuture(), 10.seconds)
      Await.result(c.createIndex(Indexes.ascending("filmId"), new IndexOptions().background(true)).toFuture(), 10.seconds)
    }.recover { case exception: Throwable => logger.warn(s"web_screenings index creation failed: ${exception.getMessage}") }
  }

  def enabled: Boolean = movies.isDefined

  /** Test seam: the write concern configured on the derived collections. */
  def collectionWriteConcerns: Seq[WriteConcern] = Seq(movies, screenings).flatten.map(_.writeConcern)

  // ── Reads ─────────────────────────────────────────────────────────────────

  def findAllMovies(): Seq[ResolvedMovie] = movies match {
    case Some(c) =>
      Try(Await.result(c.find().toFuture(), 60.seconds)).recover {
        case exception: Throwable =>
          logger.warn(s"ReadModelRepository.findAllMovies failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
    case None => Seq.empty
  }

  def findAllScreenings(): Seq[CityScreening] = screenings match {
    case Some(c) =>
      Try(Await.result(c.find().toFuture(), 60.seconds)).recover {
        case exception: Throwable =>
          logger.warn(s"ReadModelRepository.findAllScreenings failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
    case None => Seq.empty
  }

  // ── Writes ──────────────────────────────────────────────────────────────────

  def upsertMovie(m: ResolvedMovie): Unit =
    replace(movies, m._id, m, "upsertMovie")

  def upsertScreening(s: CityScreening): Unit =
    replace(screenings, s._id, s, "upsertScreening")

  def deleteMovie(id: String): Unit     = removeById(movies, id, "deleteMovie")
  def deleteScreening(id: String): Unit = removeById(screenings, id, "deleteScreening")

  private def replace[T](coll: Option[MongoCollection[T]], id: String, document: T, op: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), document, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable if isClusterClosed(exception) => ()
      case exception: Throwable => logger.warn(s"ReadModelRepository.$op($id) failed: ${exception.getMessage}")
    }
  }

  private def removeById[T](coll: Option[MongoCollection[T]], id: String, op: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable if isClusterClosed(exception) => ()
      case exception: Throwable => logger.warn(s"ReadModelRepository.$op($id) failed: ${exception.getMessage}")
    }
  }

  // ── Change streams ──────────────────────────────────────────────────────────

  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit): Option[AutoCloseable] =
    movies.map(watch(_, onUpsert, onDelete, "web_movies"))

  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit): Option[AutoCloseable] =
    screenings.map(watch(_, onUpsert, onDelete, "web_screenings"))

  /** Route each insert / update / replace to `onUpsert` (full post-image via
   *  `UPDATE_LOOKUP`) and each delete to `onDelete(_id)`. The driver auto-
   *  resumes across transient blips; a terminal error logs and leaves the
   *  caller's periodic reload in charge. Requires a replica set. */
  private def watch[T: ClassTag](coll: MongoCollection[T], onUpsert: T => Unit, onDelete: String => Unit, label: String): AutoCloseable = {
    val subRef = new AtomicReference[Subscription]()
    coll.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[T]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[T]): Unit = change.getOperationType match {
          case OperationType.DELETE =>
            Option(change.getDocumentKey).flatMap(k => Option(k.getString("_id")))
              .foreach(v => try onDelete(v.getValue) catch { case exception: Throwable => logger.warn(s"$label delete-apply failed: ${exception.getMessage}") })
          case _ =>
            Option(change.getFullDocument)
              .foreach(d => try onUpsert(d) catch { case exception: Throwable => logger.warn(s"$label upsert-apply failed: ${exception.getMessage}") })
        }
        override def onError(e: Throwable): Unit =
          logger.warn(s"$label change stream ended (${e.getMessage}) — relying on the periodic reload.")
        override def onComplete(): Unit = ()
      })
    logger.info(s"MongoReadModelRepository: watching $label change stream.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }

  // Shared MongoClient owned by `MongoConnection`; this repository doesn't close it.
  def close(): Unit = ()

  private def isClusterClosed(exception: Throwable): Boolean =
    Option(exception.getMessage).exists(_.contains("state should be: open"))
}
