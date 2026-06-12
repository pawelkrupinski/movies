package services.titlerules

import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import com.mongodb.client.model.changestream.ChangeStreamDocument
import play.api.Logging
import tools.Env

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** Mongo-backed title rules store, mirroring `MongoUserStateRepo` /
 *  `MongoMovieRepo`: a shared `MongoDatabase` (the web/worker connection) or a
 *  self-init fallback, all writes best-effort with a 10s timeout, and a change
 *  stream so an edit on one process reaches the other. Collection: `titleRules`,
 *  one document per [[TitleRuleRecord]]. */
class MongoTitleRulesRepo(
  sharedDb: Option[MongoDatabase] = None,
  fallbackToOwnInit: Boolean = true
) extends TitleRulesRepo with Logging {

  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[StoredTitleRuleRecord]]) =
    sharedDb match {
      case Some(db) =>
        (None, Some(db.withCodecRegistry(TitleRuleCodecs.registry).getCollection[StoredTitleRuleRecord]("titleRules")))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None)
    }
  private def clientOpt: Option[MongoClient]                            = initResult._1
  private def coll:      Option[MongoCollection[StoredTitleRuleRecord]] = initResult._2

  def enabled: Boolean = coll.isDefined

  def loadRecords(): Seq[TitleRuleRecord] = coll.map { c =>
    Try(Await.result(c.find().toFuture(), 10.seconds))
      .recover { case ex => logger.warn(s"TitleRulesRepo.loadRecords failed: ${ex.getMessage}"); Seq.empty }
      .getOrElse(Seq.empty)
      .flatMap(safeToDomain)
  }.getOrElse(Seq.empty)

  // A doc that can't be decoded (e.g. a legacy flat rule not yet migrated, or a
  // forward-compat shape) is skipped rather than crashing the whole load.
  private def safeToDomain(s: StoredTitleRuleRecord): Option[TitleRuleRecord] =
    Try(StoredTitleRuleRecord.toDomain(s)).toOption.flatten

  def upsertRecord(rec: TitleRuleRecord): Unit = coll.foreach { c =>
    Try {
      Await.result(
        c.replaceOne(Filters.eq("_id", rec.id), StoredTitleRuleRecord.fromDomain(rec),
          new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
    }.recover { case ex => logger.warn(s"TitleRulesRepo.upsertRecord(${rec.id}) failed: ${ex.getMessage}") }
  }

  def deleteRecord(id: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      ()
    }.recover { case ex => logger.warn(s"TitleRulesRepo.deleteRecord($id) failed: ${ex.getMessage}") }
  }

  override def watchChanges(onChange: () => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().subscribe(new Observer[ChangeStreamDocument[StoredTitleRuleRecord]] {
      override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
      override def onNext(change: ChangeStreamDocument[StoredTitleRuleRecord]): Unit =
        try onChange() catch { case ex: Throwable => logger.warn(s"TitleRules change-stream apply failed: ${ex.getMessage}") }
      override def onError(e: Throwable): Unit =
        logger.warn(s"TitleRules change stream ended (${e.getMessage}) — relying on the periodic backstop reload.")
      override def onComplete(): Unit = ()
    })
    logger.info("MongoTitleRulesRepo: watching change stream for rule edits.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }

  override def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[StoredTitleRuleRecord]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoTitleRulesRepo disabled.")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val db     = client.getDatabase(dbName).withCodecRegistry(TitleRuleCodecs.registry)
          val c      = db.getCollection[StoredTitleRuleRecord]("titleRules")
          Await.result(c.countDocuments().toFuture(), 10.seconds)
          logger.info(s"MongoTitleRulesRepo connected to $dbName.titleRules")
          (Some(client), Some(c))
        }.recover { case ex =>
          logger.error(s"MongoTitleRulesRepo init failed (${ex.getMessage}) — disabled.")
          (None, None)
        }.getOrElse((None, None))
    }
}
