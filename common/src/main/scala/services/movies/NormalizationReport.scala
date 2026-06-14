package services.movies

import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** The outcome of the most recent rule-change backfill, persisted so the admin
 *  editor (a different process) can show "what got merged" after a save. Lines
 *  are pre-rendered strings so display needs no nested codec. Single document,
 *  `_id = "latest"`, overwritten each backfill. */
case class StoredNormalizationReport(
  _id:        String,
  atEpochMs:  Long,
  merges:     Seq[String],
  splits:     Seq[String],
  reEnriched: Int
)

object NormalizationReport {
  val LatestId = "latest"

  def render(r: NormalizationRebuilder.RebuildResult, reEnriched: Int, atEpochMs: Long): StoredNormalizationReport =
    StoredNormalizationReport(
      LatestId, atEpochMs,
      merges     = r.merges.map(m => s"${m.display}${m.year.map(y => s" ($y)").getOrElse("")}  ⟵  ${m.mergedTitles.mkString("  +  ")}"),
      splits     = r.splits.map(s => s"${s.from}  ⟶  ${s.into.mkString("  +  ")}"),
      reEnriched = reEnriched)
}

object NormalizationReportCodecs {
  val registry: CodecRegistry = fromRegistries(
    fromProviders(Macros.createCodecProviderIgnoreNone[StoredNormalizationReport]()),
    DEFAULT_CODEC_REGISTRY)
}

trait NormalizationReportRepository {
  def writeLatest(report: StoredNormalizationReport): Unit
  def readLatest(): Option[StoredNormalizationReport]
  def close(): Unit = ()
}

/** Fallback when Mongo is disabled, and the fake for tests. */
class InMemoryNormalizationReportRepository extends NormalizationReportRepository {
  @volatile private var latest: Option[StoredNormalizationReport] = None
  def writeLatest(report: StoredNormalizationReport): Unit = latest = Some(report)
  def readLatest(): Option[StoredNormalizationReport] = latest
}

class MongoNormalizationReportRepository(
  sharedDb: Option[MongoDatabase] = None,
  fallbackToOwnInit: Boolean = true
) extends NormalizationReportRepository with Logging {

  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[StoredNormalizationReport]]) =
    sharedDb match {
      case Some(db) =>
        (None, Some(db.withCodecRegistry(NormalizationReportCodecs.registry)
          .getCollection[StoredNormalizationReport]("normalizationReports")))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None)
    }
  private def clientOpt = initResult._1
  private def coll      = initResult._2

  def writeLatest(report: StoredNormalizationReport): Unit = coll.foreach { c =>
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", report._id), report, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
    }.recover { case exception => logger.warn(s"NormalizationReportRepository.writeLatest failed: ${exception.getMessage}") }
  }

  def readLatest(): Option[StoredNormalizationReport] = coll.flatMap { c =>
    Try(Await.result(c.find(Filters.eq("_id", NormalizationReport.LatestId)).headOption(), 10.seconds))
      .recover { case exception => logger.warn(s"NormalizationReportRepository.readLatest failed: ${exception.getMessage}"); None }
      .getOrElse(None)
  }

  override def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[StoredNormalizationReport]]) =
    Env.get("MONGODB_URI") match {
      case None => (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val database = client.getDatabase(dbName).withCodecRegistry(NormalizationReportCodecs.registry)
            .getCollection[StoredNormalizationReport]("normalizationReports")
          Await.result(database.countDocuments().toFuture(), 10.seconds)
          (Some(client), Some(database))
        }.recover { case exception =>
          logger.error(s"MongoNormalizationReportRepository init failed (${exception.getMessage}) — disabled.")
          (None, None)
        }.getOrElse((None, None))
    }
}
