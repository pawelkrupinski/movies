package services.readmodel

import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Try}

/**
 * [[ReadModelDerivationMarker]] as two documents in the country database's `read_model_derivation`
 * collection, beside the `web_movies` / `web_screenings` they describe — so a restored or copied
 * read model carries the version it was derived under with it: `_id: "projection"` holds the
 * recorded version, `_id: "pass"` the progress of the pass towards the next one.
 *
 * With no database (a Mongo-less boot) there is no stored read model to re-project: it reports
 * the running version as recorded, as [[ReadModelDerivationMarker.none]] does.
 */
class MongoReadModelDerivationMarker(database: Option[MongoDatabase], clock: java.time.Clock) extends ReadModelDerivationMarker {
  import MongoReadModelDerivationMarker.{PassId, ProjectionId, Timeout}

  private val collection: Option[MongoCollection[Document]] =
    database.map(_.getCollection[Document](MongoReadModelDerivationMarker.Collection))

  def recorded(): Try[Option[DerivationVersion]] = collection match {
    case None    => Success(Some(ReadModelDerivation.current))
    case Some(c) => read(c, ProjectionId).map(_.flatMap(_.get("version")).map(v => DerivationVersion(v.asString().getValue)))
  }

  def record(version: DerivationVersion): Unit =
    write(ProjectionId, Document("version" -> version.value))

  def progress(): Try[Option[DerivationProgress]] = collection match {
    case None    => Success(None)
    case Some(c) =>
      read(c, PassId).map(_.map(doc =>
        DerivationProgress(DerivationVersion(doc("version").asString().getValue), doc("nextSlice").asInt32().getValue)))
  }

  def recordProgress(progress: DerivationProgress): Unit =
    write(PassId, Document("version" -> progress.version.value, "nextSlice" -> progress.nextSlice))

  private def read(c: MongoCollection[Document], id: String): Try[Option[Document]] =
    Try(Option(Await.result(c.find(Filters.eq("_id", id)).first().toFuture(), Timeout)))

  private def write(id: String, fields: Document): Unit = collection.foreach { c =>
    Await.result(c.replaceOne(Filters.eq("_id", id),
      Document("_id" -> id, "recordedAt" -> java.util.Date.from(clock.instant())) ++ fields,
      new ReplaceOptions().upsert(true)).toFuture(), Timeout)
    ()
  }
}

object MongoReadModelDerivationMarker {
  val Collection           = "read_model_derivation"
  private val ProjectionId = "projection"
  private val PassId       = "pass"
  private val Timeout      = 10.seconds
}
