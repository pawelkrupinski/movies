package services.readmodel

import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Try}

/**
 * [[ReadModelDerivationMarker]] as one document in the country database's `read_model_derivation`
 * collection (`_id: "projection"`), beside the `web_movies` / `web_screenings` it describes —
 * so a restored or copied read model carries the version it was derived under with it.
 *
 * With no database (a Mongo-less boot) there is no stored read model to re-project: it reports
 * the running version as recorded, as [[ReadModelDerivationMarker.none]] does.
 */
class MongoReadModelDerivationMarker(database: Option[MongoDatabase], clock: java.time.Clock) extends ReadModelDerivationMarker {
  import MongoReadModelDerivationMarker.{DocumentId, Timeout}

  private val collection: Option[MongoCollection[Document]] =
    database.map(_.getCollection[Document](MongoReadModelDerivationMarker.Collection))

  def recorded(): Try[Option[String]] = collection match {
    case None => Success(Some(ReadModelProjection.DerivationVersion))
    case Some(c) =>
      Try(Option(Await.result(c.find(Filters.eq("_id", DocumentId)).first().toFuture(), Timeout)))
        .map(_.flatMap(_.get("version")).map(_.asString().getValue))
  }

  def record(version: String): Unit = collection.foreach { c =>
    Await.result(c.replaceOne(Filters.eq("_id", DocumentId),
      Document("_id" -> DocumentId, "version" -> version, "recordedAt" -> java.util.Date.from(clock.instant())),
      new ReplaceOptions().upsert(true)).toFuture(), Timeout)
    ()
  }
}

object MongoReadModelDerivationMarker {
  val Collection         = "read_model_derivation"
  private val DocumentId = "projection"
  private val Timeout    = 10.seconds
}
