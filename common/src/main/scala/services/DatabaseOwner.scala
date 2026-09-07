package services

import models.Country
import org.mongodb.scala.{MongoDatabase, SingleObservableFuture}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}

import scala.concurrent.Await
import scala.concurrent.duration._

/** Which country a Mongo database belongs to.
  *
  * Every worker prunes its country's read model against its own corpus, so two
  * countries writing one database delete each other's cards on every sweep. Each
  * database is per country by default (`Country.dbNameFor`), and an explicit
  * `MONGODB_DB` overrides that for local development — which is exactly the knob a
  * mis-set deployment turns into the shared-database failure. The first worker to
  * boot stamps the database with its country; a later worker for another country
  * refuses to start rather than take the database over.
  */
final class DatabaseOwner(database: MongoDatabase) {
  private val owners = database.getCollection[Document](DatabaseOwner.Collection)

  /** The country stamped on this database, if any. */
  def owner(): Option[String] =
    Await.result(owners.find(Filters.eq("_id", DatabaseOwner.Id)).first().toFutureOption(), 10.seconds)
      .flatMap(_.get("country").map(_.asString().getValue))

  /** Stamp `country` on this database, or throw when another country already owns it. */
  def claim(country: Country): Unit = owner() match {
    case Some(other) if other != country.code =>
      throw new IllegalStateException(
        s"database '${database.name}' belongs to country '$other', refusing to run '${country.code}' on it: " +
          "two countries on one database prune each other's read model. Unset MONGODB_DB or point it at " +
          s"'${country.mongoDb}'.")
    case Some(_) => ()
    case None =>
      Await.result(owners.replaceOne(Filters.eq("_id", DatabaseOwner.Id),
        Document("_id" -> DatabaseOwner.Id, "country" -> country.code), new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
  }
}

object DatabaseOwner {
  val Collection = "database_owner"
  val Id         = "owner"
}
