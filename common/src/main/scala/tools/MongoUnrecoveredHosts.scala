package tools

import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/** The hosts one breaker chain (`chain`: the worker's `scrape` or `enrich` phase) left unrecovered, in its country's
 *  `http_unrecovered_hosts`: one document per (chain, host), written as a host opens or an open host's probe fails
 *  (at most once per cooldown), deleted as it answers. */
final class MongoUnrecoveredHosts(db: MongoDatabase, chain: String) extends UnrecoveredHosts {
  private val Timeout = 10.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](MongoUnrecoveredHosts.Collection)

  def load(): Seq[UnrecoveredHosts.Host] =
    Await.result(collection.find(Filters.eq("chain", chain)).toFuture(), Timeout).flatMap { d =>
      for {
        host <- d.get[org.mongodb.scala.bson.BsonString]("host").map(_.getValue)
        at   <- d.get[BsonDateTime]("failedAt").map(t => Instant.ofEpochMilli(t.getValue))
      } yield UnrecoveredHosts.Host(host, d.get[org.mongodb.scala.bson.BsonString]("lastFailure").map(_.getValue), at)
    }.sortBy(_.host)

  def failed(host: UnrecoveredHosts.Host): Unit = {
    val document = Document("_id" -> s"$chain|${host.host}", "chain" -> chain, "host" -> host.host,
      "failedAt" -> BsonDateTime(host.failedAt.toEpochMilli)) ++
      host.lastFailure.fold(Document())(why => Document("lastFailure" -> why))
    Await.result(collection.replaceOne(Filters.eq("_id", s"$chain|${host.host}"), document,
      ReplaceOptions().upsert(true)).toFuture(), Timeout)
    ()
  }

  def recovered(host: String): Unit = {
    Await.result(collection.deleteOne(Filters.eq("_id", s"$chain|$host")).toFuture(), Timeout)
    ()
  }
}

object MongoUnrecoveredHosts {
  val Collection = "http_unrecovered_hosts"
}
