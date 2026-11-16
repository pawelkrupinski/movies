package tools

import org.mongodb.scala.bson.{BsonDateTime, BsonString}
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
  import MongoUnrecoveredHosts.{decode, encode, idOf}
  private val Timeout = 10.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](MongoUnrecoveredHosts.Collection)

  def load(): Seq[UnrecoveredHosts.Host] =
    Await.result(collection.find(Filters.eq("chain", chain)).batchSize(MongoReplies.Default).toFuture(), Timeout)
      .flatMap(decode).sortBy(_.host)

  def failed(host: UnrecoveredHosts.Host): Unit = {
    Await.result(collection.replaceOne(Filters.eq("_id", idOf(chain, host.host)), encode(chain, host),
      ReplaceOptions().upsert(true)).toFuture(), Timeout)
    ()
  }

  def recovered(host: String): Unit = {
    Await.result(collection.deleteOne(Filters.eq("_id", idOf(chain, host))).toFuture(), Timeout)
    ()
  }
}

object MongoUnrecoveredHosts {
  val Collection = "http_unrecovered_hosts"

  private def idOf(chain: String, host: String): String = s"$chain|$host"

  /** A host's document: its chain, host, when it last failed and with what. */
  def encode(chain: String, host: UnrecoveredHosts.Host): Document =
    Document("_id" -> idOf(chain, host.host), "chain" -> chain, "host" -> host.host,
      "failedAt" -> BsonDateTime(host.failedAt.toEpochMilli)) ++
      host.lastFailure.fold(Document())(why => Document("lastFailure" -> why))

  def decode(document: Document): Option[UnrecoveredHosts.Host] =
    for {
      host <- document.get[BsonString]("host").map(_.getValue)
      at   <- document.get[BsonDateTime]("failedAt").map(t => Instant.ofEpochMilli(t.getValue))
    } yield UnrecoveredHosts.Host(host, document.get[BsonString]("lastFailure").map(_.getValue), at)
}
