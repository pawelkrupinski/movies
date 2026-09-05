package integration

import com.mongodb.event.{CommandListener, CommandStartedEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import java.util.concurrent.ConcurrentHashMap
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Indexes
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoTtlIndex
import tools.Env

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * Live test of `services.MongoTtlIndex` against a real mongod, because the whole
 * point of it is WHICH COMMANDS IT SENDS — and the driver only names those against
 * a server.
 *
 * The load-bearing case is `sends no collMod when the expiry already matches`. The
 * code this replaced fired `collMod` on every construction and swallowed the
 * result at `debug`; against a local throwaway mongod (no auth, so the test user
 * may do anything) that succeeded and looked fine, while in production
 * `kinowo_app` holds `readWrite`, which does not carry `collMod`, so every one
 * came back `Unauthorized` and counted as a mongod user assert. A command counter
 * is what separates those two worlds: the old behaviour sends one command per
 * boot regardless, the new one sends none once the index agrees.
 */
class MongoTtlIndexIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  /** Commands the driver actually put on the wire, by name. */
  private val commands = new ConcurrentHashMap[String, java.util.concurrent.atomic.AtomicInteger]()

  private val listener = new CommandListener {
    override def commandStarted(event: CommandStartedEvent): Unit = {
      commands.computeIfAbsent(event.getCommandName, _ => new java.util.concurrent.atomic.AtomicInteger()).incrementAndGet()
      ()
    }
  }

  // `codecRegistry` EXPLICITLY, because building settings by hand skips what
  // `MongoClient(uri)` does for you: without it the java driver's bare registry
  // cannot decode into a scala `Document` and every read dies with "The BsonCodec
  // can only encode to Bson".
  private val client = MongoClient(
    MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(Env.get("MONGODB_URI").get))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)
      .addCommandListener(listener)
      .build()
  )

  private val database: MongoDatabase = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))

  private def sent(command: String): Int = Option(commands.get(command)).map(_.get()).getOrElse(0)
  private def forget(): Unit            = commands.clear()

  /** A fresh sentinel collection per case, so one case's index can never decide
   *  another's starting state. Dropped in `afterAll` with the rest. */
  private val sentinels = scala.collection.mutable.ListBuffer.empty[String]

  private def sentinel(name: String): MongoCollection[Document] = {
    val collectionName = s"__integration_test_ttl_$name"
    sentinels += collectionName
    val collection = database.getCollection[Document](collectionName)
    Await.ready(collection.drop().toFuture(), 10.seconds)
    // A collection has to EXIST before listIndexes/collMod address anything.
    Await.result(database.createCollection(collectionName).toFuture(), 10.seconds)
    collection
  }

  private def expiryOf(collection: MongoCollection[Document], field: String): Option[Long] =
    Await.result(collection.listIndexes().toFuture(), 10.seconds)
      .find(_.get("key").exists(_.asDocument().containsKey(field)))
      .flatMap(_.get("expireAfterSeconds")).map(_.asNumber().longValue())

  override protected def afterAll(): Unit = try {
    sentinels.foreach(name => Await.ready(database.getCollection(name).drop().toFuture(), 10.seconds))
    client.close()
  } finally super.afterAll()

  "MongoTtlIndex.reconcile" should "create the TTL index when the collection has none" in {
    val collection = sentinel("create")
    forget()

    MongoTtlIndex.reconcile(database, collection, "at", 86400L, "spec")

    expiryOf(collection, "at") shouldBe Some(86400L)
    sent("createIndexes") shouldBe 1
    // Nothing to reconcile on a collection that had no index — asking mongod to
    // alter one would be a command that can only fail.
    sent("collMod") shouldBe 0
  }

  it should "send no collMod when the expiry already matches" in {
    val collection = sentinel("agrees")
    MongoTtlIndex.reconcile(database, collection, "at", 86400L, "spec")
    expiryOf(collection, "at") shouldBe Some(86400L)

    // Second call is the one under test: this is what every pod boot after the
    // first does, and it is where the ~300 rejected commands per rollout came from.
    forget()
    MongoTtlIndex.reconcile(database, collection, "at", 86400L, "spec")

    sent("listIndexes") shouldBe 1
    sent("collMod") shouldBe 0
    sent("createIndexes") shouldBe 0
    expiryOf(collection, "at") shouldBe Some(86400L)
  }

  it should "reconcile an existing index whose expiry disagrees" in {
    val collection = sentinel("disagrees")
    Await.result(collection.createIndex(
      Indexes.ascending("at"),
      new com.mongodb.client.model.IndexOptions().expireAfter(100L, TimeUnit.SECONDS)
    ).toFuture(), 10.seconds)
    expiryOf(collection, "at") shouldBe Some(100L)

    forget()
    MongoTtlIndex.reconcile(database, collection, "at", 86400L, "spec")

    // collMod is the ONLY thing that can change an existing TTL, so it has to go
    // out here — the read-back is what decides that, not a createIndex conflict.
    sent("collMod") shouldBe 1
    expiryOf(collection, "at") shouldBe Some(86400L)
  }

  it should "ignore a compound index that merely mentions the field" in {
    val collection = sentinel("compound")
    Await.result(collection.createIndex(
      Indexes.compoundIndex(Indexes.ascending("service"), Indexes.ascending("at"))
    ).toFuture(), 10.seconds)

    forget()
    MongoTtlIndex.reconcile(database, collection, "at", 86400L, "spec")

    // A TTL index is single-field by definition; the compound one is not the index
    // being reconciled, so the single-field TTL still has to be CREATED.
    sent("createIndexes") shouldBe 1
    sent("collMod") shouldBe 0
    val ttlIndexes = Await.result(collection.listIndexes().toFuture(), 10.seconds)
      .filter(_.get("expireAfterSeconds").isDefined)
    ttlIndexes.flatMap(_.get("key")).map(_.asDocument().keySet().asScala.toSet) shouldBe Seq(Set("at"))
  }
}
