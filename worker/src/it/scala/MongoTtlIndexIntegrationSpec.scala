package integration

import com.mongodb.event.{CommandListener, CommandStartedEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import java.util.concurrent.ConcurrentHashMap
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Indexes
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{MongoTtlIndex, UptimeMonitor}
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
class MongoTtlIndexIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with Eventually {

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
    // A collection has to EXIST before listIndexes or dropIndex address anything.
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

    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")

    expiryOf(collection, "at") shouldBe Some(86400L)
    sent("createIndexes") shouldBe 1
    // Nothing to reconcile on a collection that had no index — asking mongod to
    // alter one would be a command that can only fail.
    sent("collMod") shouldBe 0
  }

  it should "send no collMod when the expiry already matches" in {
    val collection = sentinel("agrees")
    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")
    expiryOf(collection, "at") shouldBe Some(86400L)

    // Second call is the one under test: this is what every pod boot after the
    // first does, and it is where the ~300 rejected commands per rollout came from.
    forget()
    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")

    sent("listIndexes") shouldBe 1
    sent("collMod") shouldBe 0
    sent("createIndexes") shouldBe 0
    expiryOf(collection, "at") shouldBe Some(86400L)
  }

  it should "reconcile an existing index whose expiry disagrees, by REBUILDING it" in {
    val collection = sentinel("disagrees")
    Await.result(collection.createIndex(
      Indexes.ascending("at"),
      new com.mongodb.client.model.IndexOptions().expireAfter(100L, TimeUnit.SECONDS)
    ).toFuture(), 10.seconds)
    expiryOf(collection, "at") shouldBe Some(100L)

    forget()
    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")

    expiryOf(collection, "at") shouldBe Some(86400L)
    // DROP THEN CREATE, NOT collMod. `readWrite` carries dropIndex and createIndex and
    // not collMod (asked of the server: db.getRole("readWrite", {showPrivileges:true})),
    // so the rebuild is the only reconciliation `kinowo_app` can perform itself. A spec
    // that accepted a collMod here would pass locally — where the test user is
    // unrestricted — and describe something production cannot do.
    withClue("the reconciler used collMod, which kinowo_app is not authorised for: ")(sent("collMod") shouldBe 0)
    sent("dropIndexes")  shouldBe 1
    sent("createIndexes") shouldBe 1
    // The read-back after the rebuild is what would catch a drop that succeeded and a
    // create that did not, so a reconciled index must leave nothing outstanding.
    MongoTtlIndex.Mismatches.names should not contain collection.namespace.getCollectionName
  }

  it should "report a mismatch when the index cannot be reconciled at all" in {
    // A collection that does not exist and cannot be created is the only way to reach
    // the un-reconcilable branch without a restricted user: an invalid collection name
    // fails both the read and the create.
    val collection = database.getCollection[Document]("__integration_test_ttl_$bad$name")
    try {
      MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")
      // BY NAMESPACE, NOT BY COLLECTION NAME. A worker JVM builds one wiring per country and
      // every country owns a collection of each name, so a bare name is not an identity: one
      // country reconciling its copy would clear another country's record of a broken one.
      MongoTtlIndex.Mismatches.names should contain (collection.namespace.getFullName)
      MongoTtlIndex.Mismatches.count should be > 0
    } finally
      // The register is process-wide, so a spec that leaves an entry in it changes what every
      // later spec in this JVM reads from the gauge.
      MongoTtlIndex.Mismatches.resolved(collection.namespace.getFullName)
  }

  /** WHAT THE REGISTER IS KEYED BY, which is the whole reason the gauge can be trusted.
   *  A worker JVM builds one wiring per country in `KINOWO_COUNTRIES`, and every country
   *  owns an `uptimeBuckets`, a `resolve_*` and a `detailCache-*` of its own. Keyed by the
   *  bare collection name, one country reconciling its copy clears another country's record
   *  of a broken one — the gauge falls to zero with the index still wrong. The collision
   *  itself is exercised in `TtlIndexMetricsSpec`; what this pins is that `reconcile`
   *  supplies a namespace rather than a name, which is what makes the two distinguishable. */
  it should "record a mismatch under its full namespace, not the bare collection name" in {
    val collection = database.getCollection[Document]("__integration_test_ttl_$clash$")
    try {
      MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")
      MongoTtlIndex.Mismatches.names should contain (s"${database.name}.__integration_test_ttl_$$clash$$")
      MongoTtlIndex.Mismatches.names should not contain "__integration_test_ttl_$clash$"
    } finally MongoTtlIndex.Mismatches.resolved(collection.namespace.getFullName)
  }

  /** THE READ TIER MUST NOT DROP AN INDEX THE WRITER DEPENDS ON. Both tiers write
   *  `uptimeBuckets` and both authenticate as `kinowo_app`, so "it cannot" was never true —
   *  it is a choice this code has to make. `ensure` makes it: create when absent, report
   *  when it disagrees, never rebuild. Asserted beside `reconcile` on the SAME starting
   *  state, because the difference between them is the whole point. */
  it should "leave a disagreeing index alone under `ensure`, where `reconcile` would rebuild it" in {
    val collection = sentinel("ensure")
    Await.result(collection.createIndex(
      Indexes.ascending("at"),
      new com.mongodb.client.model.IndexOptions().expireAfter(100L, TimeUnit.SECONDS)
    ).toFuture(), 10.seconds)

    forget()
    MongoTtlIndex.ensure(collection, "at", 86400L, "spec")

    withClue("the read tier dropped an index it does not own: ")(sent("dropIndexes") shouldBe 0)
    sent("createIndexes") shouldBe 0
    withClue("the read tier changed an expiry it does not own: ")(expiryOf(collection, "at") shouldBe Some(100L))
    // And it does not raise the gauge: the owner is what reports, or the same index would be
    // counted once per process that noticed it.
    MongoTtlIndex.Mismatches.names should not contain collection.namespace.getFullName

    // The owner, on the identical state, does rebuild it.
    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")
    expiryOf(collection, "at") shouldBe Some(86400L)
  }

  it should "still create a missing index under `ensure`, so a cold collection is not left unreaped" in {
    val collection = sentinel("ensure-cold")
    forget()
    MongoTtlIndex.ensure(collection, "at", 86400L, "spec")
    expiryOf(collection, "at") shouldBe Some(86400L)
    sent("dropIndexes") shouldBe 0
  }

  /** THE WIRING RULE, not just the helper. `MongoTtlIndex.ensure` behaving well is worth
   *  nothing if the serving app never calls it, and a flag at a call site is one a future
   *  wiring can drop without any test noticing. `UptimeMonitor` derives it from
   *  `surfaceExternalWrites`, which is what `web.modules.Wiring` sets — so constructing one
   *  the way that wiring does is the assertion. */
  it should "not rebuild the bucket index from a monitor wired the way the serving app wires it" in {
    val buckets = database.getCollection[Document]("uptimeBuckets")
    Await.ready(buckets.drop().toFuture(), 10.seconds)
    Await.result(database.createCollection("uptimeBuckets").toFuture(), 10.seconds)
    Await.result(buckets.createIndex(
      Indexes.ascending("bucket"),
      new com.mongodb.client.model.IndexOptions().expireAfter(100L, TimeUnit.SECONDS)
    ).toFuture(), 10.seconds)
    try {
      forget()
      new UptimeMonitor(Some(database), surfaceExternalWrites = true)
      // The index work runs on a daemon thread, so give it room to have done the wrong thing.
      eventually(timeout(Span(5, Seconds)), interval(Span(150, Millis))) {
        sent("listIndexes") should be > 0
      }
      withClue("the serving app rebuilt an index it does not own: ")(sent("dropIndexes") shouldBe 0)
      expiryOf(buckets, "bucket") shouldBe Some(100L)
    } finally Await.ready(buckets.drop().toFuture(), 10.seconds)
  }

  it should "ignore a compound index that merely mentions the field" in {
    val collection = sentinel("compound")
    Await.result(collection.createIndex(
      Indexes.compoundIndex(Indexes.ascending("service"), Indexes.ascending("at"))
    ).toFuture(), 10.seconds)

    forget()
    MongoTtlIndex.reconcile(collection, "at", 86400L, "spec")

    // A TTL index is single-field by definition; the compound one is not the index
    // being reconciled, so the single-field TTL still has to be CREATED.
    sent("createIndexes") shouldBe 1
    sent("collMod") shouldBe 0
    val ttlIndexes = Await.result(collection.listIndexes().toFuture(), 10.seconds)
      .filter(_.get("expireAfterSeconds").isDefined)
    ttlIndexes.flatMap(_.get("key")).map(_.asDocument().keySet().asScala.toSet) shouldBe Seq(Set("at"))
  }
}
