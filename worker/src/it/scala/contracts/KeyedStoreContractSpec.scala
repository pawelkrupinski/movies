package services.contracts

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{DocCodec, DocumentExpiry, InMemoryStoreDocuments, IsolatedMongoDatabase, KeyCodec, KeyedStore, MongoStoreDocuments,
  ScanOutcome, SpecTimeouts}

/**
 * ONE behaviour suite for [[KeyedStore]], run over Mongo's documents and the in-memory ones every migrated store's spec
 * runs on -- so what those specs say holds for production. Keyed by text and by number, as the stores are.
 */
class KeyedStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "keyed-store-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private val codec = new DocCodec[String] {
    def encode(note: String): Document              = Document("note" -> note)
    def decode(document: Document): Option[String] = document.get("note").filter(_.isString).map(_.asString.getValue)
  }

  private val clock       = tools.MongoTtlSpecClock.Pinned
  private val collections = new AtomicInteger
  private val expiry      = DocumentExpiry("at", 1.hour)

  private def mongoCollection(name: String) = isolatedDatabase.freshCollection(s"keyed_${name}_${collections.incrementAndGet()}")

  private val implementations: Seq[(String, Option[DocumentExpiry] => tools.StoreDocuments)] = Seq(
    "InMemoryStoreDocuments" -> (e => e.fold(new InMemoryStoreDocuments())(InMemoryStoreDocuments.expiring(_, clock))),
    "MongoStoreDocuments"    -> (e => MongoStoreDocuments.of(mongoCollection("docs"), e)))

  implementations.foreach { case (name, documents) =>
    def text()   = new KeyedStore(documents(None), KeyCodec.Text, codec)
    def number() = new KeyedStore(documents(None), KeyCodec.Number, codec)

    s"[$name]" should "answer a put value by its key, the last put winning, and nothing for another" in {
      val store = text()
      store.put("a", "first"); store.put("a", "second")
      store.get("a") shouldBe Some("second")
      store.get("b") shouldBe None
    }

    it should "answer only the written keys of a lookup" in {
      val store = number()
      store.put(1L, "one"); store.put(2L, "two")
      store.getAll(Seq(1L, 2L, 3L, 1L)) shouldBe Map(1L -> "one", 2L -> "two")
      store.getAll(Nil) shouldBe empty
    }

    it should "scan every row in _id order, across more than one page, numbers by value" in {
      val store = number()
      val keys  = (0L until 2100L).map(_ * 7 - 50)
      store.putAll(scala.util.Random(1).shuffle(keys).map(_ -> "x"))
      val seen = Seq.newBuilder[Long]
      store.scan(page => seen ++= page.map(_._1)) shouldBe ScanOutcome.Complete
      seen.result() shouldBe keys
    }

    it should "skip a document that does not decode, never failing the read" in {
      val docs  = documents(None)
      val store = new KeyedStore(docs, KeyCodec.Text, codec)
      store.put("good", "kept")
      docs.replace(Seq(new org.bson.BsonDocument("_id", new org.bson.BsonString("bad")).append("note", new org.bson.BsonInt32(3))))
      docs.replace(Seq(new org.bson.BsonDocument("_id", new org.bson.BsonInt32(9)).append("note", new org.bson.BsonString("number key"))))
      store.all() shouldBe Seq("good" -> "kept")
      store.get("bad") shouldBe None
    }

    it should "delete one key, and many, saying how many were held" in {
      val store = text()
      store.putAll(Seq("a" -> "a", "b" -> "b", "c" -> "c"))
      store.delete("a") shouldBe true
      store.delete("never") shouldBe false
      store.deleteAll(Seq("b", "c", "never")) shouldBe 2
      store.deleteAll(Nil) shouldBe 0
      store.all() shouldBe empty
    }

    it should "insert a key once, refusing it while it is held, and take it once" in {
      val store = text()
      store.insert("code", "first") shouldBe true
      store.insert("code", "second") shouldBe false
      store.get("code") shouldBe Some("first")
      store.take("code") shouldBe Some("first")
      store.take("code") shouldBe None
      store.insert("code", "again") shouldBe true
    }

    it should "grant one of many racing inserts and takes of a key" in {
      val store = text()
      given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
      def race[A](body: => A) = Await.result(Future.sequence((1 to 8).map(_ => Future(body))), SpecTimeouts.Io)
      race(store.insert("raced", "x")).count(identity) shouldBe 1
      race(store.take("raced")).flatten shouldBe Seq("x")
    }

    it should "raise a counter from 0, keep the value beside it, and stop at a ceiling" in {
      val store = text()
      store.put("day", "note")
      store.counted("day", "used") shouldBe 0
      store.increment("day", "used") shouldBe Some(1)
      store.increment("day", "used", by = 2) shouldBe Some(3)
      store.increment("day", "used", below = Some(4)) shouldBe Some(4)
      store.increment("day", "used", below = Some(4)) shouldBe None
      store.counted("day", "used") shouldBe 4
      store.get("day") shouldBe Some("note")
      store.increment("fresh", "used", below = Some(1)) shouldBe Some(1)
      store.increment("fresh", "used", below = Some(1)) shouldBe None
    }

    it should "count every one of many racing increments, and none past the ceiling" in {
      val store = text()
      given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
      val raised = Await.result(Future.sequence((1 to 20).map(_ => Future(store.increment("budget", "used", below = Some(15))))), SpecTimeouts.Io)
      raised.flatten.sorted shouldBe (1L to 15L)
      store.counted("budget", "used") shouldBe 15
    }
  }

  "MongoStoreDocuments with an expiry" should "keep a TTL index on the expiry's field" in {
    val collection = mongoCollection("ttl")
    MongoStoreDocuments.of(collection, Some(expiry))
    tools.Eventually.eventually {
      val indexes = Await.result(collection.listIndexes().toFuture(), SpecTimeouts.Io)
      indexes.find(_.get("key").exists(_.asDocument.containsKey("at"))).flatMap(_.get("expireAfterSeconds"))
        .map(_.asNumber.longValue) shouldBe Some(3600L)
    }
  }
}
