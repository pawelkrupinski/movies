package services.contracts

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.documentToUntypedDocument
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.keyed.{DocCodec, InMemoryKeyedDocStore, KeyedDocStore, MongoKeyedDocStore}
import tools.IsolatedMongoDatabase
import tools.ScanOutcome

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/**
 * ONE behaviour suite for [[KeyedDocStore]], run against Mongo and the in-memory store every
 * keyed store's spec runs on — so what those specs say holds for production.
 */
class KeyedDocStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "keyed-doc-store-contract")
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private final case class Row(at: Instant, note: String)
  private val codec = new DocCodec[Row] {
    def encode(row: Row): Document = Document("at" -> java.util.Date.from(row.at), "note" -> row.note)
    def decode(d: Document): Option[Row] = Option(d.getDate("at")).map(at => Row(at.toInstant, d.getString("note")))
    override def stampField: Option[String] = Some("at")
  }

  private val collections = new AtomicInteger
  private val t0          = tools.MongoTtlSpecClock.Pinned.instant()

  private val implementations: Seq[(String, () => KeyedDocStore[Row])] = Seq(
    "InMemoryKeyedDocStore" -> (() => new InMemoryKeyedDocStore(codec)),
    "MongoKeyedDocStore"    -> (() => new MongoKeyedDocStore(
      isolatedDatabase.database.getCollection[Document](s"keyed_${collections.incrementAndGet()}"), codec)))

  implementations.foreach { case (name, fresh) =>
    s"[$name]" should "answer a put value by its key, the last put winning, and nothing for another" in {
      val store = fresh()
      store.put("a", Row(t0, "first"))
      store.put("a", Row(t0, "second"))
      store.get("a") shouldBe Some(Row(t0, "second"))
      store.get("b") shouldBe None
    }

    it should "answer only the written keys of a lookup" in {
      val store = fresh()
      store.put("a", Row(t0, "a")); store.put("b", Row(t0, "b"))
      store.getAll(Seq("a", "b", "never", "a")) shouldBe Map("a" -> Row(t0, "a"), "b" -> Row(t0, "b"))
      store.getAll(Nil) shouldBe empty
    }

    it should "scan every row, across more than one page" in {
      val store = fresh()
      val keys  = (0 until 2100).map(i => f"k$i%04d")
      keys.foreach(store.put(_, Row(t0, "x")))
      val seen = Seq.newBuilder[String]
      store.scan(page => seen ++= page.map(_._1)) shouldBe ScanOutcome.Complete
      seen.result() shouldBe keys
      store.all().keySet shouldBe keys.toSet
    }

    it should "forget a deleted key" in {
      val store = fresh()
      store.put("a", Row(t0, "a"))
      store.delete("a"); store.delete("never")
      store.get("a") shouldBe None
      store.all() shouldBe empty
    }

    it should "record a detached put" in {
      val store = fresh()
      store.putDetached("a", Row(t0, "a"))(e => fail(e))
      tools.Eventually.eventually(store.get("a") shouldBe Some(Row(t0, "a")))
    }

    it should "sweep the rows stamped before a cutoff, keeping one written again since" in {
      val store = fresh()
      store.put("old", Row(t0.minusSeconds(3600), "old"))
      store.put("again", Row(t0.minusSeconds(3600), "again"))
      store.put("new", Row(t0, "new"))
      val stale = store.retention.stampedBefore(t0.minusSeconds(60))
      stale.map(_._1).toSet shouldBe Set("old", "again")
      store.put("again", Row(t0, "again"))
      store.retention.deleteIfStill(stale) shouldBe 1
      store.all().keySet shouldBe Set("again", "new")
    }
  }
}
