package integration

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.attempts.{AttemptOutcome, EnrichmentAttempt, MongoEnrichmentAttemptStore}
import services.closure.{LedgerEntry, LedgerKind, MongoClosureLedger}
import services.enrichment.MongoOmdbAttemptStore
import services.tasks.{BulkTaskResult, ChunkPageMemo, MongoBulkTaskResultStore, MongoChunkPageMemo, TaskType}
import tools.Eventually.eventually
import tools.SpecTimeouts

import scala.concurrent.Await

/**
 * THE STORED SHAPE of the small keyed stores — each collection's name, every document's fields
 * in the order they are written, and the TTL indexes — pinned against a real Mongo, because prod
 * data lives in these collections: a store that wrote `ranAt` as `ran_at`, or rebuilt its TTL
 * index with another expiry, would orphan every row already there.
 *
 * Runs in a database of its own, dropped in `afterAll`.
 */
class KeyedStoreDocumentShapeIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated        = tools.IsolatedMongoDatabase.open(mongoTarget, "keyed-store-shapes")
  private val db: MongoDatabase = isolated.database
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val at   = tools.MongoTtlSpecClock.Pinned.instant()
  /** `at` as the documents' JSON spells a date. */
  private val date = Document("d" -> java.util.Date.from(at)).toJson().stripPrefix("""{"d": """).stripSuffix("}")

  private def documents(collection: String): Seq[String] =
    Await.result(db.getCollection[Document](collection).find().toFuture(), SpecTimeouts.Io).map(_.toJson()).sorted

  /** Every index but `_id_`, as `name key expireAfterSeconds`. */
  private def indexes(collection: String): Set[String] =
    Await.result(db.getCollection[Document](collection).listIndexes().toFuture(), SpecTimeouts.Io)
      .filterNot(_.getString("name") == "_id_")
      .map(i => s"${i.getString("name")} ${i.get("key").map(_.asDocument.toJson).getOrElse("")} ${i.get("expireAfterSeconds").map(_.asNumber.longValue).getOrElse("")}")
      .toSet

  // The two `$set`-upserted stores wrote their fields in the order Mongo's `$set` adds new fields: by name.
  "enrichment_attempts" should "hold one {_id, at, detail, durationMs, outcome} document per key, and no index" in {
    val store = new MongoEnrichmentAttemptStore(Some(db))
    store.record("imdb|tmdb:7", EnrichmentAttempt(at, 12L, AttemptOutcome.Changed("8.3")))
    store.record("rt|tmdb:7", EnrichmentAttempt(at, 3L, AttemptOutcome.Failed("boom")))
    eventually(documents("enrichment_attempts") shouldBe Seq(
      s"""{"_id": "imdb|tmdb:7", "at": $date, "detail": "8.3", "durationMs": 12, "outcome": "changed"}""",
      s"""{"_id": "rt|tmdb:7", "at": $date, "detail": "boom", "durationMs": 3, "outcome": "failed"}"""))
    store.record("imdb|tmdb:7", EnrichmentAttempt(at, 5L, AttemptOutcome.Unchanged))
    eventually(documents("enrichment_attempts").head shouldBe
      s"""{"_id": "imdb|tmdb:7", "at": $date, "detail": "", "durationMs": 5, "outcome": "unchanged"}""")
    indexes("enrichment_attempts") shouldBe empty
  }

  "omdb_attempts" should "hold {_id, at, level} documents under a 90-day TTL on at" in {
    val store = new MongoOmdbAttemptStore(Some(db))
    store.record("anora|2024", 3, at)
    eventually(documents("omdb_attempts") shouldBe Seq(s"""{"_id": "anora|2024", "at": $date, "level": 3}"""))
    eventually(indexes("omdb_attempts") shouldBe Set("""at_1 {"at": 1} 7776000"""))
  }

  "bulk_task_results" should "hold {_id: taskType} beside ranAt, succeeded, message and the counts, by name, and drop a count a later run lacks" in {
    val store = new MongoBulkTaskResultStore(Some(db))
    store.record(BulkTaskResult(TaskType.RefreshAllImdb, at, succeeded = true, "tick done",
      walked = Some(578), changed = Some(4), discovered = Some(0), failed = Some(1)))
    documents("bulk_task_results") shouldBe Seq(
      s"""{"_id": "RefreshAllImdb", "changed": 4, "discovered": 0, "failed": 1, "message": "tick done", "ranAt": $date, "succeeded": true, "walked": 578}""")
    store.record(BulkTaskResult(TaskType.RefreshAllImdb, at, succeeded = false, "failed: boom"))
    documents("bulk_task_results") shouldBe Seq(
      s"""{"_id": "RefreshAllImdb", "message": "failed: boom", "ranAt": $date, "succeeded": false}""")
    indexes("bulk_task_results") shouldBe empty
  }

  "venue_closures" should "hold {_id: cinema, confirmedAt, kind, evidence}, and lose a withdrawn venue" in {
    val ledger = new MongoClosureLedger(db)
    ledger.record("Kino Gone", LedgerEntry(LedgerKind.SourceDropped, at, "page gone"))
    ledger.record("Kino Shut", LedgerEntry(LedgerKind.Closed, at, "closed sign"))
    documents("venue_closures") shouldBe Seq(
      s"""{"_id": "Kino Gone", "confirmedAt": $date, "kind": "source-dropped", "evidence": "page gone"}""",
      s"""{"_id": "Kino Shut", "confirmedAt": $date, "kind": "closed", "evidence": "closed sign"}""")
    ledger.withdraw("Kino Gone")
    documents("venue_closures").map(_.take(21)) shouldBe Seq("""{"_id": "Kino Shut", """)
    indexes("venue_closures") shouldBe empty
  }

  "scrape_chunk_pages" should "hold {_id: cinema|key, page, parser, slice, at} under a 7-day TTL on at" in {
    val memo = new MongoChunkPageMemo(Some(db), tools.MongoTtlSpecClock.Pinned)
    memo.remember("Odeon Norwich", "2026-10-02", ChunkPageMemo.Entry("12:34", "1", "[]"))
    documents("scrape_chunk_pages") shouldBe Seq(
      s"""{"_id": "Odeon Norwich|2026-10-02", "page": "12:34", "parser": "1", "slice": "[]", "at": $date}""")
    eventually(indexes("scrape_chunk_pages") shouldBe Set("""at_1 {"at": 1} 604800"""))
  }
}
