package services.identity

import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The in-memory backend under the normalized store's storage contract. */
class TmdbDocumentsSpec extends TmdbDocumentRetentionBehaviour {
  protected def newDocuments(): TmdbDocuments & TmdbDocumentRetention = new InMemoryTmdbDocuments
}

/** What a backend that keeps the documents (not a decorator in front of one) owes the store's sweep. */
trait TmdbDocumentRetentionBehaviour extends TmdbDocumentsBehaviour {
  override protected def newDocuments(): TmdbDocuments & TmdbDocumentRetention
  private val unstamped = new BsonDocument("ids", new BsonArray(java.util.List.of(BsonInt32(1018))))
  private def stamped(at: Long) = new BsonDocument("ids", new BsonArray()).append(TmdbStore.FetchedAt, org.bson.BsonInt64(at))

  "the store's retention" should "name only documents fetched before the cutoff, never one without a stamp" in {
    val d = newDocuments()
    d.put(TmdbKind.Query, Seq("old" -> stamped(100), "new" -> stamped(900), "unstamped" -> unstamped))
    d.fetchedBefore(TmdbKind.Query, 500) shouldBe Seq("old" -> 100L)
    d.fetchedBefore(TmdbKind.Film, 500) shouldBe empty
  }

  it should "delete a document only while it still carries the stamp the scan read" in {
    val d = newDocuments()
    d.put(TmdbKind.Query, Seq("kept" -> stamped(100), "gone" -> stamped(100)))
    val scanned = d.fetchedBefore(TmdbKind.Query, 500)
    d.put(TmdbKind.Query, Seq("kept" -> stamped(700)))                            // re-fetched after the scan
    d.deleteIfStill(TmdbKind.Query, scanned) shouldBe 1
    d.get(TmdbKind.Query, Seq("kept", "gone")).keySet shouldBe Set("kept")
  }
}

/** What any backend of the normalized TMDB store keeps: `TmdbDocumentsSpec` (in memory) and
 *  `MongoTmdbDocumentsIntegrationSpec` (Mongo) run the same cases. */
trait TmdbDocumentsBehaviour extends AnyFlatSpec with Matchers {
  protected def newDocuments(): TmdbDocuments

  private def film(title: String) = new BsonDocument("record", new BsonDocument("title", BsonString(title))
    .append("directors", new BsonArray(java.util.List.of(BsonString("Wojciech Has"))))).append("gone", BsonNull())
  private val search = new BsonDocument("ids", new BsonArray(java.util.List.of(BsonInt32(1018), BsonInt32(9))))

  "the normalized documents" should "give back what each kind kept, by id, nested as written, and nothing never kept" in {
    val d = newDocuments()
    d.put(TmdbKind.Film, Seq("1018" -> film("Lalka")))
    d.put(TmdbKind.Query, Seq("movie|pl-PL|Lalka" -> search))
    d.get(TmdbKind.Film, Seq("1018", "9")) shouldBe Map("1018" -> film("Lalka"))
    d.get(TmdbKind.Query, Seq("movie|pl-PL|Lalka")) shouldBe Map("movie|pl-PL|Lalka" -> search)
    d.get(TmdbKind.Person, Seq("1018")) shouldBe empty                           // kinds do not share ids
  }

  it should "keep only the latest value an id was put with, and not change what a reader already holds" in {
    val d = newDocuments()
    d.put(TmdbKind.Film, Seq("1018" -> film("Lalka")))
    val held = d.get(TmdbKind.Film, Seq("1018"))("1018")
    held.put("record", BsonNull())
    d.get(TmdbKind.Film, Seq("1018"))("1018") shouldBe film("Lalka")
    d.put(TmdbKind.Film, Seq("1018" -> film("Lalka (1968)")))
    d.get(TmdbKind.Film, Seq("1018"))("1018") shouldBe film("Lalka (1968)")
  }

  it should "give an answer only the fields it reads: a film's record and hit, and of its partials only the IMDb id" in {
    val d = newDocuments()
    val stored = new BsonDocument("record", new BsonDocument("title", BsonString("Lalka")))
      .append("local", new BsonDocument("title", BsonString("Lalka")).append("imdb_id", BsonString("tt0064570")))
      .append("english", new BsonDocument("title", BsonString("The Doll")))
    d.put(TmdbKind.Film, Seq("1018" -> stored))
    d.put(TmdbKind.Film, Seq("7" -> new BsonDocument("local", new BsonDocument("title", BsonString("Partial only")))))
    d.answers(TmdbKind.Film, Seq("1018", "7", "9")) shouldBe
      Map("1018" -> new BsonDocument("record", new BsonDocument("title", BsonString("Lalka")))
        .append("local", new BsonDocument("imdb_id", BsonString("tt0064570"))).append("english", new BsonDocument()),
        "7" -> new BsonDocument("local", new BsonDocument()))
    d.get(TmdbKind.Film, Seq("1018"))("1018") shouldBe stored                       // the write path still reads it whole
  }
}

/** A read of many batches waits on their round-trips side by side — at most `InFlight` at once —
 *  and gives back every batch's result. */
class TmdbDocumentsBatchingSpec extends AnyFlatSpec with Matchers {
  "a batched read" should "keep several batches in flight, never more than InFlight, and return them all" in {
    val inFlight = new java.util.concurrent.atomic.AtomicInteger
    val peak     = new java.util.concurrent.atomic.AtomicInteger
    val pool     = java.util.concurrent.Executors.newCachedThreadPool()
    val batches  = (1 to 10).map(i => Seq(s"id$i"))
    try {
      val got = TmdbDocuments.inBatches(batches, scala.concurrent.duration.Duration(10, "seconds")) { batch =>
        scala.concurrent.Future {
          peak.accumulateAndGet(inFlight.incrementAndGet(), math.max)
          Thread.sleep(50)
          inFlight.decrementAndGet()
          batch
        }(using scala.concurrent.ExecutionContext.fromExecutor(pool))
      }
      got shouldBe batches.flatten
      peak.get should (be > 1 and be <= TmdbDocuments.InFlight)
    } finally pool.shutdownNow()
  }
}

