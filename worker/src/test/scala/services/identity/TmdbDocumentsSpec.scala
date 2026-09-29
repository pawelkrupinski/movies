package services.identity

import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The in-memory backend under the normalized store's storage contract. */
class TmdbDocumentsSpec extends TmdbDocumentsBehaviour {
  protected def newDocuments(): TmdbDocuments = new InMemoryTmdbDocuments
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

  it should "scan a kind's every id with when it was fetched, across pages, and delete by id" in {
    val d = newDocuments()
    val ids = (1 to TmdbDocuments.ScanPage + 5).map(i => f"$i%05d")
    d.put(TmdbKind.Film, ids.map(id => id -> film(id).append(TmdbStore.FetchedAt, org.bson.BsonInt64(id.toLong))))
    d.put(TmdbKind.Person, Seq("7" -> film("person")))                          // no stamp; another kind
    val seen = Seq.newBuilder[(String, Option[Long])]
    d.scan(TmdbKind.Film)(page => seen ++= page) shouldBe true
    seen.result() shouldBe ids.map(id => id -> Some(id.toLong))
    d.delete(TmdbKind.Film, ids.take(3))
    d.get(TmdbKind.Film, ids.take(4)).keySet shouldBe Set(ids(3))
    d.get(TmdbKind.Person, Seq("7")).keySet shouldBe Set("7")
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

