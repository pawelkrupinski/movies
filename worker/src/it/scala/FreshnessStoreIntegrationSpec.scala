package integration

import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, MongoFreshnessStore}
import tools.Env

import java.util.Date
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The boot hydrate's enrichment phase (~13k stamps in prod, one per film per source) is now
 * keyset-PAGED, not one unbounded `find(kind != scrape).toFuture()` — that single cursor
 * brushed the 60s timeout and can StackOverflow the async driver as the corpus grows (Sentry
 * KINOWO-19; the same class that emptied the read-model boot seed). The 60s timeout only
 * reproduces at prod scale; this guards the paging MECHANISM — batchSize forced to 2 over 5
 * sentinel stamps (3 pages) must load every stamp into the mirror across page boundaries.
 * Requires MONGODB_URI; skips otherwise.
 */
class FreshnessStoreIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))

  "MongoFreshnessStore boot hydrate" should "page the enrichment phase across batch boundaries, loading every stamp" in {
    val coll = db.getCollection("freshness")
    val ids  = (0 until 5).map(i => s"__it-freshness-page-${i}__")
    val at   = new Date()
    try {
      ids.foreach(id => Await.result(
        coll.insertOne(Document("_id" -> id, "kind" -> FreshnessKind.DetailEnrich.label, "lastFetchedAt" -> at)).toFuture(),
        10.seconds))
      // Construction kicks off the two-phase hydrate on a daemon thread; batchSize 2 → 3 pages.
      val store = new MongoFreshnessStore(Some(db), hydrateBatchSize = 2)
      Await.result(store.whenReady(FreshnessKind.DetailEnrich), 30.seconds) // restReady = enrichment phase done
      ids.foreach(id => withClue(s"$id should have hydrated across page boundaries: ")(
        store.lastFetchedAt(id) shouldBe defined))
    } finally ids.foreach(id => Await.result(coll.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds))
  }
}
