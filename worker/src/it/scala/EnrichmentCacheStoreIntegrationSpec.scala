import models.Country
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoClient, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{CachedResponse, EnrichmentCache, Env, MongoEnrichmentCacheStore}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The convergence enrichment cache against a real Mongo — the half of it that
 * in-memory tests cannot reach: that entries actually survive to a second run,
 * that a country's cache is its own, and that the TTL index really is the thing
 * expiring them.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class EnrichmentCacheStoreIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster — and this store is never dropped, so a mistake
  // here would persist. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val uri = Env.get("MONGODB_URI").get

  /** A distinct database per run so a co-running leg can't see these rows — the
   *  production name (`convergence_test`) is deliberately left alone. */
  private def withStore[A](country: Country, ttl: FiniteDuration = 1.day)(body: MongoEnrichmentCacheStore => A): A = {
    val client   = MongoClient(uri)
    val database = client.getDatabase(s"kinowo_isolated_enrichcache_${ProcessHandle.current().pid()}_${System.nanoTime()}")
    try body(new MongoEnrichmentCacheStore(database, country, ttl))
    finally {
      Await.result(database.drop().toFuture(), 30.seconds)
      client.close()
    }
  }

  "the enrichment cache store" should "hand a second run back everything the first one learned" in {
    withStore(Country.Poland) { store =>
      val first = new EnrichmentCache(store)
      first.remember("GET https://api.themoviedb.org/3/search?api_key=***&query=dune", CachedResponse.Body("""{"id":438631}"""))
      first.remember("GET https://api.themoviedb.org/3/search?api_key=***&query=nope", CachedResponse.Failed(Some(404), "GET", "HTTP 404"))

      // A fresh cache over the same store is exactly what the next run builds.
      val second = new EnrichmentCache(store)
      second.preload() shouldBe 2
      second.lookup("GET https://api.themoviedb.org/3/search?api_key=***&query=dune") shouldBe
        Some(CachedResponse.Body("""{"id":438631}"""))
      second.lookup("GET https://api.themoviedb.org/3/search?api_key=***&query=nope") shouldBe
        Some(CachedResponse.Failed(Some(404), "GET", "HTTP 404"))
    }
  }

  it should "round-trip raw bytes through Mongo without loss" in {
    withStore(Country.Poland) { store =>
      val base64 = java.util.Base64.getEncoder.encodeToString(Array[Byte](0x7A, 0xBF.toByte, 0xE6.toByte, 0x00))
      store.put("BYTES https://example.test/legacy", CachedResponse.Bytes(base64))
      store.loadAll()("BYTES https://example.test/legacy") shouldBe CachedResponse.Bytes(base64)
    }
  }

  // Three country legs run concurrently, each filling its own cache; one shared
  // collection would let a German answer be read as a Polish hit.
  it should "keep each country's answers in its own collection" in {
    withStore(Country.Poland) { polish =>
      polish.collectionName shouldBe "enrichment_cache_pl"
      polish.put("GET https://example.test/a", CachedResponse.Body("polish"))

      // Same database, different country — must not see Poland's row.
      val german = new MongoEnrichmentCacheStore(polish.database, Country.Germany, 1.day)
      german.collectionName shouldBe "enrichment_cache_de"
      german.loadAll() shouldBe empty
    }
  }

  it should "declare a TTL index carrying the configured expiry" in {
    withStore(Country.Poland, ttl = 1.day) { store =>
      store.put("GET https://example.test/a", CachedResponse.Body("x"))

      val indexes = Await.result(
        store.database.getCollection(store.collectionName).listIndexes().toFuture(), 30.seconds)
      val ttlIndex = indexes.find(_.get("expireAfterSeconds").isDefined)

      withClue(s"no TTL index on ${store.collectionName}: $indexes\n") { ttlIndex should not be empty }
      ttlIndex.get.get("expireAfterSeconds").get.asNumber().longValue() shouldBe 1.day.toSeconds
      ttlIndex.get.get("key").get.asDocument().containsKey("fetchedAt") shouldBe true
    }
  }

  // Mongo's TTL reaper runs on a ~60s cadence, so an entry can outlive its TTL on
  // disk. Reading it back would resurrect a day-old failure the run was entitled
  // to retry — the `fetchedAt` floor in `loadAll` is what stops that.
  it should "ignore an entry that outlived its TTL but hasn't been reaped yet" in {
    withStore(Country.Poland, ttl = 1.day) { store =>
      val collection = store.database.getCollection(store.collectionName)
      Await.result(collection.insertOne(Document(
        "_id"       -> "GET https://example.test/stale",
        "kind"      -> "body",
        "text"      -> "yesterday",
        "fetchedAt" -> new java.util.Date(System.currentTimeMillis() - 2.days.toMillis)
      )).toFuture(), 30.seconds)

      val stored = Await.result(collection.find(Filters.eq("_id", "GET https://example.test/stale")).toFuture(), 30.seconds)
      withClue("the stale row should still be physically present: ") { stored should have size 1 }
      withClue("…but must not be served: ") { store.loadAll() shouldBe empty }
    }
  }
}
