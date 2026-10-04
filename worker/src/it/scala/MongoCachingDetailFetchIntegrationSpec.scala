package integration

import org.mongodb.scala.{ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoCachingDetailFetch
import tools.GetOnlyHttpFetch
import tools.Eventually.eventually

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of `MongoCachingDetailFetch` against real Mongo: two instances
 * sharing one collection (standing in for two worker servers) must fetch the
 * underlying URL only once — the cross-server detail dedup the in-process cache
 * can't give. Requires MONGODB_URI; skips otherwise. Runs in a database of
 * its own, dropped in afterAll.
 */
class MongoCachingDetailFetchIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  // A database of its own (`IsolatedMongoDatabase` refuses a real cluster), dropped in afterAll.
  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "caching-detail-fetch")
  private val db       = isolated.database
  private val collName = services.MongoCachingDetailFetch.Collection
  private val Chain    = services.DetailCacheChain("test-chain")

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private class CountingFetch extends GetOnlyHttpFetch {
    @volatile var gets = 0
    override def get(url: String): String = { gets += 1; s"<html>$url</html>" }
  }

  /** Wait until the fire-and-forget store has landed in Mongo (the doc is keyed
   *  by `_id == url`), polling rather than racing a fixed sleep — a 300ms sleep
   *  lost the race on a slow CI Mongo, so the second instance missed the cache
   *  and re-fetched, failing `gets == 1` intermittently. */
  private def awaitStored(url: String): Unit = { storedDocument(s"${Chain.name}|$url"); () }

  /** The cache document under `id`, polled for until the fire-and-forget store lands. */
  private def storedDocument(id: String): org.mongodb.scala.Document = {
    def read = Await.result(db.getCollection(collName).find(Filters.eq("_id", id)).headOption(), 5.seconds)
    eventually(withClue(s"$id never stored: ")(read should not be empty), timeoutMs = 10.seconds.toMillis)
    read.get
  }

  "Two MongoCachingDetailFetch instances sharing a collection" should "fetch the underlying only once for the same URL" in {
    val url   = s"https://chain/film/${System.nanoTime()}"
    val under = new CountingFetch
    val serverA = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)
    val serverB = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)

    serverA.get(url) shouldBe s"<html>$url</html>" // fetches + stores
    awaitStored(url)                               // wait out the fire-and-forget store (no race)
    serverB.get(url) shouldBe s"<html>$url</html>" // served from Mongo — no new underlying fetch

    under.gets shouldBe 1
  }

  it should "re-fetch a different URL (cache is per-URL)" in {
    val under = new CountingFetch
    val server = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)
    server.get(s"https://chain/a/${System.nanoTime()}")
    server.get(s"https://chain/b/${System.nanoTime()}")
    under.gets shouldBe 2
  }

  /** The point of the Mongo cache is that one server's knowledge spares the fleet, and
   *  that has to include "this page is gone". 98 permanently-missing detail pages in the
   *  Polish corpus were being re-fetched by every server on every pass, and the films
   *  they belong to never got the year/director their TMDB resolution is gated on. */
  "A permanently-missing detail page" should "be fetched once fleet-wide, not once per server" in {
    val url   = s"https://chain/film/gone-${System.nanoTime()}"
    val under = new CountingFetch {
      override def get(u: String): String = { gets += 1; throw new tools.HttpStatusException(404, "GET", u, None) }
    }
    val serverA = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)
    val serverB = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)

    a [tools.HttpStatusException] should be thrownBy serverA.get(url)
    awaitStored(url)
    a [tools.HttpStatusException] should be thrownBy serverB.get(url)
    a [tools.HttpStatusException] should be thrownBy serverA.get(url)
    under.gets shouldBe 1
  }

  it should "keep its status, so callers still see a 404 rather than a generic failure" in {
    val url   = s"https://chain/film/gone-status-${System.nanoTime()}"
    val under = new CountingFetch {
      override def get(u: String): String = { gets += 1; throw new tools.HttpStatusException(410, "GET", u, None) }
    }
    val server = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)
    a [tools.HttpStatusException] should be thrownBy server.get(url)
    awaitStored(url)
    the [tools.HttpStatusException] thrownBy server.get(url) should have (Symbol("code") (410))
  }

  /** ONE collection holds every chain's cache, each chain with its own TTL. A TTL index's expiry is the
   *  collection's, so the TTL rides on each DOCUMENT instead (`expireAt`, under one index expiring at that
   *  instant): Helios's 2h and Cinema City's 6h can share it, which per-collection indexes could not —
   *  two chains asking for one meant one expiry silently losing. */
  "Chains with different TTLs" should "share the one collection, each document expiring on its own chain's TTL" in {
    val helios      = new MongoCachingDetailFetch(new CountingFetch, Some(db), 2.hours, services.DetailCacheChain("helios"), new services.TtlIndexMismatches)
    val cinemaCity  = new MongoCachingDetailFetch(new CountingFetch, Some(db), 6.hours, services.DetailCacheChain("cinema-city"), new services.TtlIndexMismatches)
    val (h, c)      = (s"https://helios.pl/api/movie/${System.nanoTime()}", s"https://www.cinema-city.pl/filmy/${System.nanoTime()}")
    helios.get(h); cinemaCity.get(c)
    def lifetime(id: String): Long = {
      val d = storedDocument(id)
      (d("expireAt").asDateTime.getValue - d("fetchedAt").asDateTime.getValue) / 1000
    }
    lifetime(s"helios|$h") shouldBe 2.hours.toSeconds
    lifetime(s"cinema-city|$c") shouldBe 6.hours.toSeconds
    awaitExpireAtIndex()
  }

  it should "not serve a document past its expireAt, though the TTL reaper has not removed it yet" in {
    val url   = s"https://chain/film/stale-${System.nanoTime()}"
    val under = new CountingFetch
    Await.result(db.getCollection(collName).insertOne(org.mongodb.scala.Document("_id" -> s"${Chain.name}|$url", "body" -> "<html>stale</html>",
      "fetchedAt" -> new java.util.Date(0L), "expireAt" -> new java.util.Date(1000L))).toFuture(), 5.seconds)
    new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, new services.TtlIndexMismatches).get(url) shouldBe s"<html>$url</html>"
    under.gets shouldBe 1
  }

  /** The index is built on a daemon thread, so poll for it rather than race a sleep. */
  private def awaitExpireAtIndex(): Unit = {
    def current: Option[Long] =
      Await.result(db.getCollection(collName).listIndexes().toFuture(), 5.seconds)
        .find(_.get("key").exists(_.asDocument().containsKey("expireAt")))
        .flatMap(_.get("expireAfterSeconds")).map(_.asNumber().longValue())
    eventually(current shouldBe Some(0L), timeoutMs = 10.seconds.toMillis)
    ()
  }

  it should "NOT be remembered when the failure is transient, so a 5xx still retries" in {
    val url   = s"https://chain/film/flaky-${System.nanoTime()}"
    val under = new CountingFetch {
      override def get(u: String): String = { gets += 1; throw new tools.HttpStatusException(503, "GET", u, None) }
    }
    val server = new MongoCachingDetailFetch(under, Some(db), 1.hour, Chain, ttlMismatches = new services.TtlIndexMismatches)
    a [tools.HttpStatusException] should be thrownBy server.get(url)
    a [tools.HttpStatusException] should be thrownBy server.get(url)
    under.gets shouldBe 2
  }
}
