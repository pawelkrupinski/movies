package integration

import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.Filters
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoCachingDetailFetch
import tools.{Env, GetOnlyHttpFetch}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of `MongoCachingDetailFetch` against real Mongo: two instances
 * sharing one collection (standing in for two worker servers) must fetch the
 * underlying URL only once — the cross-server detail dedup the in-process cache
 * can't give. Requires MONGODB_URI; skips otherwise. Sentinel collection,
 * dropped in afterAll.
 */
class MongoCachingDetailFetchIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val client   = MongoClient(Env.get("MONGODB_URI").get)
  private val db       = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
  private val collName = "__integration_test_detail_cache"

  override protected def afterAll(): Unit = try {
    Await.ready(db.getCollection(collName).drop().toFuture(), 10.seconds)
    client.close()
  } finally super.afterAll()

  private class CountingFetch extends GetOnlyHttpFetch {
    @volatile var gets = 0
    override def get(url: String): String = { gets += 1; s"<html>$url</html>" }
  }

  /** Wait until the fire-and-forget store has landed in Mongo (the doc is keyed
   *  by `_id == url`), polling rather than racing a fixed sleep — a 300ms sleep
   *  lost the race on a slow CI Mongo, so the second instance missed the cache
   *  and re-fetched, failing `gets == 1` intermittently. */
  private def awaitStored(url: String): Unit = {
    val deadline = System.currentTimeMillis() + 10.seconds.toMillis
    while (System.currentTimeMillis() < deadline &&
           Await.result(db.getCollection(collName).find(Filters.eq("_id", url)).headOption(), 5.seconds).isEmpty)
      Thread.sleep(25)
  }

  "Two MongoCachingDetailFetch instances sharing a collection" should "fetch the underlying only once for the same URL" in {
    val url   = s"https://chain/film/${System.nanoTime()}"
    val under = new CountingFetch
    val serverA = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)
    val serverB = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)

    serverA.get(url) shouldBe s"<html>$url</html>" // fetches + stores
    awaitStored(url)                               // wait out the fire-and-forget store (no race)
    serverB.get(url) shouldBe s"<html>$url</html>" // served from Mongo — no new underlying fetch

    under.gets shouldBe 1
  }

  it should "re-fetch a different URL (cache is per-URL)" in {
    val under = new CountingFetch
    val server = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)
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
    val serverA = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)
    val serverB = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)

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
    val server = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)
    a [tools.HttpStatusException] should be thrownBy server.get(url)
    awaitStored(url)
    the [tools.HttpStatusException] thrownBy server.get(url) should have (Symbol("code") (410))
  }

  /** THE TTL IS A CONSTRUCTOR ARGUMENT AND HAS TO MEAN SOMETHING. `createIndex` cannot
   *  alter an existing expiry — it is rejected `IndexOptionsConflict` — so before
   *  `MongoTtlIndex` this collection went on reaping at whatever duration it was FIRST
   *  indexed with, no matter what the caller asked for afterwards, with one warning line
   *  as the only trace. Every change to one of these durations was silently ignored. */
  "A detail cache whose TTL has changed" should "reap on the NEW duration, not the one it was first indexed with" in {
    val name = s"__integration_test_detail_ttl_${System.nanoTime()}"
    try {
      new MongoCachingDetailFetch(new CountingFetch, Some(db), 6.hours, name)
      awaitExpiry(name, 6.hours.toSeconds)

      // A second owner-lifetime with a different duration — a redeploy after the constant moved.
      new MongoCachingDetailFetch(new CountingFetch, Some(db), 2.hours, name)
      awaitExpiry(name, 2.hours.toSeconds)
    } finally Await.ready(db.getCollection(name).drop().toFuture(), 10.seconds)
  }

  /** The index is built on a daemon thread, so poll for it rather than race a sleep — the
   *  same reason `awaitStored` exists. Fails with the expiry it actually found, so a
   *  regression here says WHICH duration won. */
  private def awaitExpiry(collection: String, wantedSeconds: Long): Unit = {
    val deadline = System.currentTimeMillis() + 10.seconds.toMillis
    def current: Option[Long] =
      Await.result(db.getCollection(collection).listIndexes().toFuture(), 5.seconds)
        .find(_.get("key").exists(_.asDocument().containsKey("fetchedAt")))
        .flatMap(_.get("expireAfterSeconds")).map(_.asNumber().longValue())
    while (System.currentTimeMillis() < deadline && !current.contains(wantedSeconds)) Thread.sleep(25)
    current shouldBe Some(wantedSeconds)
  }

  it should "NOT be remembered when the failure is transient, so a 5xx still retries" in {
    val url   = s"https://chain/film/flaky-${System.nanoTime()}"
    val under = new CountingFetch {
      override def get(u: String): String = { gets += 1; throw new tools.HttpStatusException(503, "GET", u, None) }
    }
    val server = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)
    a [tools.HttpStatusException] should be thrownBy server.get(url)
    a [tools.HttpStatusException] should be thrownBy server.get(url)
    under.gets shouldBe 2
  }
}
