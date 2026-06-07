package integration

import org.mongodb.scala.{MongoClient, SingleObservableFuture}
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

  "Two MongoCachingDetailFetch instances sharing a collection" should "fetch the underlying only once for the same URL" in {
    val url   = s"https://chain/film/${System.nanoTime()}"
    val under = new CountingFetch
    val serverA = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)
    val serverB = new MongoCachingDetailFetch(under, Some(db), 1.hour, collName)

    serverA.get(url) shouldBe s"<html>$url</html>" // fetches + stores
    Thread.sleep(300) // let the fire-and-forget store land
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
}
