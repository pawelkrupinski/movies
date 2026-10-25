package integration

import tools.{Eventually, IsolatedMongoDatabase, SpecTimeouts, TickRun}

import org.mongodb.scala.{Document, MongoCollection, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.ServiceTags

import scala.concurrent.Await

/**
 * Another process's tags must load from the documents `tagService` really writes.
 *
 * `tagService` upserts by `service` and never sets `_id`, so Mongo gives every tag document an
 * ObjectId. The keyset-paged `load` read `_id` as a string, threw on the first document, and so
 * loaded nothing at all — from 2026-10-08 until the loop's staleness alert (which only counts a
 * run that read everything) made it visible on 2026-10-25. One more service than a page holds,
 * so the paging itself is crossed too.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ServiceTagsLoadIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "service-tags-load")
  private val collection: MongoCollection[Document] = isolated.database.getCollection(ServiceTags.Collection)

  override protected def afterAll(): Unit =
    try isolated.drop() finally super.afterAll()

  "ServiceTags.load" should "read back every service another process tagged, across pages" in {
    val writer   = new ServiceTags(Some(collection))
    val services = (1 to 1001).map(n => f"Kino $n%04d")
    services.foreach(service => writer.tagService(service, Set(s"custom:${service.replace(" ", "")}Client")))
    Eventually.eventually(
      Await.result(collection.countDocuments().toFuture(), SpecTimeouts.Io) shouldBe services.size.toLong)

    val reader = new ServiceTags(Some(collection))
    reader.load(collection) shouldBe TickRun.Completed

    reader.snapshot().size shouldBe services.size
    reader.snapshot()("Kino 1001") shouldBe Set("custom:Kino1001Client")
  }
}
