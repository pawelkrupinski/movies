package integration

import tools.SpecTimeouts

import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.IsolatedMongoDatabase

import scala.concurrent.Await

/** Each isolated database is a handle its suite owns: dropping it takes that database and
 *  nothing else — the earlier process-wide registry let one suite's tidy-up reach another's. */
class IsolatedMongoDatabaseIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, SpecTimeouts.Io)

  "An isolated database" should "drop only itself, and tolerate being dropped twice" in {
    val first  = IsolatedMongoDatabase.open(mongoTarget, "isolated-handle-first")
    val second = IsolatedMongoDatabase.open(mongoTarget, "isolated-handle-second")
    val admin  = MongoClient(mongoTarget.uri.value)
    try {
      Seq(first, second).foreach(i => await(i.database.getCollection("probe").insertOne(Document("_id" -> 1)).toFuture()))
      first.drop()
      noException should be thrownBy first.drop()
      val names = await(admin.listDatabaseNames().toFuture())
      names should not contain first.database.name
      names should contain (second.database.name)
      await(second.database.getCollection("probe").countDocuments().toFuture()) shouldBe 1L
    } finally { first.drop(); second.drop(); admin.close() }
  }

  // The store under test is handed a collection that already EXISTS, so its first write's bound
  // (10 s in most stores) times no collection or database creation — the step a loaded runner's
  // mongod stalls on (FacebookRescrapeStoreContractSpec, 2026-11-22), now under the spec's own.
  it should "hand out a fresh collection already created and empty, whatever an earlier one held" in {
    val isolated = IsolatedMongoDatabase.open(mongoTarget, "isolated-fresh-collection")
    try {
      await(isolated.database.listCollectionNames().toFuture()) should not contain "store"
      val first = isolated.freshCollection("store")
      await(isolated.database.listCollectionNames().toFuture()) should contain ("store")
      await(first.insertOne(Document("_id" -> 1)).toFuture())
      val again = isolated.freshCollection("store")
      await(isolated.database.listCollectionNames().toFuture()) should contain ("store")
      await(again.countDocuments().toFuture()) shouldBe 0L
    } finally isolated.drop()
  }
}
