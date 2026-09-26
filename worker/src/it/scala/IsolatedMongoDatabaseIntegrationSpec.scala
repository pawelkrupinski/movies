package integration

import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.IsolatedMongoDatabase

import scala.concurrent.Await
import scala.concurrent.duration._

/** Each isolated database is a handle its suite owns: dropping it takes that database and
 *  nothing else — the earlier process-wide registry let one suite's tidy-up reach another's. */
class IsolatedMongoDatabaseIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, 30.seconds)

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
}
