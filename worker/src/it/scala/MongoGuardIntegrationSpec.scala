package integration

import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.{Document, SingleObservableFuture}
import org.mongodb.scala.model.{Filters, Projections, Updates}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoGuard

import scala.concurrent.Await
import scala.concurrent.duration._

/** The guard a `tools.GuardedWrite` writes through, against real MongoDB: it lands over the row as
 *  read, misses one another writer moved or created, and never brings back a row deleted since. */
class MongoGuardIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated   = tools.IsolatedMongoDatabase.open(mongoTarget, "mongo-guard")
  private val collection = isolated.database.getCollection[Document]("guarded")

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, 10.seconds)
  private def read(id: String): Option[BsonDocument] =
    await(collection.find(Filters.eq("_id", id)).projection(Projections.include("v", "sub")).headOption()).map(_.toBsonDocument)
  private def guard(id: String, asRead: Option[BsonDocument]) = MongoGuard.unchanged(new BsonString(id), asRead, Seq("v", "sub"))
  private val bump = Updates.inc("v", 1)

  "MongoGuard" should "land over the row as read, sub-documents compared whole" in {
    await(collection.insertOne(Document("_id" -> "a", "v" -> 1, "sub" -> Document("x" -> 1, "y" -> "z"))).toFuture())
    MongoGuard.updateIfUnchanged(collection, guard("a", read("a")), bump, 10.seconds, insert = false) shouldBe true
    read("a").map(_.getInt32("v").getValue) shouldBe Some(2)
  }

  it should "miss a row another writer moved since the read, leaving it as that writer left it" in {
    await(collection.insertOne(Document("_id" -> "b", "v" -> 1)).toFuture())
    val asRead = read("b")
    await(collection.updateOne(Filters.eq("_id", "b"), Updates.set("v", 7)).toFuture())
    MongoGuard.updateIfUnchanged(collection, guard("b", asRead), bump, 10.seconds, insert = true) shouldBe false
    read("b").map(_.getInt32("v").getValue) shouldBe Some(7)
  }

  it should "miss, not fail, when a row absent at the read was created since" in {
    val asRead = read("c")
    await(collection.insertOne(Document("_id" -> "c", "v" -> 3)).toFuture())
    MongoGuard.updateIfUnchanged(collection, guard("c", asRead), bump, 10.seconds, insert = true) shouldBe false
    read("c").map(_.getInt32("v").getValue) shouldBe Some(3)
  }

  it should "create a row absent at the read when told it may" in {
    MongoGuard.updateIfUnchanged(collection, guard("d", read("d")), Updates.set("v", 1), 10.seconds, insert = true) shouldBe true
    read("d").map(_.getInt32("v").getValue) shouldBe Some(1)
  }

  it should "not bring back, as a replace, a row deleted since the read" in {
    await(collection.insertOne(Document("_id" -> "e", "v" -> 1)).toFuture())
    val asRead = read("e")
    await(collection.deleteOne(Filters.eq("_id", "e")).toFuture())
    MongoGuard.replaceIfUnchanged(collection, guard("e", asRead), Document("_id" -> "e", "v" -> 2), 10.seconds, insert = false) shouldBe false
    read("e") shouldBe None
  }
}
