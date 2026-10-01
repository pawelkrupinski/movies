package services.identity

import com.mongodb.{ConnectionString, MongoClientSettings}
import com.mongodb.event.{CommandListener, CommandStartedEvent}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** The incremental model's families over Mongo: what a `replace` keeps comes back, and a take-up's
 *  write of every family is a few round trips, not one per family — a US take-up writes ~2,200 of
 *  them, and one awaited `replaceOne` each was ~30 s of every boot's projection. */
class MongoIdentityModelStoreIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def family(n: Int): StoredFamily = {
    val key = ListingKey.Published(s"Venue $n", s"Film $n", Some(2000 + n % 20), Seq(s"Director $n"))
    StoredFamily(StoredFamily.idOf(Seq(key)), IdentityResolver.RegionFamily(
      listings  = Set(key),
      decisions = Nil,
      blockKeys = Set(s"film $n"),
      queries   = Set.empty,
      films     = Set(n),
      reads     = CorpusContext.Reads(Set(s"film $n"), Set.empty, Set.empty, Set.empty, Set(n), Set.empty),
      nodeKeys  = Map(key -> s"node-$n")), digest = n.toLong)
  }

  private def withStore(label: String)(body: (MongoIdentityModelStore, () => Seq[String]) => Unit): Unit = {
    val commands = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit = { commands.add(event.getCommandName); () }
      }).build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, label))
    try body(new MongoIdentityModelStore(db), () => commands.asScala.toSeq)
    finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
  }

  "the identity model's store" should "keep what a replace adds and drop what it removes" in withStore("model-store-round-trip") { (store, _) =>
    val first = (1 to 5).map(family)
    store.replace(Set.empty, first)
    store.families().sortBy(_.id) shouldBe first.sortBy(_.id)
    val again = family(2).copy(digest = 99L)
    store.replace(Set(first.head.id), Seq(again))
    store.families().sortBy(_.id) shouldBe (first.drop(2) :+ again).sortBy(_.id)
  }

  it should "write a take-up's families in one round trip, not one per family" in withStore("model-store-bulk") { (store, commands) =>
    store.replace(Set.empty, (1 to 200).map(family))
    commands().count(_ == "update") shouldBe 1
    store.families() should have size 200
  }
}
