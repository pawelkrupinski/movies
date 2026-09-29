package services.movies

import com.mongodb.event.{CommandListener, CommandStartedEvent}
import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.MongoIdentityModelStore
import services.observations.{MongoObservationBackend, ObservationStore}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * A find read to completion with `toFuture()` asks the server for batchSize = Int.MaxValue, so each
 * reply fills to Mongo's 16 MB cap and the driver keeps a buffer that size pooled (32 MB of idle
 * pooled read buffers in worker-uk's live heap, 2026-09-29). The largest documents came from these
 * two reads: the observation store's current scan (obs_lookups replies to 16 MB) and the identity
 * model's families (identity_model_families replies to 16 MB at every take-up). Each must ask for a
 * batch sized to its documents.
 */
class BoundedMongoRepliesSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def withFinds(label: String)(body: (org.mongodb.scala.MongoDatabase, () => Seq[org.bson.BsonDocument]) => Unit): Unit = {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val client = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new CommandListener {
        override def commandStarted(event: CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      })
      .build())
    val db = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, label))
    try body(db, () => finds.asScala.toSeq)
    finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
  }

  private def batchOf(cmd: org.bson.BsonDocument): Int =
    Option(cmd.get("batchSize")).map(_.asNumber.intValue).getOrElse(Int.MaxValue)

  "the observation store's current scan" should "ask for replies sized to observation documents" in withFinds("bounded-obs") { (db, finds) =>
    val backend = new MongoObservationBackend(db, ObservationStore.LookupsCollection, new services.TtlIndexMismatches)
    backend.eachCurrent(None)(_ => ())
    finds() should not be empty
    finds().foreach(cmd => withClue(s"$cmd ")(batchOf(cmd) should (be > 0 and be <= tools.MongoReplies.Observations)))
  }

  "the identity model's families read" should "ask for replies sized to family documents" in withFinds("bounded-families") { (db, finds) =>
    new MongoIdentityModelStore(db).families() shouldBe empty
    finds() should not be empty
    finds().foreach(cmd => withClue(s"$cmd ")(batchOf(cmd) should (be > 0 and be <= tools.MongoReplies.Families)))
  }
}
