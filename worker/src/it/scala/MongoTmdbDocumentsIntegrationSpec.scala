package integration

import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import services.identity.{MongoTmdbDocuments, TmdbDocuments, TmdbDocumentsBehaviour, TmdbKind}
import tools.{IntegrationCorpusDatabase, IntegrationMongoSuite}

import scala.concurrent.Await
import scala.concurrent.duration._

/** The normalized TMDB store's storage contract over Mongo: the cases `TmdbDocumentsSpec` runs in memory. */
class MongoTmdbDocumentsIntegrationSpec extends TmdbDocumentsBehaviour with IntegrationMongoSuite {
  private val client = MongoClient(MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
    .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).build())
  private val database: MongoDatabase = client.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "tmdb"))

  protected def newDocuments(): TmdbDocuments = {
    TmdbKind.values.foreach(k => Await.result(database.getCollection(k.collection).drop().toFuture(), 30.seconds))
    new MongoTmdbDocuments(database)
  }

  // Left on the server, not trimmed after the read: the partials are the bytes a take-up spent its
  // store batches fetching and decoding (UK: 12.4 s of a 21 s context).
  "an answer's read" should "ask the server for the kind's answer fields only" in {
    val finds = new java.util.concurrent.ConcurrentLinkedQueue[org.bson.BsonDocument]()
    val watched = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (event.getCommandName == "find") finds.add(event.getCommand.clone())
      }).build())
    try {
      val documents = new MongoTmdbDocuments(watched.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "tmdb")))
      newDocuments()
      documents.answers(TmdbKind.Film, Seq("1018"))
      import scala.jdk.CollectionConverters._
      val projection = finds.asScala.toSeq.map(_.getDocument("projection", new org.bson.BsonDocument()))
      projection should have size 1
      projection.head.keySet.asScala.toSet shouldBe Set("record", "hit")
    } finally watched.close()
  }
}
