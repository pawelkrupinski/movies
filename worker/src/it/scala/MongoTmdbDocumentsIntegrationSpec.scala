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
}
