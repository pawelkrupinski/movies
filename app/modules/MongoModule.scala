package modules

import com.google.inject.{AbstractModule, Provides, Singleton}
import org.mongodb.scala.{MongoClient, MongoDatabase}
import play.api.{Configuration, Logger}

/** Guice module that wires the MongoDB client into the DI graph. */
class MongoModule extends AbstractModule {

  private val logger = Logger(getClass)

  @Provides
  @Singleton
  def provideMongoClient(config: Configuration): MongoClient = {
    val uri = config.get[String]("mongodb.uri")
    logger.info(s"Connecting to MongoDB at $uri")
    MongoClient(uri)
  }

  @Provides
  @Singleton
  def provideMongoDatabase(client: MongoClient, config: Configuration): MongoDatabase = {
    val dbName = config.get[String]("mongodb.database")
    logger.info(s"Using database: $dbName")
    client.getDatabase(dbName)
  }
}
