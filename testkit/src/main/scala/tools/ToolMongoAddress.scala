package tools

import settings.{MongoDatabaseName, MongoUri, ProcessConfiguration}

/** The cluster and database a one-off tool `main` (an audit, a profiler) reads: both required,
 *  unlike the serving [[services.MongoAddress]], whose absent URI means "run without Mongo". */
final case class ToolMongoAddress(uri: MongoUri, database: MongoDatabaseName)

object ToolMongoAddress {

  /** `MONGODB_URI` and `MONGODB_DB` (default `kinowo`) from the tool's resolved configuration;
   *  a tool with no cluster to read has nothing to do, so it exits naming the missing setting. */
  def orExit(configuration: ProcessConfiguration): ToolMongoAddress = {
    val address = configuration.mongoAddress
    val uri = address.uri.getOrElse {
      System.err.println("MONGODB_URI not set — abort.")
      sys.exit(1)
    }
    ToolMongoAddress(uri, address.database.getOrElse(MongoDatabaseName("kinowo")))
  }
}
