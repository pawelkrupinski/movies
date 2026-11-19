package scripts

import models.Country
import org.mongodb.scala.MongoDatabase
import services.MongoConnection

/** A per-country script's database: `country`'s own (`Country.mongoDb`, never `MONGODB_DB`, which
 *  a prod run pins to Poland) on the cluster the resolved configuration names. */
object CountryDatabase {

  /** The connection and its database, or exit saying why not. The caller closes the connection. */
  def open(country: Country): (MongoConnection, MongoDatabase) = {
    val process    = _root_.settings.ProcessConfiguration.resolve()
    val connection = MongoConnection.forCountry(
      process.mongoAddress.copy(database = Some(_root_.settings.MongoDatabaseName(country.mongoDb))).connectionFor(country, settings.MongoTuning.from(process, settings.ProcessTier.Tool)), services.MongoRequirement.Required, country)
    val database = connection.database.getOrElse {
      println(s"${country.displayName}: could not open ${country.mongoDb} — is the Mongo tunnel up " +
        "(scripts/local-mirror/prod-tunnel.sh) and MONGODB_URI set?")
      sys.exit(1)
    }
    (connection, database)
  }
}
