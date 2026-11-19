package settings

import models.Country
import org.mongodb.scala.MongoClientSettings

/**
 * Where a process's Mongo is: the cluster it dials (`uri`) and, when a run names one
 * explicitly, the database it works in (`database`) instead of its country's own.
 *
 * A value rather than two environment reads scattered through the wiring, so the code that
 * opens connections is handed WHERE to connect and a spec hands it a disabled or local
 * address directly — never by setting `MONGODB_URI` in a JVM every other suite shares.
 * Only `settings.ProcessConfiguration` resolves it from the process.
 */
final case class MongoAddress(uri: Option[MongoUri], database: Option[MongoDatabaseName]) {

  /** The database `country`'s corpus lives in at this address: the explicitly named one
   *  (local dev, an integration run's throwaway database), else the country's own. */
  def databaseFor(country: Country): MongoDatabaseName = database.getOrElse(MongoDatabaseName(country.mongoDb))

  /** The connection to `country`'s database at this address ([[databaseFor]]). */
  def connectionFor(country: Country, tuning: MongoTuning): MongoConnectionSettings =
    MongoConnectionSettings.of(uri, databaseFor(country), tuning)

  /** The driver settings of the one client the process shares across its database views on this cluster
   *  (`tools.MongoClients.shared`); `None` when the address names no cluster (local opt-out) -- each connection then
   *  degrades on its own. */
  def clientSettings(tuning: MongoTuning): Option[MongoClientSettings] =
    uri.map(cluster => MongoConnectionSettings.driverSettings(cluster.value, None, tuning))
}

object MongoAddress {

  /** No cluster: every connection opened at this address stays disabled. */
  val Disabled: MongoAddress = MongoAddress(uri = None, database = None)
}
