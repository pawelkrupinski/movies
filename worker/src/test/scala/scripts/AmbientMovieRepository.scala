package scripts

import services.MongoConnection
import services.movies.MongoMovieRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

/** The movies store of the database the ambient environment names (`MONGODB_URI`, with
 *  `MONGODB_DB` / `KINOWO_COUNTRY` picking the database), for the read-and-patch scripts.
 *  The repository never dials Mongo itself, so this is where a script's connection is
 *  made. Disabled when `MONGODB_URI` is unset or unreachable (the scripts check
 *  `enabled`); closing the store closes its connection. */
object AmbientMovieRepository {
  def open(): MongoMovieRepository = {
    val connection = MongoConnection.fromEnv(required = false, tools.Env.fromProcess())
    new MongoMovieRepository(connection.database, normalizer = titleNormalizer) {
      override def close(): Unit = try super.close() finally connection.close()
    }
  }
}
