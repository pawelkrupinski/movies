package scripts

import services.MongoConnection
import services.movies.MongoMovieRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

/** The movies store of the database the script's resolved configuration names (`MONGODB_URI`,
 *  with `MONGODB_DB` / `KINOWO_COUNTRY` picking the database), for the read-and-patch scripts.
 *  The repository never dials Mongo itself, so this is where a script's connection is
 *  made. Disabled when `MONGODB_URI` is unset or unreachable (the scripts check
 *  `enabled`); closing the store closes its connection. */
object AmbientMovieRepository {
  def open(configuration: _root_.settings.ProcessConfiguration): MongoMovieRepository = {
    val connection = MongoConnection.forProcess(configuration, required = services.MongoRequirement.Optional)
    new MongoMovieRepository(connection.database, normalizer = titleNormalizer) {
      override def close(): Unit = try super.close() finally connection.close()
    }
  }
}
