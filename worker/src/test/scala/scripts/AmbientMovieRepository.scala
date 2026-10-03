package scripts

import org.mongodb.scala.MongoDatabase
import services.MongoConnection
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, TitleNormalizer}

/** The movies store of the database the script's resolved configuration names (`MONGODB_URI`,
 *  with `MONGODB_DB` / `KINOWO_COUNTRY` picking the database), for the read-and-patch scripts.
 *  The repository never dials Mongo itself, so this is where a script's connection is
 *  made. Disabled when `MONGODB_URI` is unset or unreachable (the scripts check
 *  `enabled`); closing the store closes its connection. */
object AmbientMovieRepository {
  def open(configuration: _root_.settings.ProcessConfiguration): MongoMovieRepository = {
    val connection = MongoConnection.forProcess(configuration, required = services.MongoRequirement.Optional)
    over(connection.database, TitleNormalizer.forCountry(configuration.country), onClose = () => connection.close())
  }

  /** The store over `database` — one country's, keyed by that country's `normalizer` (a row's `_id` is
   *  `sanitize(title)|year`, so another country's rules would split or collide rows) — with its side
   *  collections wired as the worker wires them: under the read/write split a film's slots live in
   *  `movie_slots` and its showtimes in `screenings`, so a store without them reads every film
   *  venue-less, and a script patching such a row writes that back. */
  def over(database: Option[MongoDatabase], normalizer: TitleNormalizer, onClose: () => Unit = () => ()): MongoMovieRepository =
    new MongoMovieRepository(database, screenings = Some(new MongoScreeningsRepository(database)),
      slots = Some(new MongoSlotsRepository(database)), normalizer = normalizer) {
      override def close(): Unit = try super.close() finally onClose()
    }
}
