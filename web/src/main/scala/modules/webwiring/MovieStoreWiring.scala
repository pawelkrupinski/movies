package modules.webwiring

import org.mongodb.scala.MongoDatabase
import services.movies.{DecodeFailureMetrics, MongoMovieChangeFeed, MongoMovieListingReader, MongoMovieRepository, MongoScreeningsRepository,
  MongoSlotsRepository, MovieChangeFeed, MovieListingReader, MovieRepository, ScreeningsRepository, SlotsRepository}
import services.titles.TitleNormalizer

/** The web tier's read-only view of one country's `movies` corpus: the repository (with the change feed the /debug live view
 *  watches), the side collections it stitches its showtimes and slots from, and the server-side listing read. */
final case class MovieStores(
  screenings:    ScreeningsRepository,
  slots:         SlotsRepository,
  repository:    MovieRepository & MovieChangeFeed,
  listingReader: MovieListingReader)

/** Builds [[MovieStores]] over a database the root hands in (the serving country's mirror connection, or an extra /debug
 *  country's): one place for how the web tier reads the movie corpus, so the boot country and every switchable one are wired alike. */
final class MovieStoreWiring(clock: java.time.Clock, stopwatch: tools.Stopwatch) {

  /** `normalizer` is the rules of the country whose database this is, never the serving country's by default. */
  def readOnly(database: Option[MongoDatabase], normalizer: TitleNormalizer,
               decodeFailures: DecodeFailureMetrics = DecodeFailureMetrics.noop): MovieStores = {
    val screenings = new MongoScreeningsRepository(database, clock = clock, stopwatch = stopwatch)
    val slots      = new MongoSlotsRepository(database, clock = clock, stopwatch = stopwatch)
    val repository = new MongoMovieRepository(database, clock, screenings = Some(screenings), slots = Some(slots),
      normalizer = normalizer, decodeFailures = decodeFailures, stopwatch = stopwatch) with MongoMovieChangeFeed()
    MovieStores(screenings, slots, repository, new MongoMovieListingReader(database, Some(slots), normalizer, clock))
  }
}
