package services

import models.{CharlieMonroe, Cinema, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, Helios, KinoApollo, KinoBulgarska, KinoMuza, KinoPalacowe, Multikino, Rialto}
import play.api.Logging
import services.cinemas.{CharlieMonroeClient, CinemaCityClient, HeliosClient, KinoApolloClient, KinoBulgarskaClient, KinoMuzaClient, KinoPalacoweClient, MultikinoClient, RialtoClient}
import services.movies.MovieCache
import services.events.{EventBus, MovieAdded}

import java.util.concurrent.{Executors, TimeUnit}

/** Scrape scheduler. Hits every cinema every 5 minutes, hands the result
 *  to `MovieCache.recordCinemaScrape`, and publishes a `MovieAdded` event
 *  per movie so the enrichment pipeline can pick them up. Holds no
 *  in-memory state of its own — `MovieCache` is the read path. */
class ShowtimeCache(
  heliosClient: HeliosClient,
  bus:          EventBus,
  movieCache:   MovieCache
) extends Logging {

  logger.info(s"Starting — commit ${Option(System.getenv("COMMIT_SHA")).getOrElse("unknown")}")

  private val sources = Map(
    Multikino             -> (() => MultikinoClient.fetch()),
    CharlieMonroe         -> (() => CharlieMonroeClient.fetch()),
    KinoPalacowe          -> (() => KinoPalacoweClient.fetch()),
    Helios                -> (() => heliosClient.fetch()),
    CinemaCityPoznanPlaza -> (() => CinemaCityClient.fetch("1078", CinemaCityPoznanPlaza)),
    CinemaCityKinepolis   -> (() => CinemaCityClient.fetch("1081", CinemaCityKinepolis)),
    KinoMuza              -> (() => KinoMuzaClient.fetch()),
    KinoBulgarska         -> (() => KinoBulgarskaClient.fetch()),
    KinoApollo            -> (() => KinoApolloClient.fetch()),
    Rialto                -> (() => RialtoClient.fetch())
  )

  private val fetchExecutor = Executors.newFixedThreadPool(sources.size, { r: Runnable =>
    val t = new Thread(r, "showtime-fetch")
    t.setDaemon(true)
    t
  })

  private val scheduler = Executors.newSingleThreadScheduledExecutor { r =>
    val t = new Thread(r, "showtime-cache-refresh")
    t.setDaemon(true)
    t
  }

  /** Schedule the periodic refresh. Fires immediately on the first tick so
   *  the cache starts warming as soon as the app is up; then every 5
   *  minutes. Each cinema fetch publishes its own `MovieAdded` events as
   *  soon as it completes, so enrichment starts work without waiting for
   *  the 10-cinema barrier. */
  def start(): Unit =
    scheduler.scheduleAtFixedRate(
      () => sources.foreach { case (cinema, fetch) =>
        fetchExecutor.submit(new Runnable { def run(): Unit = refreshOne(cinema, fetch) })
      },
      0L, 5L, TimeUnit.MINUTES
    )

  def stop(): Unit = {
    scheduler.shutdown()
    fetchExecutor.shutdown()
  }

  private def refreshOne(cinema: Cinema, fetch: () => Seq[CinemaMovie]): Unit = {
    val t0 = System.currentTimeMillis()
    try {
      val movies  = fetch()
      val elapsed = System.currentTimeMillis() - t0
      movieCache.recordCinemaScrape(cinema, movies)
      logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms")
      movies.foreach(cm => bus.publish(MovieAdded(cm.movie.title, cm.movie.releaseYear, cm.movie.originalTitle, cm.director)))
    } catch {
      case e: Exception =>
        val elapsed = System.currentTimeMillis() - t0
        logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
    }
  }
}
