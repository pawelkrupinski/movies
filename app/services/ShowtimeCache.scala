package services

import clients.{CharlieMonroeClient, CinemaCityClient, HeliosClient, KinoBulgarskaClient, KinoPalacoweClient, KinoMuzaClient, MultikinoClient, RialtoClient}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{CharlieMonroe, Cinema, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, Helios, KinoBulgarska, KinoPalacowe, KinoMuza, Multikino, Rialto}
import play.api.{Environment, Logging, Mode}
import play.api.inject.ApplicationLifecycle

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

@Singleton
class ShowtimeCache @Inject()(lifecycle: ApplicationLifecycle, env: Environment) extends Logging {

  logger.info(s"Starting — commit ${Option(System.getenv("COMMIT_SHA")).getOrElse("unknown")}")

  if (env.mode == Mode.Prod && Option(System.getenv("SCRAPINGANT_KEY")).forall(_.isEmpty))
    throw new RuntimeException("SCRAPINGANT_KEY must be set in production")

  private val cache: Cache[Cinema, Seq[CinemaMovie]] = Caffeine.newBuilder().build()

  private val sources = Map(
    Multikino             -> (() => MultikinoClient.fetch()),
    CharlieMonroe         -> (() => CharlieMonroeClient.fetch()),
    KinoPalacowe          -> (() => KinoPalacoweClient.fetch()),
    Helios                -> (() => HeliosClient.fetch()),
    CinemaCityPoznanPlaza -> (() => CinemaCityClient.fetch("1078", CinemaCityPoznanPlaza)),
    CinemaCityKinepolis   -> (() => CinemaCityClient.fetch("1081", CinemaCityKinepolis)),
    KinoMuza              -> (() => KinoMuzaClient.fetch()),
    KinoBulgarska         -> (() => KinoBulgarskaClient.fetch()),
    Rialto                -> (() => RialtoClient.fetch())
  )

  private val LoadThreshold  = sources.size - 2   // 7 of 9
  private val loadedCount    = new AtomicInteger(0)
  private val thresholdLatch = new CountDownLatch(1)

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

  // Fire immediately at startup, then every 5 minutes
  scheduler.scheduleAtFixedRate(
    () => {
      val futures = sources.map { case (cinema, fetch) =>
        fetchExecutor.submit(new Runnable { def run(): Unit = refreshOne(cinema, fetch) })
      }
      futures.foreach(f => try f.get() catch { case _: Exception => () })
    },
    0L, 5L, TimeUnit.MINUTES
  )

  lifecycle.addStopHook(() => Future.successful {
    scheduler.shutdown()
    fetchExecutor.shutdown()
  })

  // Blocks until at least LoadThreshold cinemas have completed their first fetch.
  def get(disabledCinemas: Set[String] = Set.empty): Seq[CinemaMovie] = {
    thresholdLatch.await()
    sources.flatMap { case (cinema, _) => Option(cache.getIfPresent(cinema)).getOrElse(Seq.empty) }.toSeq
  }

  private def refreshOne(cinema: Cinema, fetch: () => Seq[CinemaMovie]): Unit = {
    val t0 = System.currentTimeMillis()
    try {
      val movies  = fetch()
      val elapsed = System.currentTimeMillis() - t0
      cache.put(cinema, movies)
      logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms")
    } catch {
      case e: Exception =>
        val elapsed = System.currentTimeMillis() - t0
        logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
    } finally {
      if (loadedCount.incrementAndGet() >= LoadThreshold) thresholdLatch.countDown()
    }
  }
}
