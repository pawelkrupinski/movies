package services

import clients.{CharlieMonroeClient, CinemaCityClient, HeliosClient, KinoBulgarskaClient, KinoPalacoweClient, KinoMuzaClient, MultikinoClient, RialtoClient}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{CharlieMonroe, Cinema, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, Helios, KinoBulgarska, KinoPalacowe, KinoMuza, Multikino, Rialto}
import play.api.Logging
import play.api.inject.ApplicationLifecycle

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import javax.inject.{Inject, Singleton}
import scala.concurrent.Future

@Singleton
class ShowtimeCache @Inject()(lifecycle: ApplicationLifecycle) extends Logging {

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

  // One latch per cinema, counted down after the very first fetch attempt (success or failure).
  private val latches: Map[Cinema, CountDownLatch] =
    sources.map { case (cinema, _) => cinema -> new CountDownLatch(1) }

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

  // Blocks until all non-disabled cinemas have completed their first fetch.
  def get(disabledCinemas: Set[String] = Set.empty): Seq[CinemaMovie] = {
    latches.foreach { case (cinema, latch) =>
      if (!disabledCinemas.contains(cinema.displayName))
        latch.await()
    }
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
      latches(cinema).countDown()
    }
  }
}
