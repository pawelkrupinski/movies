package services

import clients.{CharlieMonroeClient, CinemaCityClient, HeliosClient, KinoBulgarskaClient, KinoPalacoweClient, KinoMuzaClient, MultikinoClient, RialtoClient}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{CharlieMonroe, Cinema, CinemaCityKinepolis, CinemaCityPoznanPlaza, CinemaMovie, Helios, KinoBulgarska, KinoPalacowe, KinoMuza, Multikino, Rialto}
import akka.NotUsed
import akka.stream.scaladsl.Source
import play.api.Logging
import play.api.inject.ApplicationLifecycle

import java.util.concurrent.{Executors, TimeUnit}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{Await, Future, Promise, duration}

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

  // One Promise per cinema, completed (with the movies or Seq.empty on failure) after the
  // very first fetch attempt.  trySuccess is a no-op on subsequent refreshes, so the promise
  // is safe to reuse across 5-minute cache cycles.
  private val firstLoad: Map[Cinema, Promise[Seq[CinemaMovie]]] =
    sources.map { case (cinema, _) => cinema -> Promise[Seq[CinemaMovie]]() }

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

  // Blocks until all enabled cinemas have completed their first fetch.
  def get(disabledCinemas: Set[String] = Set.empty): Seq[CinemaMovie] = {
    firstLoad.foreach { case (cinema, promise) =>
      if (!disabledCinemas.contains(cinema.displayName))
        Await.ready(promise.future, duration.Duration.Inf)
    }
    sources.flatMap { case (cinema, _) => Option(cache.getIfPresent(cinema)).getOrElse(Seq.empty) }.toSeq
  }

  // Emits one Seq[CinemaMovie] per enabled cinema as each first fetch completes.
  // Fully non-blocking: mapAsyncUnordered registers callbacks on the promises and
  // emits results in completion order without ever blocking a thread.
  def stream(disabledCinemas: Set[String]): Source[Seq[CinemaMovie], NotUsed] = {
    val enabled = firstLoad.toList.filterNot { case (cinema, _) => disabledCinemas.contains(cinema.displayName) }
    if (enabled.isEmpty) return Source.empty[Seq[CinemaMovie]]
    Source(enabled).mapAsyncUnordered(enabled.size) { case (_, promise) => promise.future }
  }

  private def refreshOne(cinema: Cinema, fetch: () => Seq[CinemaMovie]): Unit = {
    val t0 = System.currentTimeMillis()
    try {
      val movies  = fetch()
      val elapsed = System.currentTimeMillis() - t0
      cache.put(cinema, movies)
      logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms")
      firstLoad(cinema).trySuccess(movies)
    } catch {
      case e: Exception =>
        val elapsed = System.currentTimeMillis() - t0
        logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
        firstLoad(cinema).trySuccess(Seq.empty)
    }
  }
}
