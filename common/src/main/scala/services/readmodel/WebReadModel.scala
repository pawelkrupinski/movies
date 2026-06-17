package services.readmodel

import models.{CityScreening, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import tools.{DaemonExecutors, Env}

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The serving app's warm view of the denormalised read model. Holds the two
 * derived collections in memory — resolved movies by id, and screenings indexed
 * by city — and keeps them current from the `web_movies` / `web_screenings`
 * change streams (inserts/updates AND deletes, both of which those streams
 * deliver), with a periodic drift-checked reload as the backstop (a full reload
 * only when a stream has died or a server-side count drifts — see `backstopTick`).
 * The web never touches the `movies` collection or a MovieRecord.
 *
 * `lastModified` bumps on every applied change, so it's a *tight* cache-version
 * signal: it advances only when a resolved movie or a screening actually
 * changes — a no-op scrape tick on the worker moves nothing here.
 *
 * The join is ordering-tolerant: a screening whose movie document hasn't landed yet
 * is simply skipped by `MovieControllerService` until it does, so the
 * movie-before-screenings write order is preferred but not required.
 */
class WebReadModel(reader: ReadModelReader) extends Stoppable with Logging {

  private val movies = new ConcurrentHashMap[String, ResolvedMovie]()
  // citySlug -> (screeningId -> CityScreening). The per-city bucket is the
  // per-request read key (`/:city/api/repertoire`), so it's pre-indexed rather
  // than scanned on every request.
  private val byCity = new ConcurrentHashMap[String, ConcurrentHashMap[String, CityScreening]]()

  @volatile private var _lastModified: java.time.Instant = java.time.Instant.now()
  def lastModified: java.time.Instant = _lastModified
  private def touch(): Unit = { _lastModified = java.time.Instant.now() }

  // ── Read surface (controllers) ──────────────────────────────────────────────

  def movie(id: String): Option[ResolvedMovie] = Option(movies.get(id))
  def allMovies(): Seq[ResolvedMovie]           = movies.values.asScala.toSeq
  def screeningsForCity(citySlug: String): Seq[CityScreening] =
    Option(byCity.get(citySlug)).map(_.values.asScala.toSeq).getOrElse(Seq.empty)
  /** Every cached screening across all cities — the read cache's full
   *  `web_screenings` view, used by the dev `/debug/readmodel` dump. */
  def allScreenings(): Seq[CityScreening] =
    byCity.values.asScala.flatMap(_.values.asScala).toSeq

  // ── Change-stream appliers ──────────────────────────────────────────────────

  private def applyMovieUpsert(m: ResolvedMovie): Unit = { movies.put(m._id, m); touch() }
  private def applyMovieDelete(id: String): Unit       = { movies.remove(id); touch() }

  private def applyScreeningUpsert(s: CityScreening): Unit = {
    byCity.computeIfAbsent(s.city, _ => new ConcurrentHashMap[String, CityScreening]()).put(s._id, s)
    touch()
  }
  private def applyScreeningDelete(id: String): Unit = {
    // The delete event carries only the id; it's globally unique, so drop it
    // from whichever city bucket holds it.
    byCity.values.asScala.foreach(_.remove(id))
    touch()
  }

  /** Full reload from the derived collections — boot hydrate, periodic backstop,
   *  and the `/rehydrate` endpoint. Additive-then-evict so a page render mid-
   *  reload never sees an empty corpus (mirrors `MovieCache.rehydrate`); a
   *  transient empty result on a warm cache is treated as a Mongo hiccup and
   *  skipped. Returns the movie-document count. */
  def reload(): Int = {
    val ms = reader.findAllMovies()
    val ss = reader.findAllScreenings()
    if (ms.isEmpty && ss.isEmpty && !movies.isEmpty) {
      logger.warn("WebReadModel reload: read model returned empty while the cache is warm — " +
        "treating as a transient Mongo failure; cache left intact.")
      return movies.size
    }
    // Movies: additive put + evict the ids that disappeared.
    ms.foreach(m => movies.put(m._id, m))
    val liveMovieIds = ms.iterator.map(_._id).toSet
    movies.keySet().asScala.toSeq.filterNot(liveMovieIds).foreach(movies.remove)
    // Screenings: rebuild each city bucket additively, evict missing, drop empty
    // buckets.
    val nextByCity = ss.groupBy(_.city)
    nextByCity.foreach { case (city, items) =>
      val bucket  = byCity.computeIfAbsent(city, _ => new ConcurrentHashMap[String, CityScreening]())
      items.foreach(s => bucket.put(s._id, s))
      val liveIds = items.iterator.map(_._id).toSet
      bucket.keySet().asScala.toSeq.filterNot(liveIds).foreach(bucket.remove)
    }
    byCity.keySet().asScala.toSeq.filterNot(nextByCity.keySet).foreach(byCity.remove)
    touch()
    ms.size
  }

  private def liveScreeningCount: Int = byCity.values.asScala.iterator.map(_.size).sum

  /** Periodic backstop tick. While both change streams are live they keep the
   *  model current, so re-reading and re-decoding the whole corpus every tick is
   *  wasted CPU on the single-vCPU serving box — and that decode burst is what
   *  stalls a request that happens to land during it. So skip the reload when the
   *  streams are live *and* the cheap server-side counts still match what we hold;
   *  pay the O(corpus) reload only when a stream has died (full catch-up, the
   *  original backstop behaviour) or a count has drifted (a delivered event we
   *  failed to apply, or one missed by a silently-stalled stream). */
  private[readmodel] def backstopTick(): Unit = {
    val streamsLive = movieWatch.exists(_.live) && screeningWatch.exists(_.live)
    if (!streamsLive) { reload(); return }
    val dbMovies     = reader.countMovies()
    val dbScreenings = reader.countScreenings()
    val drifted =
      dbMovies     < 0 || dbMovies     != movies.size.toLong ||
      dbScreenings < 0 || dbScreenings != liveScreeningCount.toLong
    if (drifted) {
      logger.info(s"WebReadModel backstop: drift detected — reloading " +
        s"(movies mem=${movies.size}/db=$dbMovies, screenings mem=$liveScreeningCount/db=$dbScreenings).")
      reload()
    }
  }

  // ── Lifecycle ───────────────────────────────────────────────────────────────

  private val scheduler       = DaemonExecutors.scheduler("web-read-model")
  private val BackstopSeconds  = Env.positiveLong("KINOWO_READMODEL_RELOAD_SECONDS", 1800L)
  @volatile private var movieWatch:     Option[StreamSubscription] = None
  @volatile private var screeningWatch: Option[StreamSubscription] = None

  def start(): Unit = {
    reload()
    movieWatch     = reader.watchMovies(applyMovieUpsert, applyMovieDelete)
    screeningWatch = reader.watchScreenings(applyScreeningUpsert, applyScreeningDelete)
    scheduler.scheduleAtFixedRate(
      () => Try(backstopTick()).recover { case exception => logger.warn(s"WebReadModel backstop tick failed: ${exception.getMessage}") },
      BackstopSeconds, BackstopSeconds, TimeUnit.SECONDS)
    logger.info(s"WebReadModel started; backstop reload every ${BackstopSeconds}s; " +
      s"change-stream watches ${if (movieWatch.isDefined) "active" else "unavailable — backstop only"}.")
  }

  def stop(): Unit = {
    movieWatch.foreach(h => Try(h.close()))
    screeningWatch.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
