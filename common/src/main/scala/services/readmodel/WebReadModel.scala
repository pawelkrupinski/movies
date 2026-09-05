package services.readmodel

import models.{City, CityScreening, ResolvedMovie}
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
  /** Model-wide change stamp — moves when ANYTHING in the corpus changes. The
   *  sitemap's `<lastmod>`, the `filmSlugs` memo and `/debug/readmodel` all want
   *  exactly this. A conditional GET does not: see [[lastModifiedFor]]. */
  def lastModified: java.time.Instant = _lastModified

  // ── Per-city cache validators ───────────────────────────────────────────────
  //
  // A conditional GET for one city asks a narrower question than `lastModified`
  // answers: did the bytes THAT CITY renders change? Answering it with the
  // model-wide stamp meant a Warsaw showtime invalidated London's ETag, so every
  // city's payload looked like it changed every couple of minutes and no 304 --
  // browser, mobile app or Cloudflare -- survived long enough to be worth much.
  //
  // Stamps are only ever allowed to run FAST, never slow: an over-eager bump
  // costs one revalidation, a missed one serves stale showtimes behind a 304.
  private val cityStamps = new ConcurrentHashMap[String, java.time.Instant]()

  // filmId -> the cities screening it, so a movie document's change bumps only
  // those. Deliberately allowed to be a SUPERSET: a screening delete cannot tell
  // whether the city still shows the film at another venue, so entries are added
  // but never removed incrementally, and the index is rebuilt exactly on every
  // `reload()`. Drift therefore only ever over-invalidates.
  private val filmCities = new ConcurrentHashMap[String, java.util.Set[String]]()

  // The floor under every city's stamp: changes no per-city bump can scope.
  //
  // ⚠️ THE SLUG CORPUS IS WHY THIS EXISTS. `FilmSlugs` assigns `/{city}/movie/{slug}`
  // addresses over the WHOLE corpus -- a film appearing anywhere can take the bare
  // slug off a film playing in a different city and silently change that city's
  // rendered links. So any change to the `(id, title, releaseYear)` projection
  // `FilmSlugs` is a pure function of moves EVERY city, and nothing else does.
  @volatile private var _globalFloor: java.time.Instant = _lastModified

  /** The conditional-GET validator for one city: the latest of the model-wide
   *  floor, the city's own stamp, and the stamps of any slug it formerly used
   *  (`screeningsForCity` still serves rows filed under those, so they are part
   *  of what the city renders). */
  def lastModifiedFor(citySlug: String): java.time.Instant = {
    var latest = _globalFloor
    latest = laterOf(latest, cityStamps.get(citySlug))
    City.formerSlugs(citySlug).foreach(former => latest = laterOf(latest, cityStamps.get(former)))
    latest
  }

  private def laterOf(current: java.time.Instant, candidate: java.time.Instant): java.time.Instant =
    if (candidate != null && candidate.isAfter(current)) candidate else current

  /** Strictly monotonic. The stamp is a wall clock and a coarse one hands out the
   *  same `Instant` twice; a repeated validator is a 304 for changed bytes. A
   *  clock that steps backwards must not stall invalidation either. */
  private def advance(previous: java.time.Instant): java.time.Instant = {
    val now = java.time.Instant.now()
    if (now.isAfter(previous)) now else previous.plusNanos(1)
  }

  private def touch(): Unit = { _lastModified = advance(_lastModified) }

  /** Bump one city's validator (and the model-wide stamp with it). */
  private def touchCity(citySlug: String): Unit = {
    touch()
    cityStamps.compute(citySlug, (_, previous) =>
      if (previous == null) java.time.Instant.now() else advance(previous))
  }

  /** Bump the floor, and with it every city. */
  private def touchEveryCity(): Unit = {
    touch()
    _globalFloor = advance(_globalFloor)
  }

  /** The projection `FilmSlugs` is a pure function of. Two movie documents with
   *  equal keys assign identical addresses, so a change between them is
   *  city-scopable; a change to one is not. */
  private def slugKey(m: ResolvedMovie): (String, Option[Int]) = (m.title, m.releaseYear)

  private def citiesScreening(filmId: String): Seq[String] =
    Option(filmCities.get(filmId)).map(_.asScala.toSeq).getOrElse(Nil)

  private def indexFilmCity(filmId: String, citySlug: String): Unit =
    filmCities.computeIfAbsent(filmId, _ => ConcurrentHashMap.newKeySet[String]()).add(citySlug)

  // ── Read surface (controllers) ──────────────────────────────────────────────

  def movie(id: String): Option[ResolvedMovie] = Option(movies.get(id))
  def allMovies(): Seq[ResolvedMovie]           = movies.values.asScala.toSeq

  /** Film→URL addressing for the whole corpus at once ([[FilmSlugs]] explains
   *  why it can't be a per-title fold). Recomputed only when the corpus
   *  actually changes: it walks every movie, and a city listing would otherwise
   *  redo that work on every request. `lastModified` is the same stamp the
   *  change streams already bump, so a stale map can't outlive an upsert. */
  def filmSlugs: FilmSlugs = {
    val stamp = _lastModified
    val cached = _filmSlugs
    if (cached != null && cached._1 == stamp) cached._2
    else {
      val fresh = FilmSlugs(allMovies())
      _filmSlugs = (stamp, fresh)
      fresh
    }
  }

  @volatile private var _filmSlugs: (java.time.Instant, FilmSlugs) = null
  def screeningsForCity(citySlug: String): Seq[CityScreening] = {
    val current = bucket(citySlug)
    // A city that changed slug still has most of its rows projected under the
    // OLD one (see `City.formerSlugs`), and would otherwise serve almost nothing
    // until every one of its films had been projected again. Rows under the
    // current slug WIN — they are the freshly projected ones — and the former
    // bucket only fills the venues that have not caught up yet.
    //
    // Restricted to the city's OWN venues where the former slug was SPLIT rather
    // than renamed — `alaska` became nine metros and its rows hold every Alaskan
    // venue, so unfiltered Anchorage would serve Juneau's cinemas, 1,400 km and
    // no road away. `City.ownVenuesOfSplitCity` is absent for a plain rename,
    // whose rows are this city's already.
    val former = City.formerSlugs(citySlug).flatMap(bucket)
    if (former.isEmpty) current
    else {
      val projected = current.map(s => (s.filmId, s.cinema)).toSet
      val mine      = City.ownVenuesOfSplitCity.get(citySlug)
      current ++ former.filter(s =>
        !projected((s.filmId, s.cinema)) && mine.forall(_.contains(s.cinema)))
    }
  }

  private def bucket(citySlug: String): Seq[CityScreening] =
    Option(byCity.get(citySlug)).map(_.values.asScala.toSeq).getOrElse(Seq.empty)
  /** Every cached screening across all cities — the read cache's full
   *  `web_screenings` view, used by the dev `/debug/readmodel` dump. */
  def allScreenings(): Seq[CityScreening] =
    byCity.values.asScala.flatMap(_.values.asScala).toSeq

  // ── Change-stream appliers ──────────────────────────────────────────────────

  private def applyMovieUpsert(m: ResolvedMovie): Unit = {
    val previous = Option(movies.put(m._id, m))
    // A REWRITE THAT CHANGED NOTHING INVALIDATES NOTHING. The stream carries
    // document WRITES, not content changes: a re-key or a venue re-projection
    // rewrites rows wholesale (`replaceFilm` once rewrote all 298 rows for one
    // venue) and every one of them arrives here as an upsert. Bumping on the
    // write threw away the city's page, its gzipped body and every client's 304
    // for bytes identical to those already held. These are pure case classes
    // with no timestamp, so structural equality is exactly the question "would
    // any client see different bytes?".
    if (!previous.contains(m)) {
      // A film ENTERING the corpus, or changing title/year, reshuffles addresses
      // corpus-wide (see `_globalFloor`). Anything else -- a rating refresh, a new
      // poster, a synopsis -- only changes the bytes of the cities screening it.
      if (!previous.exists(slugKey(_) == slugKey(m))) touchEveryCity()
      else citiesScreening(m._id).foreach(touchCity)
    }
  }

  private def applyMovieDelete(id: String): Unit = {
    movies.remove(id)
    // A departing film frees its slug for a namesake in another city.
    touchEveryCity()
  }

  private def applyScreeningUpsert(s: CityScreening): Unit = {
    val bucket   = byCity.computeIfAbsent(s.city, _ => new ConcurrentHashMap[String, CityScreening]())
    val previous = bucket.put(s._id, s)
    indexFilmCity(s.filmId, s.city)
    // Only a row that actually differs changes what the city renders — see the
    // note on `applyMovieUpsert`. `previous` is null for a genuinely new row,
    // which is never equal to `s`, so a first insert still bumps.
    if (previous != s) touchCity(s.city)
  }
  private def applyScreeningDelete(id: String): Unit = {
    // The delete event carries only the id; it's globally unique, so drop it
    // from whichever city bucket holds it -- and bump only the cities that
    // actually held it.
    var found = false
    byCity.forEach { (city, bucket) =>
      if (bucket.remove(id) != null) { found = true; touchCity(city) }
    }
    // A delete for a row we never held still moves the model-wide stamp, as it
    // always did; no city's bytes changed, so no city stamp does.
    if (!found) touch()
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
    // Rebuild the film->cities index exactly; this is the point at which the
    // incrementally-grown superset is made true again.
    filmCities.clear()
    ss.foreach(s => indexFilmCity(s.filmId, s.city))
    // Every city is re-derived, so no per-city stamp survives as evidence of
    // anything; the floor alone answers for all of them.
    cityStamps.clear()
    touchEveryCity()
    ms.size
  }

  private def liveScreeningCount: Int = byCity.values.asScala.iterator.map(_.size).sum

  /** Cold-retry tick — the guard `reload`'s cannot be.
   *
   *  `reload` protects a WARM cache from a failed read, but at boot the cache is empty, so an
   *  unreachable Mongo hands `start()` an empty corpus indistinguishable from a corpus that
   *  really is empty (`pagedFindAll` returns `Seq.empty` on an incomplete keyset scan). The
   *  model then serves nothing until the next backstop — 1800s away. On 2026-07-29 a Mongo
   *  OOM-kill did exactly that: the web tier restarted into the outage window and every PL
   *  and UK city served zero films until an unrelated health-check restart happened to land
   *  on a recovered Mongo.
   *
   *  So while the model holds nothing, keep asking. The probe is the cheap server-side count,
   *  not a reload: serving nothing while `web_movies` holds films is unambiguous — either a
   *  read failed or a boot raced the database, and both want the same answer. A warm model
   *  costs one field read (drift is the backstop's job), and a genuinely empty corpus costs
   *  one count. A negative count means the count itself is unavailable, which is no evidence
   *  there is anything to load. */
  private[readmodel] def coldRetryTick(): Unit = {
    if (!movies.isEmpty) return
    val dbMovies = reader.countMovies()
    if (dbMovies > 0) {
      logger.warn(s"WebReadModel cold-retry: serving an empty corpus while web_movies holds " +
        s"$dbMovies movie(s) — the boot hydrate read failed; reloading.")
      reload()
    }
  }

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
  // Far tighter than the backstop because the state it recovers from is a blank site, not
  // drift. Cheap enough to run at this cadence precisely because it probes with a count.
  private val ColdRetrySeconds = Env.positiveLong("KINOWO_READMODEL_COLD_RETRY_SECONDS", 30L)
  @volatile private var movieWatch:     Option[StreamSubscription] = None
  @volatile private var screeningWatch: Option[StreamSubscription] = None

  def start(): Unit = {
    reload()
    movieWatch     = reader.watchMovies(applyMovieUpsert, applyMovieDelete)
    screeningWatch = reader.watchScreenings(applyScreeningUpsert, applyScreeningDelete)
    scheduler.scheduleAtFixedRate(
      () => Try(backstopTick()).recover { case exception => logger.warn(s"WebReadModel backstop tick failed: ${exception.getMessage}") },
      BackstopSeconds, BackstopSeconds, TimeUnit.SECONDS)
    scheduler.scheduleAtFixedRate(
      () => Try(coldRetryTick()).recover { case exception => logger.warn(s"WebReadModel cold-retry tick failed: ${exception.getMessage}") },
      ColdRetrySeconds, ColdRetrySeconds, TimeUnit.SECONDS)
    logger.info(s"WebReadModel started; backstop reload every ${BackstopSeconds}s; " +
      s"cold retry every ${ColdRetrySeconds}s; " +
      s"change-stream watches ${if (movieWatch.isDefined) "active" else "unavailable — backstop only"}.")
  }

  def stop(): Unit = {
    movieWatch.foreach(h => Try(h.close()))
    screeningWatch.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
