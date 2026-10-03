package services.readmodel

import models.{Cinema, City, CityScreening, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import settings.{ReadModelColdRetryInterval, ReadModelReloadInterval}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import tools.DaemonExecutors

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
class WebReadModel(
    reader: ReadModelReader,
    // The backstop reload and the cold-start retry cadences (`KINOWO_READMODEL_RELOAD_SECONDS` /
    // `…_COLD_RETRY_SECONDS`, resolved by the web root); the compiled-in ones for specs.
    reloadInterval:    ReadModelReloadInterval    = WebReadModel.DefaultReloadInterval,
    coldRetryInterval: ReadModelColdRetryInterval = WebReadModel.DefaultColdRetryInterval,
    driftSettle:       WebReadModel.DriftSettle   = WebReadModel.DefaultDriftSettle) extends Stoppable with Logging {

  private val movies = new ConcurrentHashMap[String, ResolvedMovie]()
  // citySlug -> (screeningId -> CityScreening). The per-city bucket is the
  // per-request read key (`/:city/api/repertoire`), so it's pre-indexed rather
  // than scanned on every request.
  private val byCity = new ConcurrentHashMap[String, ConcurrentHashMap[String, CityScreening]]()

  // ATOMIC, NOT @volatile. Advancing a stamp is a read-modify-write, and the two
  // change streams deliver on different threads (the backstop scheduler and
  // /rehydrate touch the model too). @volatile would publish the write but not
  // make the update atomic, so an interleaving could LOSE one and move the stamp
  // backwards — after which a later advance can re-issue a value a client
  // already holds, which is a 304 for changed bytes. `advance` is a pure
  // function of its argument, so it is safe to re-apply on a CAS retry.
  private val _lastModified = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](java.time.Instant.now())
  /** Model-wide change stamp — moves when ANYTHING in the corpus changes. The
   *  sitemap's `<lastmod>`, the `filmSlugs` memo and `/debug/readmodel` all want
   *  exactly this. A conditional GET does not: see [[lastModifiedFor]]. */
  def lastModified: java.time.Instant = _lastModified.get()

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
  private val _globalFloor = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](_lastModified.get())

  /** The conditional-GET validator for one city: the latest of the model-wide
   *  floor, the city's own stamp, and the stamps of any slug it formerly used
   *  (`screeningsForCity` still serves rows filed under those, so they are part
   *  of what the city renders). */
  def lastModifiedFor(citySlug: String): java.time.Instant = {
    var latest = _globalFloor.get()
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

  private def touch(): Unit = { _lastModified.updateAndGet(previous => advance(previous)); () }

  /** Bump one city's validator (and the model-wide stamp with it). */
  private def touchCity(citySlug: String): Unit = {
    touch()
    cityStamps.compute(citySlug, (_, previous) =>
      if (previous == null) java.time.Instant.now() else advance(previous))
  }

  /** Bump the floor, and with it every city. */
  private def touchEveryCity(): Unit = {
    touch()
    _globalFloor.updateAndGet(previous => advance(previous))
    ()
  }

  /** The projection `FilmSlugs` is a pure function of. Two movie documents with
   *  equal keys assign identical addresses, so a change between them is
   *  city-scopable; a change to one is not. */
  private def slugKey(m: ResolvedMovie): (String, Option[Int]) = (m.title, m.releaseYear)

  /** True when the two documents are equal everywhere except `synopsisByCity` —
   *  the one field whose effect is confined to named cities. */
  private def differsOnlyInSynopsisByCity(previous: ResolvedMovie, current: ResolvedMovie): Boolean =
    previous.copy(synopsisByCity = current.synopsisByCity) == current

  /** The city slugs whose rendered synopsis moved: an entry added, removed or
   *  rewritten. A removal counts — that city falls back to `synopsis` and its
   *  bytes change with it. */
  private def citiesWithChangedSynopsis(previous: ResolvedMovie, current: ResolvedMovie): Seq[String] =
    (previous.synopsisByCity.keySet ++ current.synopsisByCity.keySet)
      .filter(slug => previous.synopsisByCity.get(slug) != current.synopsisByCity.get(slug))
      .toSeq

  private def citiesScreening(filmId: String): Seq[String] =
    Option(filmCities.get(filmId)).map(_.asScala.toSeq).getOrElse(Nil)

  private def indexFilmCity(filmId: String, citySlug: String): Unit =
    filmCities.computeIfAbsent(filmId, _ => ConcurrentHashMap.newKeySet[String]()).add(citySlug)

  // ── Read surface (controllers) ──────────────────────────────────────────────

  def movie(id: String): Option[ResolvedMovie] = Option(movies.get(id))
  def allMovies(): Seq[ResolvedMovie]           = movies.values.asScala.toSeq

  /** An index that is a pure function of the (id, title, releaseYear) corpus,
   *  rebuilt only when that corpus changes: it walks every movie, and a request
   *  would otherwise redo that work every time.
   *
   *  VERSIONED BY THE FILM CORPUS, NOT BY EVERY CHANGE. `_globalFloor` moves
   *  only when a film enters, leaves, or changes title/year — the only things
   *  that can re-address anything. Keyed on the model-wide stamp instead, a
   *  showtime edit anywhere discarded the map and the next render rebuilt every
   *  address in the corpus; screenings are the bulk of all events, so the memo
   *  almost never hit. */
  private final class CorpusIndex[A](build: Seq[ResolvedMovie] => A) {
    @volatile private var cached: (java.time.Instant, A) = null
    def get: A = {
      val stamp = _globalFloor.get()
      val hit   = cached
      if (hit != null && hit._1 == stamp) hit._2
      else {
        val fresh = build(allMovies())
        cached = (stamp, fresh)
        fresh
      }
    }
  }

  private val filmSlugsIndex  = new CorpusIndex(FilmSlugs(_))
  private val filmTitlesIndex = new CorpusIndex(FilmTitles(_))

  /** Film→URL addressing for the whole corpus at once ([[FilmSlugs]] explains
   *  why it can't be a per-title fold). */
  def filmSlugs: FilmSlugs = filmSlugsIndex.get

  /** Title→film for the legacy `?title=` address ([[FilmTitles]] explains why
   *  it is a fold, not a string match). */
  def filmTitles: FilmTitles = filmTitlesIndex.get
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
      // corpus-wide (see `_globalFloor`).
      if (!previous.exists(slugKey(_) == slugKey(m))) touchEveryCity()
      // A blurb that landed for ONE city is one city's change. `synopsisFor`
      // reads that city's `synopsisByCity` entry and falls back to the
      // city-independent `synopsis` only when it has none, so when the document
      // differs in NOTHING ELSE, exactly the cities whose entry moved render
      // different bytes -- including one whose entry was removed, which now
      // falls back. Everything else (a rating, a poster, the fallback synopsis)
      // reaches every city screening the film.
      else previous match {
        case Some(p) if differsOnlyInSynopsisByCity(p, m) => citiesWithChangedSynopsis(p, m).foreach(touchCity)
        case _                                            => citiesScreening(m._id).foreach(touchCity)
      }
    }
  }

  private def applyMovieDelete(id: String): Unit = {
    // Only a film we actually held frees a slug for a namesake in another city.
    // Deletes on this collection are mostly RE-KEYS, so one naming an id this
    // model never saw is ordinary traffic — and it re-addresses nothing.
    if (movies.remove(id) != null) touchEveryCity()
  }

  /** The row filed under the page its cinema is listed on NOW, rather than the
   *  slug it was projected under. Which page lists which venue is roster data
   *  (Poland's is re-clustered by `data/pl/scripts/build_pages.py`), and a row is
   *  only re-projected when its film is — up to a whole scrape cadence later. Re-
   *  addressed here, a venue that moved pages is served from its new page the
   *  moment this tier starts, and never again from the one it left. A venue the
   *  roster no longer knows keeps the slug it was projected under. */
  private def onCurrentPage(s: CityScreening): CityScreening = {
    val paged = Cinema.byDisplayName.get(s.cinema).flatMap(City.forCinema).map(_.slug) match {
      case Some(page) if page != s.city => s.copy(city = page)
      case _                            => s
    }
    // Every row enters through here (boot load, backstop reload, change stream), so this
    // is where its showtimes join the shared instants — see `LocalDateTimePool`.
    instants.showtimes(paged)
  }

  private val instants = new services.movies.LocalDateTimePool

  private def applyScreeningUpsert(projected: CityScreening): Unit = {
    val s        = onCurrentPage(projected)
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
   *  reload never sees an empty corpus (mirrors `MovieCache.rehydrate`). A read
   *  that comes back INCOMPLETE adds what it reached and evicts nothing: an
   *  incomplete keyset scan holds only the pages before the failure, and evicting
   *  against it would drop every row after them from a model serving them
   *  correctly. Returns the movie-document count.
   *
   *  ONE CORPUS ON THE HEAP, NOT TWO. The screenings are STREAMED a page at a time
   *  and written over the live rows, never buffered whole: web-us heap-OOMed on
   *  2026-10-02 34s into a drift reload, its dump holding 196k `CityScreening`s
   *  for a 101k corpus — the buffered read and its `groupBy` beside the live
   *  buckets, ~400 MB each, in a 1 GiB heap. What the stream keeps per row is its
   *  id, which the row already holds. */
  def reload(): Int = {
    val (ms, moviesComplete) = reader.findAllMoviesChecked()
    ms.foreach(m => movies.put(m._id, m))
    if (moviesComplete) {
      val liveMovieIds = ms.iterator.map(_._id).toSet
      movies.keySet().asScala.toSeq.filterNot(liveMovieIds).foreach(movies.remove)
    }

    val seenByCity      = new java.util.HashMap[String, java.util.HashSet[String]]()
    val nextFilmCities  = new java.util.HashMap[String, java.util.HashSet[String]]()
    val screeningsComplete = reader.foreachScreening { projected =>
      val s = onCurrentPage(projected)
      byCity.computeIfAbsent(s.city, _ => new ConcurrentHashMap[String, CityScreening]()).put(s._id, s)
      seenByCity.computeIfAbsent(s.city, _ => new java.util.HashSet[String]()).add(s._id)
      nextFilmCities.computeIfAbsent(s.filmId, _ => new java.util.HashSet[String]()).add(s.city)
    }
    if (screeningsComplete) {
      byCity.forEach { (city, bucket) =>
        val seen = seenByCity.get(city)
        if (seen == null) byCity.remove(city)
        else bucket.keySet().removeIf(id => !seen.contains(id))
      }
      // Rebuild the film->cities index exactly; this is the point at which the
      // incrementally-grown superset is made true again.
      filmCities.clear()
    }
    nextFilmCities.forEach((filmId, cities) => cities.forEach(city => indexFilmCity(filmId, city)))
    if (!moviesComplete || !screeningsComplete)
      logger.warn(s"WebReadModel reload: incomplete read (movies complete=$moviesComplete, " +
        s"screenings complete=$screeningsComplete) — added what was read, evicted nothing it could not see.")
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
    drift().foreach { first =>
      // A COUNT TAKEN MID-WRITE IS NOT DRIFT. The count is read straight off the server, the
      // model only once the write's change event has been applied, so a count landing between
      // the two sees the database a row or two ahead. That was every drift reload web-us ever
      // logged (3 in the 7 days to 2026-10-02: isolated, db ahead by 1-2 screenings, each in the
      // same minute as a worker prune burst, never at two consecutive ticks) -- a full corpus
      // decode bought for an event already on its way. A lost event is still missing after
      // the settle; an in-flight one is not.
      Thread.sleep(driftSettle.value.toMillis)
      drift() match {
        case Some(confirmed) =>
          logger.info(s"WebReadModel backstop: drift detected — reloading ($confirmed).")
          reload()
        case None =>
          logger.info(s"WebReadModel backstop: count mismatch settled within ${driftSettle.value} ($first) — no reload.")
      }
    }
  }

  /** The mismatch between the server-side counts and the model, described, or `None` when they
   *  agree. An unavailable count (negative) is a mismatch: it is no evidence the model is right. */
  private def drift(): Option[String] = {
    val dbMovies     = reader.countMovies()
    val dbScreenings = reader.countScreenings()
    val drifted =
      dbMovies     < 0 || dbMovies     != movies.size.toLong ||
      dbScreenings < 0 || dbScreenings != liveScreeningCount.toLong
    Option.when(drifted)(s"movies mem=${movies.size}/db=$dbMovies, screenings mem=$liveScreeningCount/db=$dbScreenings")
  }

  // ── Lifecycle ───────────────────────────────────────────────────────────────

  private val scheduler       = DaemonExecutors.scheduler("web-read-model")
  private val BackstopSeconds  = reloadInterval.value.toSeconds
  // Far tighter than the backstop because the state it recovers from is a blank site, not
  // drift. Cheap enough to run at this cadence precisely because it probes with a count.
  private val ColdRetrySeconds = coldRetryInterval.value.toSeconds
  @volatile private var movieWatch:     Option[StreamSubscription] = None
  @volatile private var screeningWatch: Option[StreamSubscription] = None

  def start(): Unit = {
    // The watches replay from BEFORE the hydrate. Opened "from now" after it, a write landing
    // between the two was in neither: on 2026-09-23 web-pl booted into two new Włodawa
    // screenings and served without them for the 30 minutes until the backstop saw the count
    // drift — and a missed write that leaves the counts equal, the backstop never sees. Replayed
    // events re-apply what the hydrate may already hold; every applier is idempotent.
    val checkpoint = reader.streamCheckpoint()
    reload()
    movieWatch     = reader.watchMovies(applyMovieUpsert, applyMovieDelete, checkpoint)
    screeningWatch = reader.watchScreenings(applyScreeningUpsert, applyScreeningDelete, checkpoint)
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

object WebReadModel {
  val DefaultReloadInterval: ReadModelReloadInterval       = ReadModelReloadInterval(30.minutes)
  val DefaultColdRetryInterval: ReadModelColdRetryInterval = ReadModelColdRetryInterval(30.seconds)

  /** How long the backstop waits before re-counting a mismatch (see `backstopTick`): long enough
   *  for a write's change event to reach the model, short enough to hold its one thread briefly. */
  final case class DriftSettle(value: FiniteDuration) extends AnyVal
  val DefaultDriftSettle: DriftSettle = DriftSettle(5.seconds)
}
