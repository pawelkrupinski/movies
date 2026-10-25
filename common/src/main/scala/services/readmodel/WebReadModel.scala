package services.readmodel

import models.{Cinema, City, CityScreening, Country, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import settings.{ReadModelColdRetryInterval, ReadModelReloadInterval}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import tools.{DaemonExecutors, ScheduledTick, TickRecorder, TickRun}

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
    driftSettle:       WebReadModel.DriftSettle   = WebReadModel.DefaultDriftSettle,
    // Each reopen of a change stream that ended (`kinowo_web_readmodel_stream_reopens_total`).
    streamMetrics:     ReadModelStreamMetrics     = ReadModelStreamMetrics.noop,
    // What the change stamps are read from; the system clock outside specs.
    clock:             java.time.Clock,
    // The country this deployment serves. Every country owns its own city slugs, so a
    // slug means a city only within one: `aberdeen` is the UK's Aberdeen and South
    // Dakota's. The model serves a row under a slug only when its venue is this country's
    // (or retired, so on no country's page), and reads the slugs THIS country's cities
    // used to answer at (`Country.formerSlugs`) alongside their own. `None` — specs over
    // one country's fixture rows — takes every row and no former slugs.
    servingCountry:    Option[Country] = None,
    // Where each completed backstop and cold-retry tick is reported (`ScheduledTickStale`).
    ticks:             TickRecorder = TickRecorder.Unrecorded) extends Stoppable with Logging {

  private val formerSlugs: String => Seq[String] = servingCountry.fold((_: String) => Seq.empty[String])(c => c.formerSlugs)

  /** Is `cinema` a venue some OTHER country lists? Such a row is never this
   *  deployment's to serve, whatever slug it was filed under. */
  private def foreign(cinema: String): Boolean =
    servingCountry.exists(country => Cinema.byDisplayName.get(cinema).flatMap(City.forCinema).exists(_.country != country))

  private val movies = new ConcurrentHashMap[String, ResolvedMovie]()
  // citySlug -> (screeningId -> CityScreening). The per-city bucket is the
  // per-request read key (`/:city/api/repertoire`), so it's pre-indexed rather
  // than scanned on every request.
  private val byCity = new ConcurrentHashMap[String, ConcurrentHashMap[String, CityScreening]]()

  // The change stamps: model-wide, and each city's conditional-GET validator.
  private val stamps = new ReadModelStamps(clock, formerSlugs)

  /** Model-wide change stamp — see [[ReadModelStamps.lastModified]]. */
  def lastModified: java.time.Instant = stamps.lastModified

  /** The conditional-GET validator for one city — see [[ReadModelStamps.lastModifiedFor]]. */
  def lastModifiedFor(citySlug: String): java.time.Instant = stamps.lastModifiedFor(citySlug)

  private val filmCities = new FilmCities

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

  private def citiesScreening(filmId: String): Seq[String] = filmCities.of(filmId)

  // ── Read surface (controllers) ──────────────────────────────────────────────

  def movie(id: String): Option[ResolvedMovie] = Option(movies.get(id))
  def allMovies(): Seq[ResolvedMovie]           = movies.values.asScala.toSeq

  /** An index that is a pure function of the (id, title, releaseYear) corpus,
   *  rebuilt only when that corpus changes: it walks every movie, and a request
   *  would otherwise redo that work every time.
   *
   *  VERSIONED BY THE FILM CORPUS, NOT BY EVERY CHANGE. The stamps' floor moves
   *  only when a film enters, leaves, or changes title/year — the only things
   *  that can re-address anything. Keyed on the model-wide stamp instead, a
   *  showtime edit anywhere discarded the map and the next render rebuilt every
   *  address in the corpus; screenings are the bulk of all events, so the memo
   *  almost never hit. */
  private final class CorpusIndex[A](build: Seq[ResolvedMovie] => A) {
    @volatile private var cached: (java.time.Instant, A) = null
    def get: A = {
      val stamp = stamps.floor
      val hit   = cached
      if (hit != null && hit._1 == stamp) hit._2
      else {
        val fresh = build(allMovies())
        cached = (stamp, fresh)
        fresh
      }
    }
  }

  // The clean-title strip is the serving country's; a model with no country (specs) uses the default's.
  private val filmSlugsIndex  = new CorpusIndex(FilmSlugs.forCountry(servingCountry.getOrElse(Country.default)))
  private val filmTitlesIndex = new CorpusIndex(FilmTitles(_))

  /** Film→URL addressing for the whole corpus at once ([[FilmSlugs]] explains
   *  why it can't be a per-title fold). */
  def filmSlugs: FilmSlugs = filmSlugsIndex.get

  /** Title→film for the legacy `?title=` address ([[FilmTitles]] explains why
   *  it is a fold, not a string match). */
  def filmTitles: FilmTitles = filmTitlesIndex.get
  def screeningsForCity(citySlug: String): Seq[CityScreening] = cityRows(citySlug, _ => true)

  /** [[screeningsForCity]] narrowed to the films `filmIds` names: the same rows, found
   *  without first copying the city's whole bucket. A film page asks this of every city
   *  of its country (`citiesShowing`), and copying each city's rows to find one film's
   *  was most of what the page allocated. Exact, because the former-slug fill below
   *  only ever compares rows of the same film. */
  def screeningsOfFilms(citySlug: String, filmIds: Set[String]): Seq[CityScreening] =
    cityRows(citySlug, row => filmIds(row.filmId))

  private def cityRows(citySlug: String, keep: CityScreening => Boolean): Seq[CityScreening] = {
    val current = bucket(citySlug, keep)
    // A city that changed slug still has most of its rows projected under the
    // OLD one (see `Country.formerSlugs`), and would otherwise serve almost nothing
    // until every one of its films had been projected again. Rows under the
    // current slug WIN — they are the freshly projected ones — and the former
    // bucket only fills the venues that have not caught up yet. (A row of a venue
    // the roster knows is filed under that venue's page already — `onCurrentPage`
    // — so what a former bucket holds is the rows of venues it does not.)
    val former = formerSlugs(citySlug).flatMap(bucket(_, keep))
    if (former.isEmpty) current
    else {
      val projected = current.map(s => (s.filmId, s.cinema)).toSet
      current ++ former.filter(s => !projected((s.filmId, s.cinema)))
    }
  }

  private def bucket(citySlug: String, keep: CityScreening => Boolean): Seq[CityScreening] =
    Option(byCity.get(citySlug)).map(_.values.asScala.iterator.filter(s => keep(s) && !foreign(s.cinema)).toSeq)
      .getOrElse(Seq.empty)
  /** Every cached screening across all cities — the read cache's full
   *  `web_screenings` view, used by the dev `/debug/readmodel` dump. */
  def allScreenings(): Seq[CityScreening] =
    byCity.values.asScala.flatMap(_.values.asScala).toSeq

  // ── Change-stream appliers ──────────────────────────────────────────────────

  private def applyMovieUpsert(m: ResolvedMovie): Unit = {
    streamed(m._id)
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
      // corpus-wide (see [[ReadModelStamps]]'s floor).
      if (!previous.exists(slugKey(_) == slugKey(m))) stamps.touchEveryCity()
      // A blurb that landed for ONE city is one city's change. `synopsisFor`
      // reads that city's `synopsisByCity` entry and falls back to the
      // city-independent `synopsis` only when it has none, so when the document
      // differs in NOTHING ELSE, exactly the cities whose entry moved render
      // different bytes -- including one whose entry was removed, which now
      // falls back. Everything else (a rating, a poster, the fallback synopsis)
      // reaches every city screening the film.
      else previous match {
        case Some(p) if differsOnlyInSynopsisByCity(p, m) => citiesWithChangedSynopsis(p, m).foreach(stamps.touchCity)
        case _                                            => citiesScreening(m._id).foreach(stamps.touchCity)
      }
    }
  }

  private def applyMovieDelete(id: String): Unit = {
    // Only a film we actually held frees a slug for a namesake in another city.
    // Deletes on this collection are mostly RE-KEYS, so one naming an id this
    // model never saw is ordinary traffic — and it re-addresses nothing.
    streamed(id)
    if (movies.remove(id) != null) stamps.touchEveryCity()
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
    // is where its showtimes join the shared instants and URL prefixes — see `ShowtimePool`.
    shared.share(paged)
  }

  private val shared = new services.movies.ShowtimePool

  private def applyScreeningUpsert(projected: CityScreening): Unit = {
    val s        = onCurrentPage(projected)
    streamed(s._id)
    streamedDuringReload.get.foreach(_.placements.add((s.filmId, s.city)))
    val bucket   = byCity.computeIfAbsent(s.city, _ => new ConcurrentHashMap[String, CityScreening]())
    val previous = bucket.put(s._id, s)
    filmCities.add(s.filmId, s.city)
    // Only a row that actually differs changes what the city renders — see the
    // note on `applyMovieUpsert`. `previous` is null for a genuinely new row,
    // which is never equal to `s`, so a first insert still bumps.
    if (previous != s) stamps.touchCity(s.city)
  }
  private def applyScreeningDelete(id: String): Unit = {
    // The delete event carries only the id; it's globally unique, so drop it
    // from whichever city bucket holds it -- and bump only the cities that
    // actually held it.
    streamed(id)
    var found = false
    byCity.forEach { (city, bucket) =>
      if (bucket.remove(id) != null) { found = true; stamps.touchCity(city) }
    }
    // A delete for a row we never held still moves the model-wide stamp, as it
    // always did; no city's bytes changed, so no city stamp does.
    if (!found) stamps.touch()
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
    val during = new WebReadModel.Streamed
    streamedDuringReload.set(Some(during))
    try reloadAround(during) finally streamedDuringReload.set(None)
  }

  // What the change streams applied while a reload runs: newer than the reload's read, so the reload
  // neither overwrites nor evicts it. Each applier records it BEFORE it writes, and the reload
  // decides on each atomically with its own write of it.
  private val streamedDuringReload = new java.util.concurrent.atomic.AtomicReference[Option[WebReadModel.Streamed]](None)
  private def streamed(id: String): Unit = streamedDuringReload.get.foreach(_.ids.add(id))

  private def reloadAround(during: WebReadModel.Streamed): Int = {
    val (moviesRead, moviesComplete) = reloadMovies(during)
    val screeningsComplete           = reloadScreenings(during)
    lastReloadComplete = moviesComplete && screeningsComplete
    if (lastReloadComplete) _hydrated = true
    else
      logger.warn(s"WebReadModel reload: incomplete read (movies complete=$moviesComplete, " +
        s"screenings complete=$screeningsComplete) — added what was read, evicted nothing it could not see.")
    stamps.coverEveryCity()
    moviesRead
  }

  /** Write every movie document read over the live ones — but none the streams applied meanwhile —
   *  and, on a complete read, evict the ones it no longer holds: how many it read, and whether
   *  that read was complete. */
  private def reloadMovies(during: WebReadModel.Streamed): (Int, Boolean) = {
    val applied    = during.ids
    val moviesRead = reader.findAllMoviesChecked().answered
    val ms         = moviesRead.getOrElse(Seq.empty)
    ms.foreach(m => movies.compute(m._id, (id, held) => if (applied.contains(id)) held else m))
    moviesRead.foreach { _ =>
      val liveMovieIds = ms.iterator.map(_._id).toSet
      movies.keySet().removeIf(id => !liveMovieIds(id) && !applied.contains(id))
    }
    (ms.size, moviesRead.isDefined)
  }

  /** Stream every served screening over the live rows — but none the streams applied meanwhile —
   *  and, on a complete read, evict the ones it did not see: whether that read was complete. */
  private def reloadScreenings(during: WebReadModel.Streamed): Boolean = {
    val applied        = during.ids
    val seenByCity     = new java.util.HashMap[String, java.util.HashSet[String]]()
    val nextFilmCities = new java.util.HashMap[String, java.util.Set[String]]()
    val complete = reader.foreachServedScreening { projected =>
      val s = onCurrentPage(projected)
      byCity.computeIfAbsent(s.city, _ => new ConcurrentHashMap[String, CityScreening]())
        .compute(s._id, (id, held) => if (applied.contains(id)) held else s)
      seenByCity.computeIfAbsent(s.city, _ => new java.util.HashSet[String]()).add(s._id)
      nextFilmCities.computeIfAbsent(s.filmId, _ => new java.util.HashSet[String]()).add(s.city)
    }.isComplete
    if (complete) {
      // A bucket left empty stays: removing it would race a stream apply already holding it.
      byCity.forEach { (city, bucket) =>
        val seen = Option(seenByCity.get(city)).getOrElse(java.util.Collections.emptySet[String]())
        bucket.keySet().removeIf(id => !seen.contains(id) && !applied.contains(id))
      }
      // The incrementally-grown superset made exact again — with the rows the streams applied
      // meanwhile, which the scan may not have read.
      filmCities.rebuild(nextFilmCities, (filmId, city) => during.placements.contains((filmId, city)))
    } else filmCities.addAll(nextFilmCities)
    complete
  }

  // Whether the latest reload read both collections whole. Read by the tick thread right after its
  // own reload.
  @volatile private var lastReloadComplete = false

  // Set once a reload has read BOTH collections whole, and never cleared: from then on the model
  // serves a complete corpus, and a later failed read only leaves it a little stale.
  @volatile private var _hydrated = false

  /** Whether a read of both derived collections has ever completed — the web pod's readiness. Until
   *  it has, the model serves a corpus with holes (every city empty, or every film without
   *  showtimes), and a rolling deploy must not swap a warm pod for it. A genuinely empty corpus
   *  read whole counts: nothing is missing from it. */
  def hydrated: Boolean = _hydrated

  private def liveScreeningCount: Int = byCity.values.asScala.iterator.map(_.size).sum

  /** Cold-retry tick — the guard `reload`'s cannot be, and the keeper of the change streams.
   *
   *  `reload` protects a WARM cache from a failed read, but a boot read that fails leaves the
   *  model with holes and nothing to protect: no films at all, or (the films read, the
   *  screenings not) every film without a showtime. The model then served that until the next
   *  backstop — 1800s away. On 2026-07-29 a Mongo OOM-kill did exactly that: the web tier
   *  restarted into the outage window and every PL and UK city served zero films until an
   *  unrelated health-check restart happened to land on a recovered Mongo.
   *
   *  So until a read of both collections has completed ([[hydrated]]), keep reading. Once it
   *  has, and while both streams are live, this costs two field reads: drift is the backstop's job.
   *
   *  A stream that ENDED is reopened here ([[reopenDeadStreams]]): an outage outlasting the
   *  driver's one resume ends it for good, and nothing else opens it again.
   *
   *  [[TickRun.Incomplete]] when the read it made could not complete, or the reopen's checkpoint
   *  could not be taken; a warm model with live streams, or a reopen held back by its backoff, is a
   *  completed no-op. */
  private[readmodel] def coldRetryTick(): TickRun =
    if (!streamsLive) reopenDeadStreams()
    else {
      reopenAttempts = 0
      ticksUntilReopen = 0
      if (!_hydrated) coldReload()
      else if (catchUpOwed) {
        // The reopen's read failed (or it had no checkpoint to replay from — a standalone, `Absent`):
        // what was written while the streams were down reached neither. Read again — anything
        // written from the checkpoint on, the live streams carry.
        logger.warn("WebReadModel: change streams are live again but the writes made while they were down are unread — reloading.")
        val run = reloaded()
        if (lastReloadComplete) catchUpOwed = false
        run
      } else TickRun.Completed
    }

  private def coldReload(): TickRun = {
    logger.warn(s"WebReadModel cold-retry: no complete read yet (serving ${movies.size} movie(s), " +
      s"$liveScreeningCount screening(s)) — the boot hydrate read failed; reloading.")
    reloaded()
  }

  // The reopen backoff, in cold-retry ticks: a stream reopened into an outage that is still on
  // dies again, and each reopen pays a full reload — so the waits double (1, 3, 7, … ticks) up
  // to the backstop's interval, and reset once a tick finds both streams live. A checkpoint that
  // could not be taken is no attempt: it reloads and opens nothing, so the next tick tries again.
  // Only the tick's own thread touches them.
  private var reopenAttempts   = 0
  private var ticksUntilReopen = 0
  // A reopen that could not read the corpus whole, or opened its streams with no checkpoint to
  // replay from (a standalone answers `Absent`), left the writes made while the streams were down
  // unread. A FAILED checkpoint sets nothing: it opens no stream, and the next tick tries again.
  private var catchUpOwed      = false
  // A checkpoint fails every cold-retry tick for as long as Mongo is down — 120 lines an hour a
  // pod if each logged. The first is news; after it, one line per backstop interval, counting
  // the ticks it held back. Per OUTAGE: a checkpoint taken starts a fresh one, so the next
  // outage's first failure logs at once and counts only its own ticks. On the injected clock, so
  // a spec steps it. Only the tick's own thread touches it.
  private def freshCheckpointFailureLog() =
    new tools.LogThrottle(reloadInterval.value.toNanos, new tools.Stopwatch(() => clock.millis() * 1000000L))
  private var checkpointFailureLog = freshCheckpointFailureLog()
  private val MaxTicksBetweenReopens =
    math.max(1L, reloadInterval.value.toSeconds / math.max(1L, coldRetryInterval.value.toSeconds))

  /** Reopen each stream that is not live — the way `start` opens them: checkpoint, read the
   *  corpus whole, then watch from the checkpoint, so what was written while the stream was down
   *  is in the read or the replay. On the backoff above; a cold model reloads every tick anyway. */
  private def reopenDeadStreams(): TickRun =
    if (ticksUntilReopen > 0) {
      ticksUntilReopen -= 1
      if (!_hydrated) coldReload() else TickRun.Completed
    } else
      reader.streamCheckpoint() match {
        // No checkpoint is not "watch from now": the cursor opens asynchronously, once Mongo answers
        // again — after this reopen, after the catch-up read a later tick makes — and a write landing
        // between that read and the cursor reached neither (CI 2026-10-13). Nor is it a reopen
        // attempt: it cost a `hello`, not a reload, so it leaves the backoff alone and the next
        // tick takes one — the streams are back the tick after Mongo is.
        case tools.ReadOutcome.Failed(cause) =>
          checkpointFailureLog.admit().foreach { held =>
            logger.warn(s"WebReadModel: no stream checkpoint to replay from (${cause.explain}) — the streams stay down; " +
              s"retrying every tick${if (held > 0) s" ($held more failed since the last line)" else ""}.")
          }
          if (!_hydrated) { coldReload(); () }
          TickRun.Incomplete
        case outcome =>
          checkpointFailureLog = freshCheckpointFailureLog()
          reopenAttempts += 1
          ticksUntilReopen = math.min((1L << math.min(reopenAttempts, 30)) - 1, MaxTicksBetweenReopens).toInt
          logger.warn(s"WebReadModel: change stream(s) ended (movies live=${movieWatch.exists(_.live)}, " +
            s"screenings live=${screeningWatch.exists(_.live)}) — reopening, attempt $reopenAttempts.")
          val checkpoint = outcome.answered
          val run        = reloaded()
          // Caught up only by a whole read AND a replay from before it; short of either, the next tick
          // that finds the streams live reads again (`catchUpOwed`).
          catchUpOwed = !(lastReloadComplete && checkpoint.isDefined)
          if (!movieWatch.exists(_.live))
            movieWatch = reopened(movieWatch, MongoReadModelRepository.MoviesCollection)(
              reader.watchMovies(applyMovieUpsert, applyMovieDelete, checkpoint))
          if (!screeningWatch.exists(_.live))
            screeningWatch = reopened(screeningWatch, MongoReadModelRepository.ScreeningsCollection)(
              reader.watchScreenings(applyScreeningUpsert, applyScreeningDelete, checkpoint))
          run
      }

  /** A dead `watch` on `collection` closed and opened again, counted as a reopen. */
  private def reopened(watch: Option[StreamSubscription], collection: String)(open: => Option[StreamSubscription]): Option[StreamSubscription] = {
    streamMetrics.reopened(collection)
    watch.foreach(dead => Try(dead.close()))
    open
  }

  private[readmodel] def streamsLive: Boolean = movieWatch.exists(_.live) && screeningWatch.exists(_.live)

  /** Whether `collection`'s change stream (`web_movies` / `web_screenings`) is delivering now — the
   *  `kinowo_web_readmodel_stream_live` gauge. Down, the model learns that collection's writes only
   *  from the reopen's catch-up read or the backstop. */
  def streamLive(collection: String): Boolean = collection match {
    case MongoReadModelRepository.MoviesCollection     => movieWatch.exists(_.live)
    case MongoReadModelRepository.ScreeningsCollection => screeningWatch.exists(_.live)
    case _                                             => false
  }

  /** Periodic backstop tick. While both change streams are live they keep the
   *  model current, so re-reading and re-decoding the whole corpus every tick is
   *  wasted CPU on the single-vCPU serving box — and that decode burst is what
   *  stalls a request that happens to land during it. So skip the reload when the
   *  streams are live *and* the cheap server-side counts still match what we hold;
   *  pay the O(corpus) reload only when a stream has died (full catch-up, the
   *  original backstop behaviour) or a count has drifted (a delivered event we
   *  failed to apply, or one missed by a silently-stalled stream). */
  private[readmodel] def backstopTick(): TickRun =
    if (!streamsLive) reloaded()
    else drift().fold(TickRun.Completed) { first =>
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
          reloaded()
        case None =>
          logger.info(s"WebReadModel backstop: count mismatch settled within ${driftSettle.value} ($first) — no reload.")
          TickRun.Completed
      }
    }

  /** [[reload]], as a tick's run: [[TickRun.Incomplete]] when the read could not complete (logged by the reload). */
  private def reloaded(): TickRun = { reload(); TickRun.completedWhen(lastReloadComplete) }

  /** The mismatch between the server-side counts and the model, described, or `None` when they
   *  agree. A count that could not be taken is a mismatch: it is no evidence the model is right. */
  private def drift(): Option[String] = {
    val dbMovies     = reader.countMovies().answered
    val dbScreenings = reader.countScreenings().answered
    val drifted      = !dbMovies.contains(movies.size.toLong) || !dbScreenings.contains(liveScreeningCount.toLong)
    def shown(count: Option[Long]) = count.fold("unavailable")(_.toString)
    Option.when(drifted)(s"movies mem=${movies.size}/db=${shown(dbMovies)}, screenings mem=$liveScreeningCount/db=${shown(dbScreenings)}")
  }

  // ── Lifecycle ───────────────────────────────────────────────────────────────

  private val scheduler       = DaemonExecutors.scheduler("web-read-model")
  private val BackstopSeconds  = reloadInterval.value.toSeconds
  // Far tighter than the backstop because the state it recovers from is a blank site, not
  // drift. Once hydrated a tick is one field read; until then each is a full reload, which is
  // the point — a pod that has not read the corpus whole is not ready (`hydrated`).
  private val ColdRetrySeconds = coldRetryInterval.value.toSeconds
  @volatile private var movieWatch:     Option[StreamSubscription] = None
  @volatile private var screeningWatch: Option[StreamSubscription] = None

  def start(): Unit = {
    // The watches replay from BEFORE the hydrate. Opened "from now" after it, a write landing
    // between the two was in neither: on 2026-09-23 web-pl booted into two new Włodawa
    // screenings and served without them for the 30 minutes until the backstop saw the count
    // drift — and a missed write that leaves the counts equal, the backstop never sees. Replayed
    // events re-apply what the hydrate may already hold; every applier is idempotent.
    // A checkpoint that could not be TAKEN opens nothing: the cold-retry tick's reopen does, from one.
    val checkpoint = reader.streamCheckpoint()
    reload()
    checkpoint match {
      case tools.ReadOutcome.Failed(cause) =>
        logger.warn(s"WebReadModel: no stream checkpoint to replay from (${cause.explain}) — the watches open on the cold-retry cadence.")
      case outcome =>
        movieWatch     = reader.watchMovies(applyMovieUpsert, applyMovieDelete, outcome.answered)
        screeningWatch = reader.watchScreenings(applyScreeningUpsert, applyScreeningDelete, outcome.answered)
    }
    ticks.started("read-model-backstop", reloadInterval.value, reloadInterval.value)
    ticks.started("read-model-cold-retry", coldRetryInterval.value, coldRetryInterval.value)
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("read-model-backstop", reloadInterval.value, logger, ticks)(backstopTick()),
      BackstopSeconds, BackstopSeconds, TimeUnit.SECONDS)
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("read-model-cold-retry", coldRetryInterval.value, logger, ticks)(coldRetryTick()),
      ColdRetrySeconds, ColdRetrySeconds, TimeUnit.SECONDS)
    logger.info(s"WebReadModel started; backstop reload every ${BackstopSeconds}s; " +
      s"cold retry every ${ColdRetrySeconds}s; " +
      s"change-stream watches ${if (movieWatch.isDefined) "active" else "unavailable — reopened on the cold-retry cadence"}.")
  }

  def stop(): Unit = {
    // The scheduler first: a tick in flight may be reopening a stream, which a close before it ends would miss.
    scheduler.shutdown()
    Try(scheduler.awaitTermination(5, TimeUnit.SECONDS))
    movieWatch.foreach(h => Try(h.close()))
    screeningWatch.foreach(h => Try(h.close()))
  }
}

object WebReadModel {
  /** What the change streams applied during one reload: row and movie ids, and each screening's
   *  (film, city) placement. */
  private final class Streamed {
    val ids        = ConcurrentHashMap.newKeySet[String]()
    val placements = ConcurrentHashMap.newKeySet[(String, String)]()
  }

  val DefaultReloadInterval: ReadModelReloadInterval       = ReadModelReloadInterval(30.minutes)
  val DefaultColdRetryInterval: ReadModelColdRetryInterval = ReadModelColdRetryInterval(30.seconds)

  /** How long the backstop waits before re-counting a mismatch (see `backstopTick`): long enough
   *  for a write's change event to reach the model, short enough to hold its one thread briefly. */
  final case class DriftSettle(value: FiniteDuration) extends AnyVal
  val DefaultDriftSettle: DriftSettle = DriftSettle(5.seconds)
}
