package services.readmodel

import play.api.Logging
import settings.{ReadModelColdRetryInterval, ReadModelReloadInterval}
import tools.TickRun

import scala.util.Try

/** The keeper of [[WebReadModel]]'s two change streams (`web_movies` / `web_screenings`): opens them
 *  at boot from a checkpoint taken before the hydrate, reopens each one that ENDED on the cold-retry
 *  cadence (an outage outlasting the driver's one resume ends it for good, and nothing else opens it
 *  again), and closes them at shutdown. Every event it receives is applied by the model. */
class ReadModelStreams(
    reader: ReadModelReader,
    model: WebReadModel,
    // The backstop reload and the cold-retry cadences: the reopen backoff's cap and the
    // checkpoint-failure log's interval are measured in them.
    reloadInterval:    ReadModelReloadInterval,
    coldRetryInterval: ReadModelColdRetryInterval,
    // Each reopen of a change stream that ended (`kinowo_web_readmodel_stream_reopens_total`).
    streamMetrics:     ReadModelStreamMetrics,
    // What the checkpoint-failure log's interval is measured on; the system clock outside specs.
    clock:             java.time.Clock) extends Logging {

  @volatile private var movieWatch:     Option[StreamSubscription] = None
  @volatile private var screeningWatch: Option[StreamSubscription] = None

  /** Hydrate the model and open both watches. */
  def open(): Unit = {
    // The watches replay from BEFORE the hydrate. Opened "from now" after it, a write landing
    // between the two was in neither: on 2026-09-23 web-pl booted into two new Włodawa
    // screenings and served without them for the 30 minutes until the backstop saw the count
    // drift — and a missed write that leaves the counts equal, the backstop never sees. Replayed
    // events re-apply what the hydrate may already hold; every applier is idempotent.
    // A checkpoint that could not be TAKEN opens nothing: the cold-retry tick's reopen does, from one.
    val checkpoint = reader.streamCheckpoint()
    model.reload()
    checkpoint match {
      case tools.ReadOutcome.Failed(cause) =>
        logger.warn(s"WebReadModel: no stream checkpoint to replay from (${cause.explain}) — the watches open on the cold-retry cadence.")
      case outcome =>
        movieWatch     = reader.watchMovies(model.applyMovieUpsert, model.applyMovieDelete, outcome.answered)
        screeningWatch = reader.watchScreenings(model.applyScreeningUpsert, model.applyScreeningDelete, outcome.answered)
    }
  }

  /** Whether [[open]] opened the watches (rather than leaving them to the cold-retry tick). */
  def opened: Boolean = movieWatch.isDefined

  def close(): Unit = {
    movieWatch.foreach(h => Try(h.close()))
    screeningWatch.foreach(h => Try(h.close()))
  }

  /** Cold-retry tick — the guard `reload`'s cannot be, and the keeper of the change streams.
   *
   *  `reload` protects a WARM cache from a failed read, but a boot read that fails leaves the
   *  model with holes and nothing to protect: no films at all, or (the films read, the
   *  screenings not) every film without a showtime. The model then served that until the next
   *  backstop — 1800s away. On 2026-07-29 a Mongo OOM-kill did exactly that: the web tier
   *  restarted into the outage window and every PL and UK city served zero films until an
   *  unrelated health-check restart happened to land on a recovered Mongo.
   *
   *  So until a read of both collections has completed ([[WebReadModel.hydrated]]), keep reading.
   *  Once it has, and while both streams are live, this costs two field reads: drift is the
   *  backstop's job.
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
      if (!model.hydrated) model.coldReload()
      else if (catchUpOwed) {
        // The reopen's read failed (or it had no checkpoint to replay from — a standalone, `Absent`):
        // what was written while the streams were down reached neither. Read again — anything
        // written from the checkpoint on, the live streams carry.
        logger.warn("WebReadModel: change streams are live again but the writes made while they were down are unread — reloading.")
        val run = model.reloaded()
        if (model.lastReloadComplete) catchUpOwed = false
        run
      } else TickRun.Completed
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

  /** Reopen each stream that is not live — the way [[open]] opens them: checkpoint, read the
   *  corpus whole, then watch from the checkpoint, so what was written while the stream was down
   *  is in the read or the replay. On the backoff above; a cold model reloads every tick anyway. */
  private def reopenDeadStreams(): TickRun =
    if (ticksUntilReopen > 0) {
      ticksUntilReopen -= 1
      if (!model.hydrated) model.coldReload() else TickRun.Completed
    } else
      reader.streamCheckpoint() match {
        case tools.ReadOutcome.Failed(cause) => checkpointUnavailable(cause)
        case outcome                         => reopenFrom(outcome.answered)
      }

  /** No checkpoint is not "watch from now": the cursor opens asynchronously, once Mongo answers
   *  again — after this reopen, after the catch-up read a later tick makes — and a write landing
   *  between that read and the cursor reached neither (CI 2026-10-13). Nor is it a reopen
   *  attempt: it cost a `hello`, not a reload, so it leaves the backoff alone and the next
   *  tick takes one — the streams are back the tick after Mongo is. */
  private def checkpointUnavailable(cause: tools.ReadFailure): TickRun = {
    checkpointFailureLog.admit().foreach { held =>
      logger.warn(s"WebReadModel: no stream checkpoint to replay from (${cause.explain}) — the streams stay down; " +
        s"retrying every tick${if (held > 0) s" ($held more failed since the last line)" else ""}.")
    }
    if (!model.hydrated) { model.coldReload(); () }
    TickRun.Incomplete
  }

  /** Read the corpus whole and reopen each dead stream from `checkpoint`, one attempt further along the backoff. */
  private def reopenFrom(checkpoint: Option[StreamCheckpoint]): TickRun = {
    checkpointFailureLog = freshCheckpointFailureLog()
    reopenAttempts += 1
    ticksUntilReopen = math.min((1L << math.min(reopenAttempts, 30)) - 1, MaxTicksBetweenReopens).toInt
    logger.warn(s"WebReadModel: change stream(s) ended (movies live=${movieWatch.exists(_.live)}, " +
      s"screenings live=${screeningWatch.exists(_.live)}) — reopening, attempt $reopenAttempts.")
    val run = model.reloaded()
    // Caught up only by a whole read AND a replay from before it; short of either, the next tick
    // that finds the streams live reads again (`catchUpOwed`).
    catchUpOwed = !(model.lastReloadComplete && checkpoint.isDefined)
    if (!movieWatch.exists(_.live))
      movieWatch = reopened(movieWatch, MongoReadModelRepository.MoviesCollection)(
        reader.watchMovies(model.applyMovieUpsert, model.applyMovieDelete, checkpoint))
    if (!screeningWatch.exists(_.live))
      screeningWatch = reopened(screeningWatch, MongoReadModelRepository.ScreeningsCollection)(
        reader.watchScreenings(model.applyScreeningUpsert, model.applyScreeningDelete, checkpoint))
    run
  }

  /** A dead `watch` on `collection` closed and opened again, counted as a reopen. */
  private def reopened(watch: Option[StreamSubscription], collection: String)(subscribe: => Option[StreamSubscription]): Option[StreamSubscription] = {
    streamMetrics.reopened(collection)
    watch.foreach(dead => Try(dead.close()))
    subscribe
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
}
