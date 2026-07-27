package services.staging

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{ClientSession, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.reactivestreams.{Publisher, Subscriber, Subscription}
import models.MovieRecord
import play.api.Logging
import services.MongoConnection
import services.movies.{CacheKey, MovieCodecs, SlotKeyed, StoredMovieDto, StoredMovieRecord, TitleNormalizer}

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Transactional `StagingFolder` for production. Folds a concluded newcomer's
 * `pending_movies` rows into `movies` inside ONE Mongo transaction — the movies
 * upserts + the staging deletes commit atomically, and the driver's transient-
 * error label drives a bounded retry, so a concurrent `movies` write can't be
 * lost (the user's "prevent overwrites"). The merge+settle DECISION is
 * `StagingFold.planGroup` (identical to the in-cache `canonicalizeBySanitize`
 * settle); only the I/O is session-aware here.
 *
 * Requires a replica set (prod `kinowo-mongo` is one — change streams already
 * depend on it). On a standalone Mongo `startSession`/transactions error out; the
 * composition root should wire `InMemoryStagingFolder` there instead.
 */
class MongoStagingFolder(connection: MongoConnection) extends StagingFolder with Logging {

  private val opTimeout  = 10.seconds
  private val maxRetries = 3

  private def collection(name: String): Option[MongoCollection[StoredMovieDto]] =
    connection.database.map(
      _.withCodecRegistry(MovieCodecs.registry)
        .getCollection[StoredMovieDto](name)
        // Transactions commit at majority so the fold is durable before the
        // staging rows are considered consumed.
        .withWriteConcern(WriteConcern.MAJORITY))

  private val moviesColl  = collection(services.movies.MovieRepository.Collection)
  private val stagingColl = collection(StagingRepository.Collection)

  // The side collections a `movies` row's cinemas live in. Untyped (`Document`) because
  // this only ever DELETES by `filmId` — no decode, so no codec, and no dependency on
  // either repository's DTO. Same transaction as the `movies` delete they follow, so a
  // film and its cinemas leave together or not at all.
  private def sideCollection(name: String): Option[MongoCollection[org.mongodb.scala.Document]] =
    connection.database.map(_.getCollection(name).withWriteConcern(WriteConcern.MAJORITY))
  private val slotsColl      = sideCollection(services.movies.SlotsRepository.Collection)
  private val screeningsColl = sideCollection(services.movies.ScreeningsRepository.Collection)

  def foldGroup(cleanTitle: String): Seq[(CacheKey, MovieRecord)] =
    (connection.startSession(), moviesColl, stagingColl) match {
      case (Some(session), Some(movies), Some(staging)) =>
        try foldWithRetry(session, movies, staging, cleanTitle)
        finally session.close()
      case _ => Seq.empty // Mongo disabled — nothing to fold
    }

  private def foldWithRetry(
    session: ClientSession,
    movies:  MongoCollection[StoredMovieDto],
    staging: MongoCollection[StoredMovieDto],
    cleanTitle: String
  ): Seq[(CacheKey, MovieRecord)] = {
    var attempt = 0
    var result  = Option.empty[Seq[(CacheKey, MovieRecord)]]
    while (result.isEmpty) {
      attempt += 1
      session.startTransaction()
      val outcome = Try(foldOnce(session, movies, staging, cleanTitle))
      StagingFold.nextAfterAttempt(outcome, attempt, maxRetries) match {
        case StagingFold.Next.Commit(newPromotions) =>
          await(publisherToFuture(session.commitTransaction())); result = Some(newPromotions)
        case StagingFold.Next.Retry(e) =>
          Try(await(publisherToFuture(session.abortTransaction())))
          logger.warn(s"Staging fold '$cleanTitle' hit a transient txn error (attempt $attempt): ${e.getMessage} — retrying.")
        case StagingFold.Next.Abandon(e) =>
          Try(await(publisherToFuture(session.abortTransaction())))
          logger.error(s"Staging fold '$cleanTitle' aborted after $attempt attempt(s): ${e.getMessage} " +
            "— rethrowing so the task reschedules instead of reporting an empty fold as success.")
          throw e
      }
    }
    result.getOrElse(Seq.empty)
  }

  /** One transaction body: read the WHOLE `sanitize(title)` GROUP's staging +
   *  movies rows (every year-variant), compute the settled plan, and apply the
   *  upserts/deletes — all on `session`. Group-scoped so `planGroup` can collapse
   *  the ±1-year variants and re-key to the TMDB year inside the transaction,
   *  exactly as the cache settle does (see `StagingFolder.foldGroup`). */
  private def foldOnce(
    session:    ClientSession,
    movies:     MongoCollection[StoredMovieDto],
    staging:    MongoCollection[StoredMovieDto],
    cleanTitle: String
  ): Seq[(CacheKey, MovieRecord)] = {
    val sanitize = TitleNormalizer.sanitize(cleanTitle)
    // Load every staging row and pick this fold's group by `sanitize(r.title)` —
    // NOT a `_id`-middle regex. A row's `_id` middle is the sanitize baked at
    // creation, which DRIFTS from the re-derived display title's sanitize (e.g.
    // "Toy Story 5- dubbing" → `toystory5` in `_id`, but display "Toy Story 5" →
    // `toystoryv`); matching the middle missed the row and the fold no-op'd
    // forever (see StagingFold.selectStagingGroup). The full scan is no costlier
    // than the old regex — `^[^|]+\|…` can't use the `_id` index either, and prod
    // staging holds only a handful of trickling newcomers.
    val stagingRows = StagingFold.selectStagingGroup(
      await(staging.find(session).toFuture())
        .flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto).record)),
      cleanTitle)
    if (stagingRows.isEmpty) Seq.empty
    else {
      // Movies `_id` = sanitize|year — match the sanitize group, any year.
      val groupRows = await(movies.find(session, Filters.regex("_id", s"^$sanitize\\|")).toFuture())
        .map(StoredMovieDto.toDomain)
      // Cross-title same-tmdbId siblings (any title, OUTSIDE this sanitize group),
      // so a cross-language duplicate already in `movies` merges at fold time (see
      // StagingFold.reconcileTmdbIds). Skipped when the group carries no tmdbId.
      val ids = StagingFold.reconcileTmdbIds(stagingRows, groupRows)
      val siblings = if (ids.isEmpty) Seq.empty
        else await(movies.find(session, Filters.and(
          Filters.in("tmdbId", ids.toSeq*),
          Filters.not(Filters.regex("_id", s"^$sanitize\\|")))).toFuture()).map(StoredMovieDto.toDomain)
      val plan = StagingFold.planGroup(stagingRows, groupRows ++ siblings)
      plan.moviesUpserts.foreach { case (k, record) =>
        val id = StoredMovieRecord.idFor(k.cleanTitle, k.year)
        await(movies.replaceOne(session, Filters.eq("_id", id),
          StoredMovieDto.fromDomain(id, record, Instant.now()), new ReplaceOptions().upsert(true)).toFuture())
      }
      plan.moviesDeletes.foreach { k =>
        val loserId = StoredMovieRecord.idFor(k.cleanTitle, k.year)
        await(movies.deleteOne(session, Filters.eq("_id", loserId)).toFuture())
        // A film's cinemas live in the side collections now, and this delete bypasses
        // `MovieRepository.delete` — which is what takes them along. Without this the
        // merge loser's rows outlive it forever: measured on prod PL 2026-07-27, 888
        // `movie_slots` rows across 19 vanished films, plus 61 orphaned `screenings`
        // films from the same bypass predating the split. In the SAME transaction, so
        // the film and its cinemas leave together.
        slotsColl.foreach(c      => await(c.deleteMany(session, SlotKeyed.filmFilter(loserId)).toFuture()))
        screeningsColl.foreach(c => await(c.deleteMany(session, SlotKeyed.filmFilter(loserId)).toFuture()))
      }
      plan.stagingDeletes.foreach(r =>
        await(staging.deleteOne(session, Filters.eq("_id", r.id)).toFuture()))
      // These `movies` deletes bypass MovieRepository.delete (direct in-txn deleteOne),
      // so audit them here — the fold losers a group merge removes from the corpus.
      if (plan.moviesDeletes.nonEmpty)
        services.movies.RemovalAudit.filmsRemoved("staging-fold",
          plan.moviesDeletes.map(k => s"${k.cleanTitle} (${k.year.getOrElse("—")})"), reason = s"folded-into='$sanitize'")
      logger.info(s"Folded staging group '$sanitize': ${stagingRows.size} row(s) → ${plan.moviesUpserts.size} movies row(s).")
      plan.newPromotions
    }
  }

  private def await[T](f: => scala.concurrent.Future[T]): T = Await.result(f, opTimeout)

  /** Adapt a reactive-streams `Publisher` (what `ClientSession.commitTransaction`
   *  / `abortTransaction` return — raw Java publishers, not scala Observables) to
   *  a `Future` so it composes with `await`. Completes on the terminal signal. */
  private def publisherToFuture[T](pub: Publisher[T]): scala.concurrent.Future[Unit] = {
    val promise = scala.concurrent.Promise[Unit]()
    pub.subscribe(new Subscriber[T] {
      def onSubscribe(s: Subscription): Unit = s.request(Long.MaxValue)
      def onNext(t: T): Unit = ()
      def onError(e: Throwable): Unit = promise.tryFailure(e)
      def onComplete(): Unit = promise.trySuccess(())
    })
    promise.future
  }
}
