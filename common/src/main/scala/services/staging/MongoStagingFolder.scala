package services.staging

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{ClientSession, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.reactivestreams.{Publisher, Subscriber, Subscription}
import models.MovieRecord
import play.api.Logging
import services.MongoConnection
import services.movies.{CacheKey, MovieCodecs, StoredMovieDto, StoredMovieRecord, TitleNormalizer}

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
class MongoStagingFolder(
  connection: MongoConnection,
  // See `InMemoryStagingFolder` — the country whose rules select and key the group.
  normalizer: services.movies.TitleNormalizer =
    services.movies.TitleNormalizer.forCountry(models.Country.default)
) extends StagingFolder with Logging {

  private given services.movies.TitleNormalizer = normalizer

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

  def foldGroup(cleanTitle: String, candidateIds: Option[Set[String]] = None): Seq[(CacheKey, MovieRecord)] =
    (connection.startSession(), moviesColl, stagingColl) match {
      case (Some(session), Some(movies), Some(staging)) =>
        try foldWithRetry(session, movies, staging, cleanTitle, candidateIds)
        finally session.close()
      // NOT `Seq.empty`. An empty fold is the answer for "this group is already
      // folded", and returning it here made "I could not even try" indistinguishable
      // from "there was nothing to do". A convergence leg on a real database graduated
      // nothing, logged nothing, and looked like a corpus with no work outstanding;
      // three diagnoses went past it. The callers already treat a thrown fold as a
      // reschedule — see the `Abandon` branch, which rethrows for exactly this reason —
      // so failing loudly is the behaviour they are written for.
      case _ =>
        throw new IllegalStateException(
          s"Staging fold '$cleanTitle' could not run: no Mongo session or collections " +
          "(connection disabled or unreachable). Refusing to report this as an empty fold.")
    }

  private def foldWithRetry(
    session: ClientSession,
    movies:  MongoCollection[StoredMovieDto],
    staging: MongoCollection[StoredMovieDto],
    cleanTitle: String,
    candidateIds: Option[Set[String]]
  ): Seq[(CacheKey, MovieRecord)] = {
    var attempt = 0
    var result  = Option.empty[Seq[(CacheKey, MovieRecord)]]
    while (result.isEmpty) {
      attempt += 1
      session.startTransaction()
      val outcome = Try(foldOnce(session, movies, staging, cleanTitle, candidateIds))
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
    cleanTitle: String,
    candidateIds: Option[Set[String]]
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
    // Read only the rows the caller says might belong to this group, when it says so.
    // The scan below is still correct and is what runs without a hint; it is simply
    // ruinous once staging is large, because it happens inside EVERY fold — 3,629 folds
    // over 28,572 rows is ~104 million decodes. `selectStagingGroup` still does the
    // choosing, so a hint that is too broad costs I/O and a hint that is wrong changes
    // nothing about which rows fold.
    val candidates = candidateIds match {
      case Some(ids) if ids.isEmpty => Seq.empty
      case Some(ids)                => await(staging.find(session, Filters.in("_id", ids.toSeq*)).toFuture())
      case None                     => await(staging.find(session).toFuture())
    }
    val stagingRows = StagingFold.selectStagingGroup(
      candidates.flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto).record)),
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
      // Delete the retired `movies` rows ONLY — never their side-collection rows.
      //
      // Taking the cinemas along looks right (that is what `MovieRepository.delete` does,
      // and it is why orphans accumulate here) but it is wrong for THIS caller, because
      // most of `moviesDeletes` are not films leaving — they are films being RE-KEYED.
      // `planGroup` collapses `foo|` onto `foo|2026` once TMDB concludes the year: the
      // winner is upserted above, the old key lands here, and the film's showtimes are
      // still stored under the OLD id. The winner's side rows are not written by this
      // transaction at all — they materialise later, when `MovieRepository.upsert` next
      // writes that film. Deleting the loser's rows therefore destroys the showtimes in
      // the window between the two, and the read model re-projects the film with none.
      //
      // Shipped @8033e39c6 and reverted the same day: it ran into the tail of the staging
      // backlog draining (PL alone folded 1,100+ rows, i.e. a re-key wave) and took prod
      // PL from 39,413 upcoming showtimes to 18,161 and UK from 22,250 to 7,226 before it
      // was pulled. The orphans it was meant to stop are inert rows nothing reads;
      // `scripts.ReapOrphanedFilmRows` clears them without racing a re-key. Making the
      // fold side-aware means MIGRATING the loser's rows onto the winner, not deleting
      // them — a real change, not a delete.
      plan.moviesDeletes.foreach(k =>
        await(movies.deleteOne(session, Filters.eq("_id", StoredMovieRecord.idFor(k.cleanTitle, k.year))).toFuture()))
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
