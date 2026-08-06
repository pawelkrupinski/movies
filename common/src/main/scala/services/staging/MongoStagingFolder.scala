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
  // See `InMemoryStagingFolder` — the country whose rules select and key the
  // group. REQUIRED here: this folder writes the real corpus.
  normalizer: services.movies.TitleNormalizer,
  // Where a folded film is written AGAIN once the transaction has committed, this
  // time through the repository's own write protocol — see `completeSideCollections`.
  // REQUIRED, not an Option: a folder that silently skips the completion writes
  // produces films with no showtimes, which is the defect this parameter exists to
  // close, and a default would let a new call site re-open it by omission.
  movieRepository: services.movies.MovieRepository
) extends StagingFolder with Logging {


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
    // What the committed attempt actually wrote to `movies`, captured per attempt so a
    // retry's plan replaces the abandoned one rather than adding to it.
    var written = Seq.empty[(CacheKey, MovieRecord)]
    while (result.isEmpty) {
      attempt += 1
      session.startTransaction()
      val outcome = Try(foldOnce(session, movies, staging, cleanTitle, candidateIds, written = _))
      StagingFold.nextAfterAttempt(outcome, attempt, maxRetries) match {
        case StagingFold.Next.Commit(newPromotions) =>
          await(publisherToFuture(session.commitTransaction()))
          completeSideCollections(written)
          result = Some(newPromotions)
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

  /** Write each folded film AGAIN, through the repository's own protocol, so it ends up
   *  in the shape every reader expects.
   *
   *  The transaction above writes `movies` directly — it has to, because the upserts and
   *  the staging deletes must commit together and the repository's write path is not
   *  session-aware. That write embeds the film's `sourceData`, showtimes and all, and
   *  touches neither `movie_slots` nor `screenings`. Under the production read-split
   *  those are not equivalent shapes: `SlotsRepository.merge` tolerates an embedded slot
   *  map (it UNIONS stored with embedded), but `ScreeningsRepository.stitch` treats
   *  `screenings` as AUTHORITATIVE and empties the showtimes of any slot it has no row
   *  for. So a folded film read back through `findAll` / `foreachRecord` had cinemas and
   *  no showtimes at all — it rendered as a title with nothing under it until that
   *  cinema's next scrape upserted it properly. Measured on prod 2026-08-06
   *  (`ktoscalkiemobcy|2024`: 0 slots, 0 screenings), and reproduced by every
   *  order-independence pass once the convergence harness was wired for the split.
   *
   *  `upsert` is the right writer rather than a second copy of the rule: it re-stitches
   *  (leaving a slot that already carries showtimes alone — `reStitchChecked` refills
   *  only STRIPPED slots), writes `movie_slots`, clears the embedded map once they land,
   *  and writes `screenings` last. Without the split it degrades to one more `movies`
   *  write of identical content.
   *
   *  Per film, and failures are logged rather than thrown: the fold itself has COMMITTED
   *  by now, so raising here would reschedule a fold that already happened. A film left
   *  incomplete is the pre-existing behaviour and the next scrape's upsert repairs it. */
  private def completeSideCollections(folded: Seq[(CacheKey, MovieRecord)]): Unit =
    folded.foreach { case (key, record) =>
      Try(movieRepository.upsert(key.cleanTitle, key.year, record)).failed.foreach { e =>
        logger.warn(s"Staging fold: '${key.cleanTitle}' (${key.year.getOrElse("—")}) committed, but its " +
          s"slots/screenings write failed (${e.getClass.getSimpleName}: ${e.getMessage}) — the film holds " +
          "no showtimes until its next scrape rewrites it.")
      }
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
    candidateIds: Option[Set[String]],
    // Reports the films this attempt wrote to `movies`, so the caller can finish them
    // through the repository once the transaction commits (`completeSideCollections`).
    // A callback rather than a return value because the outcome type is `StagingFold`'s,
    // shared with the in-memory folder, and only this one needs a post-commit step.
    wrote: Seq[(CacheKey, MovieRecord)] => Unit
  ): Seq[(CacheKey, MovieRecord)] = {
    val sanitize = normalizer.sanitize(cleanTitle)
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
      candidates.flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto, normalizer).record, normalizer)),
      cleanTitle, normalizer)
    if (stagingRows.isEmpty) Seq.empty
    else {
      // Movies `_id` = sanitize|year — match the sanitize group, any year.
      val groupRows = await(movies.find(session, Filters.regex("_id", s"^$sanitize\\|")).toFuture())
        .map(StoredMovieDto.toDomain(_, normalizer))
      // Cross-title same-tmdbId siblings (any title, OUTSIDE this sanitize group),
      // so a cross-language duplicate already in `movies` merges at fold time (see
      // StagingFold.reconcileTmdbIds). Skipped when the group carries no tmdbId.
      val ids = StagingFold.reconcileTmdbIds(stagingRows, groupRows)
      val siblings = if (ids.isEmpty) Seq.empty
        else await(movies.find(session, Filters.and(
          Filters.in("tmdbId", ids.toSeq*),
          Filters.not(Filters.regex("_id", s"^$sanitize\\|")))).toFuture()).map(StoredMovieDto.toDomain(_, normalizer))
      val plan = StagingFold.planGroup(stagingRows, groupRows ++ siblings, normalizer)
      wrote(plan.moviesUpserts.toSeq)
      plan.moviesUpserts.foreach { case (k, record) =>
        val id = StoredMovieRecord.idFor(k)
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
        await(movies.deleteOne(session, Filters.eq("_id", StoredMovieRecord.idFor(k))).toFuture()))
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
