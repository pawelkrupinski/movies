package services.staging

import com.mongodb.{MongoException, WriteConcern}
import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{ClientSession, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.reactivestreams.{Publisher, Subscriber, Subscription}
import play.api.Logging
import services.MongoConnection
import services.movies.{MovieCodecs, StoredMovieDto, StoredMovieRecord, TitleNormalizer}

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * Transactional `StagingFolder` for production. Folds a concluded newcomer's
 * `pending_movies` rows into `movies` inside ONE Mongo transaction — the movies
 * upserts + the staging deletes commit atomically, and the driver's transient-
 * error label drives a bounded retry, so a concurrent `movies` write can't be
 * lost (the user's "prevent overwrites"). The merge DECISION is `StagingFold.plan`
 * (identical to the in-cache settle); only the I/O is session-aware here.
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

  private val moviesColl  = collection("movies")
  private val stagingColl = collection("pending_movies")

  def foldFilm(cleanTitle: String, year: Option[Int]): Unit =
    (connection.startSession(), moviesColl, stagingColl) match {
      case (Some(session), Some(movies), Some(staging)) =>
        try foldWithRetry(session, movies, staging, cleanTitle, year)
        finally session.close()
      case _ => () // Mongo disabled — nothing to fold
    }

  private def foldWithRetry(
    session: ClientSession,
    movies:  MongoCollection[StoredMovieDto],
    staging: MongoCollection[StoredMovieDto],
    cleanTitle: String,
    year:       Option[Int]
  ): Unit = {
    val sanitize = TitleNormalizer.sanitize(cleanTitle)
    var attempt  = 0
    var settled  = false
    while (!settled) {
      attempt += 1
      session.startTransaction()
      Try(foldOnce(session, movies, staging, sanitize, year)) match {
        case Success(_) =>
          await(publisherToFuture(session.commitTransaction())); settled = true
        case Failure(e: MongoException)
          if e.hasErrorLabel(MongoException.TRANSIENT_TRANSACTION_ERROR_LABEL) && attempt < maxRetries =>
          Try(await(publisherToFuture(session.abortTransaction())))
          logger.warn(s"Staging fold '$cleanTitle' hit a transient txn error (attempt $attempt) — retrying.")
        case Failure(e) =>
          Try(await(publisherToFuture(session.abortTransaction())))
          logger.warn(s"Staging fold '$cleanTitle' (${year.getOrElse("—")}) aborted: ${e.getMessage}")
          settled = true
      }
    }
  }

  /** One transaction body: read the `(sanitize, year)` VARIANT's staging + movies
   *  rows, compute the plan, and apply the upserts/deletes — all on `session`.
   *  Scoped to one year so each variant folds to its own `movies` row and the
   *  periodic ±1 settle merges them (see `StagingFolder.foldFilm`). */
  private def foldOnce(
    session:  ClientSession,
    movies:   MongoCollection[StoredMovieDto],
    staging:  MongoCollection[StoredMovieDto],
    sanitize: String,
    year:     Option[Int]
  ): Unit = {
    val yearStr = year.map(_.toString).getOrElse("")
    // Staging `_id` = cinema|sanitize|year — match middle + exact year (anchored).
    val stagingRows = await(staging.find(session, Filters.regex("_id", s"^[^|]+\\|$sanitize\\|$yearStr$$")).toFuture())
      .flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto).record))
    if (stagingRows.nonEmpty) {
      // Movies `_id` = sanitize|year — match exact (sanitize, year).
      val moviesRows = await(movies.find(session, Filters.regex("_id", s"^$sanitize\\|$yearStr$$")).toFuture())
        .map(StoredMovieDto.toDomain)
      val plan = StagingFold.plan(stagingRows, moviesRows)
      plan.moviesUpserts.foreach { case (k, record) =>
        val id = StoredMovieRecord.idFor(k.cleanTitle, k.year)
        await(movies.replaceOne(session, Filters.eq("_id", id),
          StoredMovieDto.fromDomain(id, record, Instant.now()), new ReplaceOptions().upsert(true)).toFuture())
      }
      plan.moviesDeletes.foreach(k =>
        await(movies.deleteOne(session, Filters.eq("_id", StoredMovieRecord.idFor(k.cleanTitle, k.year))).toFuture()))
      plan.stagingDeletes.foreach(r =>
        await(staging.deleteOne(session, Filters.eq("_id", StagingRecord.idFor(r.cinema, r.title, r.year))).toFuture()))
      logger.info(s"Folded staging group '$sanitize': ${stagingRows.size} row(s) → ${plan.moviesUpserts.size} movies row(s).")
    }
  }

  private def await[T](f: => scala.concurrent.Future[T]): T = Await.result(f, opTimeout)

  /** Adapt a reactive-streams `Publisher` (what `ClientSession.commitTransaction`
   *  / `abortTransaction` return — raw Java publishers, not scala Observables) to
   *  a `Future` so it composes with `await`. Completes on the terminal signal. */
  private def publisherToFuture[T](pub: Publisher[T]): scala.concurrent.Future[Unit] = {
    val p = scala.concurrent.Promise[Unit]()
    pub.subscribe(new Subscriber[T] {
      def onSubscribe(s: Subscription): Unit = s.request(Long.MaxValue)
      def onNext(t: T): Unit = ()
      def onError(e: Throwable): Unit = p.tryFailure(e)
      def onComplete(): Unit = p.trySuccess(())
    })
    p.future
  }
}
