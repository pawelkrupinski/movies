package services.staging

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{ClientSession, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.reactivestreams.{Publisher, Subscriber, Subscription}
import models.MovieRecord
import play.api.Logging
import services.MongoConnection
import services.movies.{CacheKey, MovieCodecs, MovieRecordMerge, StoredMovieDto, StoredMovieRecord, TitleNormalizer}

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
    while (result.isEmpty) {
      attempt += 1
      session.startTransaction()
      // The plan comes back WITH the outcome rather than through a callback into a `var`
      // hoisted out of this loop, and that is correctness rather than taste. `foldOnce`
      // returns early without planning anything when the group's staging rows are gone, and
      // `nextAfterAttempt` maps that `Success` to `Commit` like any other — so a retry that
      // found the group already drained (a competing worker folded it while this attempt was
      // aborting on a write conflict) used to commit an empty transaction and then apply the
      // ABANDONED attempt's plan: `moveFilm` relocating side rows onto a key this transaction
      // never wrote, and `upsert` resurrecting a `movies` document from pre-conflict content.
      // Carrying the plan alongside the result makes the applied plan definitionally the one
      // the committed attempt produced.
      val outcome = Try(foldOnce(session, movies, staging, cleanTitle, candidateIds))
      StagingFold.nextAfterAttempt(outcome.map(_.newPromotions), attempt, maxRetries) match {
        case StagingFold.Next.Commit(newPromotions) =>
          await(publisherToFuture(session.commitTransaction()))
          val plan = outcome.toOption
          // Migrate BEFORE completing: `upsert` re-stitches a stripped slot from the
          // `screenings` rows filed under the id it is writing, so the winner has to own
          // the retired rows by the time it runs, or it writes the film with an empty board.
          plan.foreach(p => migrateRetiredSideRows(p.retirements))
          completeSideCollections(plan.map(_.moviesUpserts.toSeq).getOrElse(Seq.empty))
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
      val id = StoredMovieRecord.idFor(key)
      // `upsert` REPLACES a film's side rows with what the record names: `replaceFilm`
      // upserts the payload and deletes every slot outside it. The folded record is the
      // group's plan — the staging rows plus whatever the RAW `movies` documents carried —
      // and under the storage split a migrated film's document carries no cinemas at all.
      // Writing it as-is therefore deletes the slots and screenings of every cinema that
      // was not part of this fold: the bare row losing Charlie Monroe and Kino Muza while a
      // decorated edition folded in, which the convergence order-independence leg sees as
      // the same film's screenings landing under different ids on different passes.
      //
      // So complete against the film as it currently STANDS, folded slots winning: the
      // write becomes a superset and a fold can only add cinemas to a film, never silently
      // drop the ones it never looked at.
      val (existing, readOk) = movieRepository.findByIdChecked(id)
      // …and a failed read is not "this film has no cinemas". Completing on that would
      // delete the whole board, which is the outage this method exists to prevent
      // (@8033e39c6). The fold has COMMITTED; skipping the completion leaves the film in
      // the pre-existing incomplete shape, which the next scrape's upsert repairs.
      if (!readOk)
        logger.warn(s"Staging fold: '${key.cleanTitle}' (${key.year.getOrElse("—")}) committed, but the " +
          "film could not be read back — skipping the slots/screenings completion rather than " +
          "writing a record that would delete the cinemas this fold never saw.")
      else {
        // `MovieRecordMerge.union`, not a map `++`: a colliding slot has to keep BOTH
        // sides' showtimes. The folded slot often carries none — a staging row that has
        // only just been scraped, or a cinema whose board lives in `screenings` — and
        // letting it win the key outright blanks the very showtimes this completion is
        // supposed to preserve. `union` takes the canonical's metadata and the union of
        // the boards, which is the rule the rest of the pipeline already merges by.
        val complete = existing.map(e => MovieRecordMerge.union(record, e.record)).getOrElse(record)
        Try(movieRepository.upsert(key.cleanTitle, key.year, complete)).failed.foreach { e =>
          logger.warn(s"Staging fold: '${key.cleanTitle}' (${key.year.getOrElse("—")}) committed, but its " +
            s"slots/screenings write failed (${e.getClass.getSimpleName}: ${e.getMessage}) — the film holds " +
            "no showtimes until its next scrape rewrites it.")
        }
      }
    }

  /** Hand each retired key's `movie_slots` / `screenings` rows to the row it folded into.
   *
   *  A fold retirement is almost never a film leaving — it is a film being RE-KEYED, and a
   *  re-key is a rename: the showtimes are still filed under the OLD id while the winner's
   *  side rows do not exist yet. Deleting them was tried (@8033e39c6) and took prod PL from
   *  39,413 upcoming showtimes to 18,161 in twenty minutes. Simply leaving them was the safe
   *  half of that lesson and is what shipped, but it leaves the winner with no board at all:
   *  whether a film keeps its showtimes then depends on whether the fold happened to retire
   *  it, which is a race against enrichment, and the convergence order-independence leg sees
   *  it as the same film's screenings landing under different ids on different passes.
   *
   *  `moveFilm` is the operation the revert commit asked for: it MERGES into the destination
   *  (destination rows are read first and survive) and only deletes the source once the
   *  write has been verified, so a failed move strands a duplicate rather than destroying
   *  the only copy — the same rule `MovieCache.rekey` already follows for this exact reason.
   *  Failures are logged, not thrown: the fold has COMMITTED by the time this runs. */
  private def migrateRetiredSideRows(retirements: Seq[(CacheKey, CacheKey)]): Unit =
    retirements.foreach { case (retired, winner) =>
      val from = StoredMovieRecord.idFor(retired)
      val to   = StoredMovieRecord.idFor(winner)
      if (from != to) Try(movieRepository.moveFilm(from, to)) match {
        case scala.util.Success(true)  =>
          logger.info(s"Staging fold: carried '$from' cinemas onto '$to' — a retirement is a re-key.")
        case scala.util.Success(false) =>
          logger.warn(s"Staging fold: could not carry '$from' cinemas onto '$to' (a read or write " +
            "did not happen) — the rows stay under the old id, where they remain the only copy.")
        case scala.util.Failure(e)     =>
          logger.warn(s"Staging fold: carrying '$from' cinemas onto '$to' failed " +
            s"(${e.getClass.getSimpleName}: ${e.getMessage}) — the rows stay under the old id.")
      }
    }

  /** The cinema-reported titles the group's films keep in `movie_slots` — the ones their
   *  `movies` documents do not carry.
   *
   *  The transactional reads above see RAW documents, and a MIGRATED film's `sourceData` is
   *  empty: its cinemas are side rows. `StagingFold.planGroup` picks the surviving key
   *  through `FilmCanonicalizer.canonical`, which votes on exactly those cinema-reported
   *  titles, so on the raw view the only cinemas in the vote were the ones on the STAGING
   *  rows: whichever venues happen to have diverted. One venue publishing a decorated
   *  spelling was an unopposed plurality of one and re-keyed the whole film onto it, while
   *  the settle (`MovieCache.canonicalizeBySanitize`, which reads the stitched record) saw
   *  every plain cinema and re-keyed it straight back. Neither is wrong on its own inputs
   *  and neither converges: that disagreement IS the 30-minute settle beat — ~83
   *  `merges_total{reason="canonicalize"}` a day, each flip re-requesting the film's
   *  Filmweb/Metacritic/RT ratings. Both components now vote on the same pool.
   *
   *  TITLES ONLY, deliberately, and never merged into the records `planGroup` plans over.
   *  Those records are written straight back into `movies` and then through
   *  `MovieRepository.upsert`, and a slot recovered from `movie_slots` carries no
   *  `showtimesDigest` — that field is cache-only and never persisted (`SourceData`). So a
   *  stitched slot is indistinguishable from "this cinema screens nothing"
   *  (`ScreeningsRepository.reStitchChecked` refills only slots that have a digest), and
   *  putting one into the written record makes `upsert` delete that cinema's screenings.
   *  Feeding the vote alone cannot lose data, whatever the fold then decides.
   *
   *  `findByIdChecked`, not `findById`: a failed side-collection read must not read as "this
   *  film has no cinemas", because voting on that empty pool is the very re-key this method
   *  exists to prevent. The fold fails loudly instead and its caller reschedules it — the
   *  same choice `foldGroup` makes for a missing session. */
  private def stitchedCinemaTitles(rows: Seq[StoredMovieRecord]): Seq[String] =
    rows.flatMap { row =>
      val id                 = StoredMovieRecord.idOf(row, normalizer)
      val (stitched, readOk) = movieRepository.findByIdChecked(id)
      if (!readOk) throw new IllegalStateException(
        s"Staging fold could not read '$id' back through the storage split. Refusing to " +
        "re-key the film on a view that reports none of its cinemas.")
      stitched.toSeq.flatMap(_.record.cinemaData.values.flatMap(_.title))
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
    // Returns the whole PLAN, not just the promotions: the caller finishes the written films
    // through the repository and migrates the retired rows once the transaction commits, and
    // tying those to the attempt's own return value is what stops an abandoned attempt's plan
    // being applied after a retry (see `foldWithRetry`).
  ): StagingFold.Plan = {
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
    if (stagingRows.isEmpty) StagingFold.Plan(Nil, Nil, Nil, Nil)
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
      val group = groupRows ++ siblings
      val plan  = StagingFold.planGroup(stagingRows, group, normalizer, stitchedCinemaTitles(group))
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
      plan
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
