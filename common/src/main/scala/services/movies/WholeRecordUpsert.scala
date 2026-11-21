package services.movies

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import com.mongodb.client.model.ReplaceOptions
import models.{CacheKey, FilmId, StoredMovieRecord}
import models.{MovieRecord, Source, SourceData}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}

/** THE WHOLE-RECORD WRITE of a film — [[MongoMovieRepository.upsert]], the path every scrape merge takes: the stored
 *  document read first, the duplicate-key guard, then `movie_slots`, `movies` and `screenings` in that order, each
 *  skipped when what is stored already equals it. What to write is [[MoviesUpsert]]'s decision; this is the I/O
 *  around it.
 *
 *  Built by the repository over its own side collections, stamp sequence and write metrics, and logging under the
 *  repository's `logger`: a refused or failed upsert reads as the repository's, which it is.
 *
 *  @param slotsForStorage the repository's [[MovieRepository.slotsForStorage]] — what of a record's slots `movies`
 *                         keeps under the split. */
private[movies] final class WholeRecordUpsert(
  movies:          MongoCollection[StoredMovieDto],
  screenings:      Option[ScreeningsRepository],
  slots:           Option[SlotsRepository],
  slotsForStorage: Map[Source, SourceData] => Map[Source, SourceData],
  stamps:          tools.MonotonicStampSequence,
  writeMetrics:    RepositoryWriteMetrics,
  logger:          play.api.Logger
) {

  /** Write the film `film` as `e` into `movies` and its side collections — see [[MovieRepository.upsert]]. */
  def upsert(film: FilmId, cacheKey: CacheKey, e: MovieRecord): WriteOutcome = {
    val id    = film.value
    val key   = StoredMovieRecord.keyFor(cacheKey)
    val label = s"MovieRepository.upsert(${cacheKey.cleanTitle}, ${cacheKey.year})"
    val restitch = restitchOf(id, e)
    // Read the stored document FIRST, before anything is written. `MoviesUpsert.plan` needs it
    // regardless, but reading it first ALSO lets the duplicate-key guard run before the slots
    // write rather than after — see `collidesWithAnother`. Handed on as the `Try` it is: a read
    // that FAILED and a document that is ABSENT both write, but only one of them is "no such
    // film", and the decision is where that is pinned. Read here, beside the replace it feeds,
    // so `NoUnguardedReadModifyWriteSpec` sees the pair (and its allowlisted WHY).
    val stored   = Try(Await.result(movies.find(Filters.eq("_id", id)).limit(1).toFuture(), 10.seconds)).map(_.headOption)
    val collision = collidesWithAnother(id, key, e, stored.toOption.flatten)
    if (collision.isFailure)
      // Not knowing is no answer: writing on would land the slots before a `movies` write the
      // index may refuse, the strand the pre-check exists to prevent. Reported, so the caller's
      // next identical scrape retries the whole upsert.
      RepositoryWrite.unit(MovieRepository.Collection, "upsert", label, writeMetrics, logger)(collision.get)
    else if (collision.get) {
      logIdentityHeld(label, e, "")
      WriteOutcome.IdentityHeld
    } else {
      // Slots first — see `writeSlots`.
      val slotsWrite  = writeSlots(id, restitch)
      // What to write, and whether the stored document already equals it — the decision is
      // `MoviesUpsert`'s, so it is unit-tested apart from the three reads that feed it.
      val plan        = MoviesUpsert.plan(id, key, e, restitch.data, slotsWrite.contains(WriteOutcome.Written), slotsForStorage, stored, stamps.next())
      val moviesWrite = RepositoryWrite.attempt(MovieRepository.Collection, "upsert", label, writeMetrics, logger) {
        val moviesLanded = plan.unchanged ||
          Try(Await.result(movies.replaceOne(Filters.eq("_id", id), plan.document, new ReplaceOptions().upsert(true)).toFuture(), MovieRepository.WriteTimeout))
            .map(_ => true).recover {
              // The pre-check above catches every collision its read could see; this remains as
              // defence for the residual race it cannot — a sibling document landing the same
              // key/tmdbId between that read and this write. Slots have already landed in that
              // narrow window, the accepted "one delayed row" trade `writeSlots` makes on this path.
              case exception if isDuplicateKey(exception) =>
                logIdentityHeld(label, e, s" (${exception.getMessage})")
                false
            }.get
        if (!moviesLanded) WriteOutcome.IdentityHeld else writeScreenings(id, restitch)
      }
      // A failed slots write is reported too, after the movies half has had its go: the embedded
      // copy kept the film's cinemas, but only a retry lands the rows themselves, and the caller
      // retries only a write it was told FAILED.
      WriteOutcome.all(moviesWrite +: slotsWrite.toSeq)
    }
  }

  /** The record an upsert writes, its slots' showtimes re-stitched. A whole-record write can carry
   *  slots STRIPPED for the cache; `showtimesOf` would drop them and `replaceFilm` would DELETE their
   *  screenings. …and a re-stitch whose READ failed under-reports the film: every slot it could not
   *  refill looks showtime-less, so the `replaceFilm` would delete it. `complete` carries that
   *  distinction down to the write, and `stored` carries the read itself, so the write can tell an
   *  unchanged film from a changed one without asking again. */
  private def restitchOf(id: String, e: MovieRecord): ScreeningsSplit.ReStitched =
    screenings.fold(ScreeningsSplit.ReStitched(e.data, Map.empty, complete = true))(ScreeningsSplit.reStitchChecked(_, id, e.data))

  /** Whether another document already holds the key or tmdbId this upsert would give the film.
   *
   *  Only asked when THIS call is about to give the film a key or tmdbId it did not already have
   *  stored — i.e. a re-key or a resolution, not the every-venue-every-tick re-merge that is
   *  upsert's normal load. That gates the extra read off the hot path: a tick that changes neither
   *  can't newly collide with a sibling document.
   *
   *  Landing the slots write BEFORE knowing the `movies` write will succeed left three films
   *  stranded on 2026-09-16: the slots write landed a genuine TMDB match, the `movies` write then
   *  hit the OTHER document already holding that tmdbId and was swallowed, and the row was left
   *  with a resolved TMDB slot but `tmdbId` permanently null — invisible on the site, and never
   *  revisited (the since-deleted `UnresolvedTmdbReaper` treated `tmdbId`+`tmdbAttempt` both absent
   *  as "never even tried"). Checking for that sibling before either write means a real collision
   *  refuses the whole upsert — slots included — leaving the film exactly as it was, instead of
   *  leaving the two collections disagreeing about it forever. A check that could not be read is
   *  the failure it is, never "no collision". */
  private def collidesWithAnother(id: String, key: String, e: MovieRecord,
                                  storedDoc: Option[StoredMovieDto]): Try[Boolean] =
    if (!MoviesUpsert.identityChanging(storedDoc.map(d => MoviesUpsert.Identity(d.key, d.tmdbId)), key, e.tmdbId))
      scala.util.Success(false)
    else
      Try(Await.result(movies.find(Filters.and(
        Filters.ne("_id", id),
        Filters.or(List(Some(Filters.eq("key", key)), e.tmdbId.map(t => Filters.eq("tmdbId", t))).flatten*)
      )).limit(1).toFuture(), 10.seconds))
        .map(_.exists(d => MoviesUpsert.heldBy(key, e.tmdbId)(MoviesUpsert.Identity(d.key, d.tmdbId))))

  private def logIdentityHeld(label: String, e: MovieRecord, detail: String): Unit =
    logger.warn(s"$label refused: another document already holds its " +
      s"key or its tmdbId=${e.tmdbId.getOrElse("?")} — the film keeps its previous document$detail")

  /** An upsert's slots write — FIRST, and `movies` only drops its embedded copy once it has
   *  actually landed. Dropping it on a FAILED slot write would leave the film with no cinemas in
   *  either place — the one way this migration can lose data — and a slots failure never throws, so
   *  it can't break the movies write, so the write itself has to report back. A film whose slot
   *  write failed keeps the embedded map, and the upsert reports the failure so the caller's next
   *  identical scrape retries it.
   *
   *  Skipped when the stored rows already match. `upsert` is the whole-record path every scrape
   *  merge takes, and `replaceFilm` rewrites EVERY row of the film — 471 of them for a film showing
   *  across the UK — so a scrape that changed only showtimes would otherwise churn the entire slot
   *  set for nothing. One indexed read replaces that. Already-matching counts as LANDED: the rows
   *  are correct, so the embedded copy is still safe to drop. A failed read returns empty, which
   *  reads as "differs" and writes — the safe direction.
   *
   *  That ONE read is used twice ([[SlotsRepository.applyFilm]]): it answers "is anything different
   *  at all" (skip the write entirely) and, handed on, "which rows are different" (write only those)
   *  — which `replaceFilm` would otherwise have to go and ask again, on the hottest write path in
   *  the system. The read is not fenced against a concurrent writer, so a slot that changes between
   *  this read and the write can leave us skipping a row whose value happens to match what we read;
   *  the price is one delayed row, which the film's next scrape writes. Not worth a transaction on
   *  the hottest write path. */
  private def writeSlots(id: String, restitch: ScreeningsSplit.ReStitched): Option[WriteOutcome] =
    slots.map(SlotsRepository.applyFilm(_, id, SlotsRepository.slotsOf(restitch.data)))

  /** Write this film's cinema showtimes to `screenings` (their authority). `replaceFilm` is upsert
   *  PLUS a delete of every slot the record doesn't name, so it may only run on a record we know is
   *  complete. When the re-stitch read failed we still write what this tick positively carries, but
   *  never the delete half — a slot we simply could not read is not a slot that stopped screening.
   *
   *  The whole call is skipped when the stored rows already match — the same guard the slots write
   *  has, and here it is FREE: `reStitchChecked` has already read these rows, so `restitch.stored`
   *  costs no round trip where the slots half pays one. This is the outer of TWO guards, and they
   *  answer different questions. `replaceFilm` itself drops the rows that did not move (see
   *  `changedSlots`), which is what stops one venue's change rewriting all 298 rows of a German
   *  release. This one asks whether ANY row moved, and when none did it saves `replaceFilm` the read
   *  it would need to find that out. `upsert` is the whole-record path EVERY scrape merge takes and
   *  a film at N venues is written by N venues, so that read is worth skipping on its own.
   *
   *  Equality is safe against the delete vector: if the stored rows equal what we would write,
   *  there is no slot for `replaceFilm` to prune. A differing read — including an empty one —
   *  writes, which is the safe direction. The three-way choice lives in `ScreeningsSplit.applyFilm`
   *  so the in-memory repository makes the same one. */
  private def writeScreenings(id: String, restitch: ScreeningsSplit.ReStitched): WriteOutcome =
    screenings.fold[WriteOutcome](WriteOutcome.Written) { s =>
      ScreeningsSplit.applyFilm(s, id, ScreeningsSplit.screeningsOf(restitch.data), restitch)
    }

  /** Mongo's duplicate-key error — a second document claiming a tmdbId the unique index
   *  already holds. */
  private def isDuplicateKey(exception: Throwable): Boolean =
    exception.isInstanceOf[com.mongodb.MongoWriteException] &&
      exception.asInstanceOf[com.mongodb.MongoWriteException].getError.getCategory == com.mongodb.ErrorCategory.DUPLICATE_KEY
}
