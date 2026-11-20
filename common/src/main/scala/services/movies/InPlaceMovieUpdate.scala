package services.movies

import scala.concurrent.Await

import com.mongodb.client.model.UpdateOptions
import models.{CacheKey, FilmId, StoredMovieRecord}
import models.{MovieRecord, Source, SourceData}
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoCollection, SingleObservableFuture}

/** THE FIELD-LEVEL UPDATE of a stored film — [[MongoMovieRepository.updateIfPresent]]: the diff from `before` to
 *  `after` split into its three writes (the `movies` patch, the showtime deltas to `screenings`, the slot deltas to
 *  `movie_slots`), each skipped when empty, the side writes applied only once the row is known present, and a patch
 *  naming a dotted cinema written as a guarded whole-document replace.
 *
 *  Built by the repository over its side collections, stamp sequence and write metrics, and logging under its
 *  `logger`. `readWhole` is the repository's read of the stored document the dotted-name fallback patches, decoded
 *  under its title rules — its seam, so a spec can land another write between that read and the replace.
 *
 *  @param slotsForStorage the repository's [[MovieRepository.slotsForStorage]]. */
private[movies] final class InPlaceMovieUpdate(
  movies:          MongoCollection[StoredMovieDto],
  screenings:      Option[ScreeningsRepository],
  slots:           Option[SlotsRepository],
  slotsForStorage: Map[Source, SourceData] => Map[Source, SourceData],
  stamps:          tools.MonotonicStampSequence,
  writeMetrics:    RepositoryWriteMetrics,
  logger:          play.api.Logger,
  readWhole:       String => Option[InPlaceMovieUpdate.ReadDocument]
) {

  /** Update the stored row of `film` from `before` to `after` — see [[MovieRepository.updateIfPresent]]. */
  def updateIfPresent(film: FilmId, cacheKey: CacheKey, before: MovieRecord, after: MovieRecord): Boolean = {
    val id     = film.value
    val update = planUpdate(before, after)
    // `false` for an absent row AND for a failed write — both mean the store does not hold
    // `after`, and `MovieCache.putIfPresent` rolls its copy back on either. A side-collection
    // write that failed counts: its row is what the next identical update retries.
    if (update.isEmpty) true
    else RepositoryWrite.attempt(MovieRepository.Collection, "updateIfPresent", s"MovieRepository.updateIfPresent(${cacheKey.cleanTitle}, ${cacheKey.year})", writeMetrics, logger) {
      // Present when the movies write matched, OR a side-collection-only change (no movies
      // write, None); false only on Some(0) — the row is absent, so don't apply the
      // side-collection deltas (no orphan rows) and report not-present.
      if (!patchMovies(id, StoredMovieRecord.keyFor(cacheKey), update.patch).forall(_ > 0)) WriteOutcome.Declined("absent")
      else applySideWrites(id, update)
    } == WriteOutcome.Written
  }

  /** The three writes an update from `before` to `after` makes, each from the ORIGINAL records.
   *
   *  Showtime deltas and listing-key restamps → `screenings` (its authority under the split), only
   *  when a screenings repo is wired. Slot deltas → `movie_slots` (dual write): `slotsOf` drops
   *  showtimes itself, so a showtimes-only change yields no slot write and the two side collections
   *  stay independent.
   *
   *  The movies patch is diffed from the (split-stripped) records, so a showtimes-only change yields
   *  an EMPTY movies patch — movies stays put, no fat change event. When all three are empty the row
   *  already equals `after`: the write (and its no-op `$set` + change event) is skipped. "Present and
   *  up to date" is still success. With the slots split on, `movies` is not where slots live, so the
   *  patch must not carry them: `upsert` drops the embedded map once the slots land, and a later
   *  patch that still wrote `sourceData.<slot>` would resurrect it field by field and undo exactly
   *  the shrink this split exists for. Dropping it here means the embedded map simply goes stale —
   *  reads prefer `movie_slots` whenever a film has rows there, and fall back to that stale copy
   *  only for a film with none, which is the same already-correct value it had before. */
  private def planUpdate(before: MovieRecord, after: MovieRecord): InPlaceMovieUpdate.PlannedUpdate = {
    val ops        = if (screenings.isDefined) ScreeningsSplit.writesFor(before.data, after.data) else ScreeningsWrites.none
    val slotWrites = if (slots.isDefined) SlotsRepository.slotOps(before.data, after.data) else Map.empty[String, Option[SourceData]]
    val rawPatch   = MovieRecordPatch.diff(before.copy(data = slotsForStorage(before.data)), after.copy(data = slotsForStorage(after.data)))
    InPlaceMovieUpdate.PlannedUpdate(if (slots.isDefined) rawPatch.copy(data = Map.empty) else rawPatch, ops, slotWrites)
  }

  /** Write the movies half of an update: the documents it matched, or None for a side-collection-only
   *  change, which leaves `movies` alone — `screenings` and `movie_slots` each have a cursor of their
   *  own on the same fan-out (see `MovieChangeStream`). A slots-only change used to TOUCH `movies`
   *  anyway — a one-field `updatedAt` bump standing in for the watcher `movie_slots` did not have —
   *  and once it had one that bump fanned the same change out twice, which is the double projection
   *  the old arrangement existed to avoid.
   *
   *  MongoDB update-operator paths treat '.' as a nesting separator, so a per-source `$set` on
   *  `sourceData.<displayName>` is rejected when a source's displayName has a dot ("Helios Ostrów
   *  Wlkp."); that patch falls back to [[dottedReplace]]. */
  private def patchMovies(id: String, key: String, patch: MovieRecordPatch): Option[Long] =
    Option.when(!patch.isEmpty) {
      if (patch.data.keysIterator.exists(_.displayName.contains('.'))) dottedReplace(id, key, patch)
      else Await.result(movies.updateOne(Filters.eq("_id", id), MovieUpdates.of(patch, stamps.next()), new UpdateOptions().upsert(false)).toFuture(), MovieRepository.WriteTimeout)
        .getMatchedCount
    }

  /** The dotted-name fallback: the documents a guarded whole-document replace matched.
   *
   *  It can't drive the field-level diff (the dotted `$set` path is rejected), so it replaces the
   *  whole document. Writing the cache row BLINDLY would NULL any Mongo-owned field the cache lacks
   *  — a rating not yet rehydrated after a restart, or an out-of-band edit (FilmwebUrlAudit). It
   *  reads the current doc and applies the SAME patch to it, so the replace carries exactly the
   *  diff the `$set` path would, every other field preserved. Absent row → nothing to replace →
   *  report not-present.
   *
   *  …and the replace lands only over the document as read, WHOLE: a rating refresh landing between
   *  the read and the replace was wiped by it, the same null this fallback exists to avoid. Not over
   *  its `updatedAt` alone — this process's stamps are strictly increasing, but another process's (a
   *  second worker mid-rollout, a script) can equal the one read, and that write would then be taken
   *  for the row as read. On a mismatch the patch is applied again to the document as it now is. */
  private def dottedReplace(id: String, key: String, patch: MovieRecordPatch): Long =
    tools.GuardedWrite(MovieRepository.DottedReplaceAttempts)(() => readWhole(id)) { asRead =>
      InPlaceMovieUpdate.dottedReplaceRecord(asRead.map(_.record), patch)
    } { (asRead, merged) =>
      // An absent row decided nothing above, so only a row as read reaches here.
      asRead.exists(read => services.MongoGuard.replaceIfUnchanged(movies,
        services.MongoGuard.wholeUnchanged(BsonString(id), read.whole),
        StoredMovieDto.fromDomain(id, key, merged, stamps.next()), MovieRepository.WriteTimeout, insert = false))
    } match {
      case tools.GuardedWrite.Landed(_)                 => 1L
      case tools.GuardedWrite.Unneeded                  => 0L
      case tools.GuardedWrite.ChangedUnderYou(attempts) =>
        throw new IllegalStateException(s"'$id' changed between its read and its replace $attempts times in a row")
    }

  /** The side-collection deltas of an update whose row is present. */
  private def applySideWrites(id: String, update: InPlaceMovieUpdate.PlannedUpdate): WriteOutcome =
    WriteOutcome.all(
      screenings.toSeq.flatMap(update.screenings.applyTo(_, id)) ++
      slots.toSeq.flatMap(s =>
        s.upsertSlots(id, update.slots.collect { case (k, Some(sd)) => k -> sd }.toMap) +:
          update.slots.collect { case (k, None) => s.deleteSlot(id, k) }.toSeq))
}

object InPlaceMovieUpdate {
  /** A `movies` document as the dotted-name fallback read it: its `record`, to patch, and `whole`, as
   *  stored, for the replace to be guarded on. */
  final case class ReadDocument(record: MovieRecord, whole: org.bson.BsonDocument)

  /** The full-document replacement to write in the dotted-displayName fallback of
   *  [[updateIfPresent]] (a per-source `$set sourceData.<name>` is rejected when the
   *  name holds a '.'). Apply the SAME field-level patch to the CURRENT persisted
   *  record so the replace carries exactly the diff the `$set` path would — every
   *  Mongo-owned field the in-memory cache row lacks (a rating not yet rehydrated
   *  after a restart, an out-of-band audit edit) is preserved rather than nulled by
   *  blindly writing the cache row. `None` when the row is absent (nothing to
   *  replace → report not-present, don't upsert). Pure — no Mongo I/O. */
  def dottedReplaceRecord(persisted: Option[MovieRecord], patch: MovieRecordPatch): Option[MovieRecord] =
    persisted.map(patch.applyTo)

  /** What an update writes: the `movies` patch, the showtimes deltas and the slot deltas. */
  private final case class PlannedUpdate(patch: MovieRecordPatch, screenings: ScreeningsWrites,
                                         slots: Map[String, Option[SourceData]]) {
    def isEmpty: Boolean = patch.isEmpty && screenings.isEmpty && slots.isEmpty
  }
}
