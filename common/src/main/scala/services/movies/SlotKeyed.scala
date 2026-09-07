package services.movies

import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{Aggregates, Filters}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * The film-level questions every slot-keyed side collection answers — the seam
 * [[StrandedSideRows]] sweeps through. Both [[ScreeningsRepository]] and
 * [[SlotsRepository]] extend it, so a sweep asks the same two questions of each store
 * and the rule deciding what is stranded lives once, above both.
 */
trait SlotKeyedRows {
  /** The DISTINCT `filmId`s with at least one row here, plus whether the read succeeded.
   *  `(Set.empty, false)` is "could not tell" — a caller deleting on this store's behalf
   *  must skip it, not treat it as empty. Never a full document read: `screenings` is
   *  129 MB fleet-wide, and the answer is a few thousand short strings. */
  def filmIdsChecked(): (Set[String], Boolean)

  /** Drop every row of every film in `filmIds` in one write; returns the rows removed.
   *  The batch counterpart of `deleteFilm`, for a caller holding a SET of films to clear
   *  that wants one round-trip rather than one per film. */
  def deleteFilms(filmIds: Set[String]): Long
}

/**
 * The addressing shape shared by every side collection split out of `movies` and
 * keyed per cinema slot — [[ScreeningsRepository]] (`screenings`, showtimes) and
 * [[SlotsRepository]] (`movie_slots`, the SourceData metadata).
 *
 * Both store one row per `(filmId, slotKey)` under a composite `_id`, index `filmId`
 * for per-film reads, and prune stale slots with the same `$nin` predicate. Keeping
 * that in ONE place is what stops the two drifting: a change to how a composite id is
 * formed or parsed has to stay consistent across them, and the `$nin: []` edge case
 * below is load-bearing for both.
 */
object SlotKeyed {

  /** Non-printable separator, so a composite `_id` can never collide with a slot key
   *  (which itself uses `␟` between cinema and title). */
  val IdSep: Char = '\u001f'

  def idOf(filmId: String, slotKey: String): String = s"$filmId$IdSep$slotKey"

  /** The `filmId` prefix of a composite `_id` — how a DELETE change event, which
   *  carries no post-image, recovers which film changed. */
  def filmIdOf(compositeId: String): String = compositeId.takeWhile(_ != IdSep)

  /** Every stored row of one film, in either side collection — the per-film read/delete
   *  predicate. Shared so a caller that reaches a side collection directly (the staging
   *  fold, which deletes `movies` rows inside its own transaction and must take their
   *  slots + screenings with them) keys on the same field as the repositories do. */
  def filmFilter(filmId: String): Bson = Filters.eq("filmId", filmId)

  /** The rows of `incoming` a write actually has to make — the ones whose stored value differs,
   *  plus the ones with no stored row at all.
   *
   *  `replaceFilm` is film-wide in both side collections, but its CALLERS' change is not: one
   *  venue re-scrapes, its own row moves, the film's whole map therefore differs, and every
   *  other row of the film is rewritten with nothing but a fresh `updatedAt`. In `screenings`
   *  that is not merely a wasted write — the row lands in the oplog, rings that collection's
   *  change stream, and buys `ReadModelProjector` a stitch read plus a full projection OF THE
   *  SAME FILM. Measured on prod 2026-09-04: a newly-folded German release attached to 298
   *  venues produced six bursts of 298 writes, 297 of them redundant, consecutive versions of a
   *  row differing only in `updatedAt`. In `movie_slots` nothing watches the collection, so the
   *  cost is bytes rather than projections — but the rows are whole `SourceData` documents
   *  (title, synopsis, cast, poster), so it is MORE bytes, on the same film, at the same rate.
   *
   *  `readComplete = false` returns EVERYTHING: a read that did not see the film cannot say
   *  which of its rows are unchanged, and writing a row that did not need it is the harmless
   *  direction — skipping one that did is not. Same convention as `reStitchChecked`.
   *
   *  Pure, so the rule both guards rest on is unit-tested without a Mongo. */
  def changedRows[A](stored: Map[String, A], readComplete: Boolean, incoming: Map[String, A]): Map[String, A] =
    if (!readComplete) incoming else incoming.filter { case (k, v) => !stored.get(k).contains(v) }

  /** The stored rows of `filmId` that `keep` no longer names — the DELETE half of a
   *  `replaceFilm`, as ONE server-side predicate rather than a read plus a delete per
   *  stale slot.
   *
   *  Keys on the `filmId` + `slotKey` FIELDS rather than re-deriving the composite
   *  `_id`, so it stays unambiguous even for a `filmId` that itself contains [[IdSep]].
   *
   *  `keep` EMPTY yields `$nin: []` — nothing is a member of the empty set, so it
   *  matches EVERY row of the film. An empty slot map therefore clears the film
   *  exactly as a "delete every key the read returned" loop did. This predicate is the
   *  only thing standing between a whole-record write and a film's stored rows, so it
   *  is unit-tested directly. */
  def staleSlotsFilter(filmId: String, keep: Set[String]): Bson =
    Filters.and(Filters.eq("filmId", filmId), Filters.nin[String]("slotKey", keep.toSeq*))

  /** [[SlotKeyedRows.filmIdsChecked]] for a Mongo side collection: a `$group` on `filmId`,
   *  which the `filmId` index serves as a DISTINCT_SCAN, so neither the documents nor the
   *  showtimes they carry cross the wire. Shared by both Mongo stores so the two cannot
   *  answer the sweep's question differently. A failed read reports `false` and is logged
   *  through `warn` under the caller's label. */
  def distinctFilmIdsChecked[T](c: MongoCollection[T], label: String, warn: String => Unit): (Set[String], Boolean) =
    Try(Await.result(c.aggregate[Document](Seq(Aggregates.group("$filmId"))).toFuture(), 60.seconds)) match {
      case Success(groups) =>
        (groups.flatMap(_.get("_id")).collect { case id if id.isString => id.asString.getValue }.toSet, true)
      case Failure(exception) =>
        warn(s"$label.filmIds failed: ${exception.getClass.getSimpleName}: ${exception.getMessage} — " +
          "reporting the read as incomplete.")
        (Set.empty, false)
    }

  /** [[SlotKeyedRows.deleteFilms]] for a Mongo side collection: one `filmId $in` delete.
   *  Best-effort like every other side-collection write — a failure is logged through
   *  `warn` and reported as 0 rows. An empty set writes nothing. */
  def deleteFilms[T](c: MongoCollection[T], filmIds: Set[String], label: String, warn: String => Unit): Long =
    if (filmIds.isEmpty) 0L
    else Try(Await.result(c.deleteMany(Filters.in("filmId", filmIds.toSeq*)).toFuture(), 60.seconds).getDeletedCount) match {
      case Success(deleted) => deleted
      case Failure(exception) =>
        warn(s"$label.deleteFilms(${filmIds.size} film(s)) failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
        0L
    }
}
