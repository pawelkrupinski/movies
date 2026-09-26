package services.movies

import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{Aggregates, Filters, Projections}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, SingleObservableFuture}

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * The film-level questions every slot-keyed side collection answers — the seam
 * [[StrandedSideRows]] sweeps through. Both [[ScreeningsRepository]] and
 * [[SlotsRepository]] extend it, so a sweep asks the same two questions of each store
 * and the rule deciding what is stranded lives once, above both.
 */
trait SlotKeyedRows extends ListingKeyedRows {
  /** The DISTINCT `filmId`s with at least one row here, plus whether the read succeeded.
   *  `(Set.empty, false)` is "could not tell" — a caller deleting on this store's behalf
   *  must skip it, not treat it as empty. Never a full document read: `screenings` is
   *  129 MB fleet-wide, and the answer is a few thousand short strings. */
  def filmIdsChecked(): (Set[String], Boolean)

  /** Drop every row of every film in `filmIds` in one write; returns the rows removed.
   *  The batch counterpart of `deleteFilm`, for a caller holding a SET of films to clear
   *  that wants one round-trip rather than one per film. */
  def deleteFilms(filmIds: Set[String]): Long

  /** Every row `_id` here (`filmId + IdSep + slotKey`), plus whether the read succeeded —
   *  the id strings only, never the rows. The stranded sweep compares the two side
   *  collections' id sets: a `screenings` row with no `movie_slots` twin projects nothing. */
  def rowIdsChecked(): (Set[String], Boolean)

  /** Every row `_id` with the instant it was last WRITTEN (`updatedAt`, which the no-op write
   *  guards leave alone), plus whether the read succeeded. A sweep that must not touch rows
   *  a newer deploy may have just written — [[RetiredVenueRows]] — ages them by this. */
  def rowWrittenAtChecked(): (Map[String, Instant], Boolean)

  /** Drop the rows with exactly these `_id`s in one write; returns the rows removed. */
  def deleteRows(ids: Set[String]): Long
}

/**
 * The listing-key questions of a slot-keyed side collection — the identity migration's DUAL
 * READ seam (docs/design/identity-resolver.md §16): which listing each row is stamped with, and
 * which rows one listing's key finds. Asked only by the shadow read (`services.identity.ListingKeyShadowRead`) and the unstamped-row
 * census; no serving path reads `listingKey` until the migration's cutover.
 */
trait ListingKeyedRows {
  /** Every row `_id` with the `listingKey` it is stamped with (`None` where the field is absent
   *  or null), plus whether the read succeeded. Ids and keys only, never the rows. */
  def rowListingKeysChecked(): (Map[String, Option[String]], Boolean)

  /** The `_id`s of the rows stamped with `listingKey` (a [[ListingKey.serialised]] key), plus
   *  whether the read succeeded — the read by listing a dual read will serve from, and what the
   *  `listingKey` index is for. */
  def rowIdsForListingKeyChecked(listingKey: String): (Set[String], Boolean)
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

  /** The slot-key suffix of a composite `_id` — [[idOf]]'s other half. */
  def slotKeyOf(compositeId: String): String = compositeId.drop(filmIdOf(compositeId).length + 1)

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
   *  row differing only in `updatedAt`. `movie_slots` has its own cursor on the same projector
   *  now (see [[SideCollectionWatch]]), so a redundant row there buys the same projection — and
   *  the rows are whole `SourceData` documents (title, synopsis, cast, poster), so it is MORE
   *  bytes, on the same film, at the same rate.
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

  /** [[SlotKeyedRows.rowIdsChecked]] for a Mongo side collection: the `_id`s alone, projected
   *  server-side, so the showtimes never cross the wire. Same failure contract as
   *  [[distinctFilmIdsChecked]]. */
  def rowIdsChecked[T](c: MongoCollection[T], label: String, warn: String => Unit,
                       paging: Paging): (Set[String], Boolean) = {
    val (docs, read) = projectedRowsChecked(c, s"$label.rowIds", warn, paging, Projections.include("_id"))
    (docs.flatMap(idOfDoc).toSet, read)
  }

  /** The stamped listing key's field, on both side collections. */
  val ListingKeyField = "listingKey"

  /** The indexes both side collections carry: `filmId` for the per-film reads and deletes, and
   *  `listingKey` for the read by listing ([[ListingKeyedRows.rowIdsForListingKeyChecked]]) that
   *  the identity migration's dual reads will serve from. Best effort, like every index here: a
   *  store without one still answers, by a collection scan. */
  def ensureIndexes[T](c: MongoCollection[T]): Unit =
    Seq("filmId", ListingKeyField).foreach(field =>
      Try(Await.result(c.createIndex(org.mongodb.scala.model.Indexes.ascending(field)).toFuture(), 10.seconds)))

  /** [[ListingKeyedRows.rowListingKeysChecked]] for a Mongo side collection: `_id` + `listingKey`
   *  projected server-side, keyset-paged like the other whole-collection id reads. */
  def rowListingKeysChecked[T](c: MongoCollection[T], label: String, warn: String => Unit,
                               paging: Paging): (Map[String, Option[String]], Boolean) = {
    val (docs, read) = projectedRowsChecked(c, s"$label.rowListingKeys", warn, paging, Projections.include("_id", ListingKeyField))
    (docs.flatMap(d => idOfDoc(d).map(_ -> d.get(ListingKeyField).collect { case k if k.isString => k.asString.getValue })).toMap, read)
  }

  /** [[ListingKeyedRows.rowIdsForListingKeyChecked]] for a Mongo side collection: one equality
   *  read on the `listingKey` index, `_id`s only. */
  def rowIdsForListingKeyChecked[T](c: MongoCollection[T], listingKey: String, label: String,
                                    warn: String => Unit): (Set[String], Boolean) =
    Try(Await.result(c.find[Document](Filters.eq(ListingKeyField, listingKey)).projection(Projections.include("_id")).toFuture(), 30.seconds)) match {
      case Success(docs) => (docs.flatMap(idOfDoc).toSet, true)
      case Failure(exception) =>
        warn(s"$label.rowIdsForListingKey failed: ${exception.getClass.getSimpleName}: ${exception.getMessage} — " +
          "reporting the read as incomplete.")
        (Set.empty, false)
    }

  /** [[SlotKeyedRows.rowWrittenAtChecked]] for a Mongo side collection: `_id` + `updatedAt`
   *  projected server-side. A row with no `updatedAt` (none is written without one) reads as
   *  the epoch — as old as it gets, since nothing current could have written it. */
  def rowWrittenAtChecked[T](c: MongoCollection[T], label: String, warn: String => Unit,
                             paging: Paging): (Map[String, Instant], Boolean) = {
    val (docs, read) = projectedRowsChecked(c, s"$label.rowWrittenAt", warn, paging, Projections.include("_id", "updatedAt"))
    (docs.flatMap { d =>
      idOfDoc(d).map(_ -> d.get("updatedAt").collect { case at if at.isDateTime =>
        Instant.ofEpochMilli(at.asDateTime.getValue) }.getOrElse(Instant.EPOCH))
    }.toMap, read)
  }

  /** How a whole-collection read of a side collection pages — the store's own keyset page
   *  size and retry budget. */
  final case class Paging(batchSize: Int, attempts: Int, backoff: FiniteDuration)

  private def idOfDoc(d: Document): Option[String] =
    d.get("_id").collect { case id if id.isString => id.asString.getValue }

  /** Every row of `c`, `projection` only, read in keyset pages ([[KeysetScan]]) — never one
   *  unbounded `find()`, the shape that overflowed the async driver's completion chain on
   *  `movies` and then `screenings`. An incomplete scan reports `false` and no rows. */
  private def projectedRowsChecked[T](c: MongoCollection[T], label: String, warn: String => Unit,
                                      paging: Paging, projection: Bson): (Seq[Document], Boolean) = {
    val buf = Vector.newBuilder[Document]
    val complete = KeysetScan.scan[Document](
      label          = label,
      batchSize      = paging.batchSize,
      maxAttempts    = paging.attempts,
      initialBackoff = paging.backoff,
      keyOf          = d => idOfDoc(d).getOrElse(throw new IllegalStateException(s"$label: a row whose _id is not a string")),
      fetchPage      = (afterId, limit) => Await.result(
        c.find[Document](afterId.fold(Filters.empty())(Filters.gt("_id", _))).projection(projection)
          .sort(org.mongodb.scala.model.Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds),
      onIncomplete   = exception =>
        warn(s"$label failed: ${exception.getClass.getSimpleName}: ${exception.getMessage} — " +
          "reporting the read as incomplete.")
    )(buf ++= _)
    if (complete) (buf.result(), true) else (Seq.empty, false)
  }

  /** A bulk delete's ids as its failure line names them: the ids themselves when there are a
   *  few (the line then says WHICH film), a count otherwise. */
  private def sample(ids: Set[String], noun: String): String =
    if (ids.sizeIs <= 3) ids.toSeq.sorted.mkString(", ") else s"${ids.size} ${noun}s"

  /** [[SlotKeyedRows.deleteRows]] for a Mongo side collection: one `_id $in` delete. A
   *  failure is logged and counted through [[RepositoryWrite]] and reported as 0 rows. */
  def deleteRows[T](c: MongoCollection[T], ids: Set[String], collection: String,
                    metrics: RepositoryWriteMetrics, logger: play.api.Logger): Long =
    if (ids.isEmpty) 0L
    else RepositoryWrite.guarded(collection, "deleteRows", s"$collection.deleteRows(${sample(ids, "row")})", metrics, logger)(
      Await.result(c.deleteMany(Filters.in("_id", ids.toSeq*)).toFuture(), 60.seconds).getDeletedCount)(_ => 0L)

  /** [[SlotKeyedRows.deleteFilms]] for a Mongo side collection: one `filmId $in` delete.
   *  Best-effort like every other side-collection write — a failure is logged and counted
   *  through [[RepositoryWrite]] and reported as 0 rows. An empty set writes nothing. */
  def deleteFilms[T](c: MongoCollection[T], filmIds: Set[String], collection: String,
                     metrics: RepositoryWriteMetrics, logger: play.api.Logger): Long =
    if (filmIds.isEmpty) 0L
    else RepositoryWrite.guarded(collection, "deleteFilms", s"$collection.deleteFilms(${sample(filmIds, "film")})", metrics, logger)(
      Await.result(c.deleteMany(Filters.in("filmId", filmIds.toSeq*)).toFuture(), 60.seconds).getDeletedCount)(_ => 0L)
}
