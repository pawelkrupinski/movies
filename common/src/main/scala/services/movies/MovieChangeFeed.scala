package services.movies

import models.{FilmId, StoredMovieRecord}

import scala.annotation.unused

/**
 * The `movies` store's change stream: out-of-band changes delivered to attached consumers,
 * the fence a consumer that also writes applies them under, and the cursors' liveness. A role
 * of its own beside [[MovieRepositoryReader]] and the writes of [[MovieRepository]], so a
 * consumer that watches — the read-model projector, the sweeper ageing the cursor, the cache —
 * says so in its type, and a store nobody watches (a script's) carries no stream. Production's
 * is [[MongoMovieChangeFeed]], mixed into the Mongo store; the in-memory store is all three.
 */
trait MovieChangeFeed {
  /** Stream out-of-band changes to persisted rows as they happen, so the cache
   *  can apply each change incrementally instead of periodically reloading the
   *  whole collection. `onUpsert` fires once per inserted / updated / replaced
   *  row, already decoded. Best-effort: out-of-band *deletes* and any gap while
   *  the stream reconnects are left to the periodic backstop rehydrate, so a
   *  store that can't stream (disabled, or a standalone Mongo with no change
   *  streams) may return `None` and the caller simply relies on that backstop.
   *  Multiple consumers may attach — they share ONE underlying stream (see
   *  [[ChangeStreamFanout]]); the returned handle detaches just that consumer.
   *  Default: not supported. */
  def watchUpserts(onUpsert: StoredMovieRecord => Unit): Option[AutoCloseable] =
    watchChanges(onUpsert, _ => ())

  /** Like [[watchUpserts]] but also surfaces out-of-band DELETEs (by `_id`), so
   *  a consumer that must reflect row *removal* sees it — the /debug live view,
   *  where a merge deletes the losing row and the row must disappear. `onDelete`
   *  gets the deleted row's id. Default: not supported (returns None), same as
   *  [[watchUpserts]]. */
  def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = None

  /** [[watchChanges]] for a consumer that also WRITES the films it mirrors — the cache. Each
   *  upsert comes with the [[FilmWriteFence]] mark its re-read was taken under, to be applied
   *  only through `writeFence.ifUndisturbed`, so a read taken before one of the consumer's own
   *  writes cannot roll it back. Default: [[watchChanges]] with every delivery
   *  [[FilmWriteFence.Unfenced]] — right for a store that notifies synchronously with the
   *  write, where no delivery can predate a write. */
  def watchChangesFenced(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = watchChanges(onUpsert(_, FilmWriteFence.Unfenced), onDelete)

  /** [[watchChanges]] for a consumer that can also take a change confined to some venues'
   *  showtimes as those venues alone ([[VenueSlots]]), answering whether it applied it — a false
   *  answer brings it the whole film. A store that cannot read venues alone hands every change
   *  over whole. */
  def watchChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    @unused onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = watchChanges(onUpsert, onDelete)

  /** [[watchChangesWithVenues]] fenced as [[watchChangesFenced]] is. */
  def watchChangesFencedWithVenues(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit,
    @unused onVenues: (VenueSlots, Long) => VenueVerdict
  ): Option[AutoCloseable] = watchChangesFenced(onUpsert, onDelete)

  /** [[watchChangesWithVenues]] attached WITHOUT opening the stream: the consumer is dispatched
   *  every change from the stream's first, whenever another consumer's watch — or [[openChanges]] —
   *  opens it. For a consumer that must not miss what the stream applies before it is ready to
   *  apply changes itself (the read-model projector, attached as its boot reads begin, while the
   *  cache opens the stream). Default: [[watchChangesWithVenues]] — right for a store that notifies
   *  synchronously with the write, which has no stream to open. */
  def attachChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = watchChangesWithVenues(onUpsert, onDelete, onVenues)

  /** Open the change stream for the consumers [[attachChangesWithVenues]] attached, if it is not
   *  running. Default: nothing to open. */
  def openChanges(): Unit = ()

  /** Where a consumer of [[watchChangesFenced]] fences its own writes of a film. */
  lazy val writeFence: FilmWriteFence = new FilmWriteFence()

  /** When each change-stream cursor last DELIVERED an event — see [[ChangeStreamLiveness]].
   *  The worker's `/metrics` ages every cursor off it, and the read-model prune sweep uses
   *  the `movies` cursor's instant as the floor of what a silent stream may have missed.
   *  A repository with no stream answers one per instance from [[ChangeStreamLiveness.unwatched]],
   *  on its own clock: its cursors age from creation and are never stamped. */
  def changeStreamLiveness: ChangeStreamLiveness
  /** Re-reads the change stream holds back for the rest of their film's burst — see
   *  [[MovieChangeStream.Debounce]]; 0 where nothing is watched or nothing debounces. */
  def heldChanges: Int = 0
  /** Queue every held re-read now — for a caller that must see the stream settled. */
  def releaseHeldChanges(): Unit = ()
}
