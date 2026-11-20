package services.movies

import com.mongodb.client.model.changestream.FullDocument
import models.{FilmId, StoredMovieRecord}
import org.bson.BsonDocument
import org.mongodb.scala.Document
import tools.ChangeStreamLiveness

/**
 * The `movies` store's change stream, mixed into a [[MongoMovieRepository]] that serves consumers
 * who watch it — `new MongoMovieRepository(…) with MongoMovieChangeFeed(…)`, at the composition
 * root. The repository is the persistence contract ([[MovieRepository]]); watching is a role of
 * its own ([[MovieChangeFeed]], CLAUDE.md "Interface segregation"), and a store nobody watches —
 * a script, a one-off read — is built without it and never opens a cursor.
 *
 * Mixed in rather than wired beside the store because the stream is the store's: every change
 * re-reads its film through the store's own stitched read, its catch-up floors come from the
 * same stamp sequence as the rows' `updatedAt` ([[ChangeStreamLiveness.now]]), and a consumer
 * that also writes (the cache) fences its writes under the same [[FilmWriteFence]].
 *
 * The subscription itself — shared cursor, fan-out, backpressure, resume token, reopen — is
 * [[MovieChangeStream]]'s; this trait builds one over the store and hands the consumers to it.
 * None on a disabled store, so every watch returns None and the caller relies on its periodic
 * backstop.
 *
 * @param resumeTokens where the change-stream resume token is kept, so the shared cursor reopens
 *        (after a terminal error or a WORKER RESTART) from where it left off — replaying events that
 *        landed while this process was down, closing the gap the consumers' periodic backstops exist
 *        for. The WORKER (the durable read-model / cache mirror) passes its database's store; web
 *        /debug keeps none, so its ephemeral cursor position cannot clobber the worker's token.
 * @param changeStreamMetrics the `movies` cursor's events by op and update-field kind. Noop for
 *        web/tests; the worker injects the Prometheus-backed sink so /metrics carries them.
 * @param screeningsMetrics what the SCREENINGS cursor's apply does — its events, and the ones it
 *        coalesced away. Separate from `changeStreamMetrics` because they are two streams answering
 *        two questions; the worker satisfies both with one object. See [[ScreeningsMetrics]].
 * @param slotsMetrics the same for the SLOTS cursor, counted under its own collection label.
 * @param changeDemandWindow how far each cursor may run ahead of the apply thread. Injected only so
 *        a spec can prove the bound with a handful of writes; production never passes it.
 * @param changeDebounce how long a change waits for the rest of its film's burst before the film is
 *        re-read — see [[MovieChangeStream.Debounce]]. The worker passes its country's; None re-reads at once.
 */
trait MongoMovieChangeFeed(
  resumeTokens:        ResumeTokenStore                    = ResumeTokenStore.Ephemeral,
  changeStreamMetrics: ChangeStreamMetrics                 = ChangeStreamMetrics.noop,
  screeningsMetrics:   ScreeningsMetrics                   = ScreeningsMetrics.noop,
  slotsMetrics:        SideCollectionChangeMetrics         = SideCollectionChangeMetrics.noop,
  changeDemandWindow:  Int                                 = ChangeStreamDemand.DefaultWindow,
  changeDebounce:      Option[MovieChangeStream.Debounce]  = None
) extends MovieRepository with MovieChangeFeed { self: MongoMovieRepository =>

  private val changeStream: Option[MovieChangeStream] = collection.map { c =>
    new MovieChangeStream(
      // The collection's own change stream, with post-images looked up (left undecoded: the stream decodes them).
      source              = (resumeAfter, observer) => {
        val base = c.watch[BsonDocument]().fullDocument(FullDocument.UPDATE_LOOKUP)
        resumeAfter.fold(base)(t => base.resumeAfter(Document(t))).subscribe(observer)
      },
      screenings          = screeningsStore,
      slots               = slotsStore,
      reread              = id => findByIdChecked(FilmId(id)),
      fence               = writeFence,
      // The `screenings` and `movie_slots` streams each persist their own sibling token.
      resumeToken         = new ChangeStreamResumeToken("movies", resumeTokens, stopwatch),
      changeStreamMetrics = changeStreamMetrics,
      screeningsMetrics   = screeningsMetrics,
      slotsMetrics        = slotsMetrics,
      changeDemandWindow  = changeDemandWindow,
      clock               = clock,
      // On the store's stamp sequence: a catch-up floor and a row's `updatedAt` are strictly ordered.
      liveness            = new ChangeStreamLiveness(clock, Some(stamps)),
      // Decoded with the collection's own codecs; a document they refuse is counted and skipped.
      postImages          = ChangeEventDecoder.of[StoredMovieDto](ChangeStreamLiveness.Movies, c.codecRegistry, decodeFailures, stopwatch),
      debounce            = changeDebounce,
      // Only where both side collections are split out is a venue readable alone.
      readVenues          = Option.when(screeningsStore.isDefined && slotsStore.isDefined)(readVenues))
  }

  /** Change events handed to the apply thread but not yet applied — see
   *  [[MovieChangeStream.applyBacklog]]; 0 on a disabled store. */
  def changeApplyBacklog: Int = changeStream.fold(0)(_.applyBacklog)

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = changeStream.map(_.watch(onUpsert, id => onDelete(FilmId(id))))

  override def watchChangesFenced(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = changeStream.map(_.watchFenced(onUpsert, id => onDelete(FilmId(id))))

  override def watchChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = changeStream.map(_.watchFenced((film, _) => onUpsert(film), id => onDelete(FilmId(id)), (venues, _) => onVenues(venues)))

  override def watchChangesFencedWithVenues(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit,
    onVenues: (VenueSlots, Long) => VenueVerdict
  ): Option[AutoCloseable] = changeStream.map(_.watchFenced(onUpsert, id => onDelete(FilmId(id)), onVenues))

  override def attachChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = changeStream.map(_.attachFenced((film, _) => onUpsert(film), id => onDelete(FilmId(id)), (venues, _) => onVenues(venues)))

  override def openChanges(): Unit = changeStream.foreach(_.open())

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatchingChangeStream: Boolean = changeStream.exists(_.isWatching)

  override def changeStreamLiveness: ChangeStreamLiveness = changeStream.fold(unwatchedLiveness)(_.liveness)

  /** A disabled store's: no cursor, nothing ever delivered. */
  private lazy val unwatchedLiveness: ChangeStreamLiveness = ChangeStreamLiveness.unwatched(clock)
  override def heldChanges: Int           = changeStream.fold(0)(_.held)
  override def releaseHeldChanges(): Unit = changeStream.foreach(_.releaseHeld())

  /** The subscription torn down for good — its final resume positions persisted — before the store. */
  abstract override def close(): Unit = { changeStream.foreach(_.close()); super.close() }
}
