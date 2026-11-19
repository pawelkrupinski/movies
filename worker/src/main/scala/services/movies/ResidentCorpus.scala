package services.movies

import models.{CacheKey, FilmId, StoredMovieRecord, StringPool}

import services.titles.TitleNormalizer

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.MovieRecord
import play.api.Logging

import java.util.Locale

/**
 * The resident corpus of a [[CaffeineMovieCache]]: the Caffeine map of every stored film, its [[CorpusIndex]], how a
 * row is shaped to be held ([[ResidentRecords]]), and the funnels that are the ONLY ways into and out of the map —
 * [[store]], [[evict]], [[compute]] and the rollbacks [[unstore]] / [[restore]] — each change announced to the resident
 * listeners ([[onResident]]); with the cache's change listeners ([[onChanged]]) and its `lastModified` stamp. The cache's
 * write funnels and its [[CorpusMirror]] reach the map only through these.
 */
private[movies] final class ResidentCorpus(
  val repository: MovieRepositoryReader & MovieChangeFeed,
  val normalizer: TitleNormalizer,
  clock: java.time.Clock,
  stringPool: StringPool
) extends MirroredCorpus with Logging {
  // `recordStats` so the resident corpus can report its hit ratio — a read served
  // here is a Mongo read not made. Unbounded, so its eviction count stays 0 by
  // construction rather than by luck.
  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().recordStats().build()
  // How a row is shaped to be held resident: its slots stripped and pooled (see [[ResidentRecords]]).
  val shape = new ResidentRecords(() => repository.hasScreenings, stringPool)

  /** The film id behind each resident key, and back ([[CorpusIndex]]): what every write asks
   *  before it lands and every change-stream apply asks to find a film's key, without a store
   *  round-trip.
   *
   *  It shadows `positive`, so it is only as correct as the funnels below: EVERY write
   *  to `positive` goes through `store` / `evict` / `compute`, and
   *  nothing else may call `positive.put` or `positive.invalidate` directly. The same
   *  funnels `announce` each change to the resident listeners (`onResident`); a rollback
   *  that puts a row back by hand announces it itself. The cache
   *  is unbounded, so there is no eviction path to miss. */
  val index: CorpusIndex = new CorpusIndex

  def held(key: CacheKey): Option[MovieRecord] = Option(positive.getIfPresent(key))
  def heldKeys: Seq[CacheKey] = { import scala.jdk.CollectionConverters._; positive.asMap().keySet().asScala.toSeq }
  def heldCount: Long = positive.estimatedSize()

  /** Write a row and keep the index with it. The ONLY way into `positive`. */
  def store(key: CacheKey, record: MovieRecord, id: FilmId): Unit = {
    positive.put(key, record)
    index.put(key, id)
    announce(key)
  }

  /** Drop a row and keep the index with it. The ONLY way out of `positive`. */
  def evict(key: CacheKey): Unit = {
    positive.invalidate(key)
    index.remove(key)
    announce(key)
  }

  /** Take back a row this cache just stored at `key` as `written` and that has no previous one, if it is still what is
   *  held there (a concurrent writer or an eviction may have replaced it since). Announced by the caller. */
  def unstore(key: CacheKey, written: MovieRecord): Unit =
    if (positive.asMap().remove(key, written)) index.remove(key)

  /** Put `previous` back at `key` in place of `written`, if `written` is still what is held there
   *  (a concurrent writer or an eviction may have replaced it since): whether it did. Announced by the caller. */
  def restore(key: CacheKey, written: MovieRecord, previous: MovieRecord, id: FilmId): Boolean =
    positive.asMap().replace(key, written, previous) && { index.put(key, id); true }

  /** Caffeine's `computeIfPresent` on `key`: the value `f` produced, or `None` when the key had left the cache by the
   *  time it computed. A row `f` hands back changed is announced. */
  def compute(key: CacheKey)(f: MovieRecord => MovieRecord): Option[MovieRecord] = {
    // Whether `f` handed back another row than the one held: a write that changes nothing is not announced, since the
    // landing hands every listing's unchanged row back, once per venue of a film shown at thousands.
    var moved = false
    val computed = Option(positive.asMap().computeIfPresent(key, new java.util.function.BiFunction[CacheKey, MovieRecord, MovieRecord] {
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = { val next = f(current); moved = next ne current; next }
    }))
    if (moved) announce(key)
    computed
  }

  private val residentListeners = new java.util.concurrent.CopyOnWriteArrayList[(CacheKey, Option[StoredMovieRecord]) => Unit]()

  def onResident(listener: (CacheKey, Option[StoredMovieRecord]) => Unit): Unit = {
    residentListeners.add(listener)
    positive.asMap().keySet().forEach(key => announce(key, java.util.List.of(listener)))
  }

  /** Tell the resident listeners what `key` holds now. Read under the key's own lock, so two writes to one key cannot
   *  reach a listener in the opposite order; `positive` itself is left as it is. Every change to `positive` calls it
   *  once the change is made — `store` and `evict`, and the writes that cannot go through them. */
  def announce(key: CacheKey, to: java.util.List[(CacheKey, Option[StoredMovieRecord]) => Unit] = residentListeners): Unit =
    if (!to.isEmpty) { positive.asMap().compute(key, (_, held) => {
      val film = Option(held).map(storedAt(key, _))
      // A listener that throws must not fail the write it is told of: the row is already held.
      to.forEach(listener => try listener(key, film) catch {
        case scala.util.control.NonFatal(e) => logger.warn(s"MovieCache: a resident listener failed on '${key.cleanTitle}': ${e.getMessage}")
      })
      held
    }); () }

  /** The film `key` holds as `record`, under its indexed id. */
  private def storedAt(key: CacheKey, record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord(key.cleanTitle, key.year, record, index.idOf(key).getOrElse(FilmId.legacy(key)), Some(StoredMovieRecord.keyFor(key)))

  /** The id of a RESIDENT row. Every write into `positive` goes through `store`, which
   *  indexes the id, so a resident row without one is a broken funnel — say so, rather
   *  than mint an id for a row that has a document. */
  def residentIdOf(key: CacheKey): FilmId =
    index.idOf(key).getOrElse(throw new IllegalStateException(s"resident row '${key.cleanTitle}' (${key.year.getOrElse("—")}) has no film id"))

  /** The resident corpus, for `kinowo_worker_cache_*`. UNBOUNDED by design — it is
   *  the hydrated corpus, not a working set — so it reports entries and no maximum:
   *  a maximum of zero would render as "full" on a ratio panel. What it is worth
   *  watching for is the SHAPE, a count that tracks the corpus rather than climbing
   *  past it. */
  def occupancy: tools.CacheOccupancy =
    tools.CacheOccupancy.of(positive, weighted = false)

  // Read from the injected clock, like every other stamp this cache takes — strictly monotonic, so a
  // put in the same instant as the last one still moves it.
  private val _lastModified = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](clock.instant())
  def lastModified: java.time.Instant = _lastModified.get()
  def touch(): Unit = { _lastModified.updateAndGet(tools.MonotonicStamp.after(_, clock)); () }

  // Above the mirror, which tells these listeners of another writer's changes.
  private val changeListeners = new java.util.concurrent.CopyOnWriteArrayList[FilmId => Unit]()
  def onChanged(listener: FilmId => Unit): Unit = { changeListeners.add(listener); () }
  def changed(id: FilmId): Unit = changeListeners.forEach(_(id))

  /** Every resident film, by title. */
  def snapshot(): Seq[StoredMovieRecord] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => storedAt(k, e) }
      .toSeq
      .sortBy(_.title.toLowerCase(Locale.ROOT))
  }

  /** Snapshot of (key, enrichment) pairs. Copy so a concurrent `store` mid-iteration doesn't surprise the caller. */
  def entries: Seq[(CacheKey, MovieRecord)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
  }
}
