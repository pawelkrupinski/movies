package services.movies

import scala.concurrent.duration.*

import models.{CacheKey, FilmId}
import models.{MovieRecord, Source, SourceData}

/**
 * Persistent store for `(title, year) → MovieRecord` records: the writer contract,
 * over the reads of [[MovieRepositoryReader]]. The change stream is NOT part of it: watching is
 * the role [[MovieChangeFeed]], which a store serving watchers implements beside this one
 * (`MongoMovieRepository with MongoMovieChangeFeed`; the in-memory store, which notifies with
 * every write, is both).
 *
 * The trait is what consumers that WRITE (`MovieCache`, scripts, integration tests) see
 * — `MongoMovieRepository` (production) and `InMemoryMovieRepository` (tests) are the two
 * implementations. A caller that only reads takes [[MovieRepositoryReader]]; one that also
 * watches adds [[MovieChangeFeed]] to its type (`MovieRepositoryReader & MovieChangeFeed`).
 * Per CLAUDE.md's DIP guidance: every collaborator is wired via the trait; the concrete type
 * only appears at the composition root (`AppLoader`) and in test setup.
 */
trait MovieRepository extends MovieRepositoryReader {
  /** The slot map a `movies` document is allowed to carry — showtimes removed once
   *  `screenings` is their authority, kept when there is nowhere else to hold them.
   *
   *  On the trait because it is the storage contract, not this repository's private
   *  business: the (since deleted) staging fold wrote `movies` directly, in a shape
   *  that did not strip, and the difference was not cosmetic. A slot map with its showtimes inline grows with the number of
   *  venues screening the film, and the United States has 5,031 of them: on
   *  2026-09-01 the fold of `Avengers: Doomsday` threw
   *  `BsonMaximumSizeExceededException` on every single attempt, so the group was
   *  never consumed, so the staging reaper re-enqueued it every tick — forever, with no
   *  backoff and no give-up, since the exception carries no transient label to retry
   *  on and none to abandon on either.
   *
   *  The stripped shape loses nothing: under the split `ScreeningsSplit.stitch`
   *  treats `screenings` as authoritative and empties the showtimes of any slot it
   *  has no row for, so an embedded board is discarded on the way back out. It is
   *  weight the reader was already throwing away.
   *
   *  `final` because the rule is the storage contract, not an implementation
   *  detail — a repository that answered differently would be a repository whose
   *  documents mean something else. */
  final def slotsForStorage(data: Map[Source, SourceData]): Map[Source, SourceData] =
    if (hasScreenings) ScreeningsSplit.stripShowtimes(data) else data

  /** Remove the film stored under this id, with its side-collection rows. Never
   *  throws: a failure is logged, counted and reported as [[WriteOutcome.Failed]]. */
  def delete(id: FilmId): WriteOutcome

  /** Remove every side-collection row (`screenings`, `movie_slots`) whose film has no
   *  document in this store any more — the leftovers of deletes and merges from before
   *  [[delete]] cascaded, which nothing else ever clears.
   *  The rule, its refusals (an incomplete or empty corpus scan deletes nothing) and the
   *  reason it exists are [[StrandedSideRows]]'s, shared with the in-memory fake.
   *  Best-effort; returns what was removed. A store with no side collections has
   *  nothing stranded and reports [[StrandedSideRows.none]]. */
  def deleteStrandedSideRows(): StrandedSideRows = StrandedSideRows.none

  /** Write-through upsert of the film `id`, stored under the lookup key
   *  `sanitize(title)|year`. The same id under a new key is a RETITLE — the document
   *  stays, its `key` moves. Never throws: a failure is logged, counted and reported as
   *  [[WriteOutcome.Failed]] — including a failed side-collection write, since the next
   *  identical upsert is what retries it — and a caller holding an in-memory copy of the
   *  row must roll that copy back on one (see `MovieCache.persist`). */
  def upsert(id: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome

  /** [[upsert]] for a caller outside `services` that holds only a title and year (a
   *  fixture re-seeding rows it read back); the key is derived by this store's rules. */
  def upsert(id: FilmId, title: String, year: Option[Int], e: MovieRecord): WriteOutcome =
    upsert(id, CacheKey(title, year, normalizer), e)

  def updateIfPresent(id: FilmId, key: CacheKey, before: MovieRecord, after: MovieRecord): Boolean

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit
}

/**
 * Writes addressed by the lookup key rather than the film id — for a caller that has
 * no id in hand: seeding a spec, a one-off script. NOT part of [[MovieRepository]]'s
 * contract: production code holds ids (the cache's index, a stored row) and the
 * contract stays id-addressed; the implementations carry this so a test can write by
 * title and year. The row already stored under the key keeps its id; otherwise the
 * row is created under the key's LEGACY id (the key string itself, the shape every
 * document written before ids existed has), or a fresh id if a document already owns
 * that string. A store that cannot say whether a document holds the key is left alone
 * — a failed read is not "absent", and writing could make a second document.
 */
trait KeyAddressedMovieWrites { self: MovieRepository =>
  def upsert(title: String, year: Option[Int], e: MovieRecord): WriteOutcome = {
    val key = CacheKey(title, year, normalizer)
    findByKeyChecked(key) match {
      case tools.ReadOutcome.Answered(row)       => upsert(row.id, key, e)
      case tools.ReadOutcome.Absent(_)  =>
        scala.util.Try(Some(FilmId.legacy(key)).filterNot(holdsId).getOrElse(FilmId.fresh(key, holdsId))) match {
          case scala.util.Success(id) => upsert(id, key, e)
          case scala.util.Failure(_)  => WriteOutcome.Declined("id-unreadable")
        }
      case tools.ReadOutcome.Failed(_)  => WriteOutcome.Declined("key-unreadable")
    }
  }

  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean =
    findByKeyChecked(CacheKey(title, year, normalizer)).answered.exists(row => updateIfPresent(row.id, row.cacheKey(normalizer), before, after))

  def delete(title: String, year: Option[Int]): WriteOutcome =
    findByKeyChecked(CacheKey(title, year, normalizer)) match {
      case tools.ReadOutcome.Answered(row)      => delete(row.id)
      case tools.ReadOutcome.Absent(_) => WriteOutcome.Written                   // nothing under the key: already gone
      case tools.ReadOutcome.Failed(_) => WriteOutcome.Declined("key-unreadable")
    }
}

object MovieRepository {
  /** How many times the dotted-name fallback reads and patches again when the document moved under it. */
  val DottedReplaceAttempts = 3

  /** How long a `movies` write is waited for before it is given up on. */
  val WriteTimeout: FiniteDuration = 10.seconds

  /** How far before its floor a catch-up read starts. A row's `updatedAt` is stamped BEFORE its
   *  write is sent, so a write stamped just under a catch-up's floor can commit after that
   *  catch-up's scan has passed — and the next one, reading only past the floor, would skip it
   *  for good. Twice the time a write is waited for, so a write still in flight when a scan ran
   *  is past the floor's overlap by the time it can commit. Re-reading the rows inside it is
   *  harmless: a projection writes only what changed. */
  val CatchUpOverlap: java.time.Duration = java.time.Duration.ofMillis(2 * WriteTimeout.toMillis)

  /** The corpus collection. Named here rather than inline so
   *  [[services.DebugMirror]] can state what the local /debug mirror has to carry. */
  val Collection = "movies"
}
