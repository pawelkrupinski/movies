package services.identity

import scala.concurrent.duration.*

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.model.{IndexOptions, Indexes}
import play.api.Logging

/** One film's entry in the FilmId map: today's opaque string id (legacy `title|year` or `f…`) and
 *  the counter [[IdAssigner]] knows the film by. */
final case class FilmIdCounter(filmId: String, counter: Long)

/**
 * The persisted, APPEND-ONLY map between today's string FilmIds and [[IdAssigner]]'s `Long`
 * counters (docs/design/identity-resolver.md §6, §16.4 item 5), as a value. Every rule of the map
 * lives here, above the store, so the Mongo and in-memory stores cannot understand it differently:
 *
 *  - it is injective both ways — one counter per film id, one film id per counter;
 *  - an entry, once made, never changes and is never removed: a counter is how `IdAssigner` ranks
 *    a film as OLDER (a smaller counter wins a contested cluster), so moving one would re-decide
 *    every seeding that already used it;
 *  - films not yet mapped are numbered from one past the largest counter ever handed out, largest
 *    film first (most listings — the film more of the site's showtimes hang off), ties by id. That
 *    is [[IdSeeding]]'s ranking, so seeding an empty map gives exactly the numbering the seeding
 *    review measured, and a map seeded yesterday keeps yesterday's order today.
 *
 * Nothing serving reads it; the migration's seeding writes it (`scripts.FilmIdCounterSeed`).
 */
final class FilmIdCounters private (val entries: Seq[FilmIdCounter]) {
  private val byFilm    = entries.iterator.map(e => e.filmId -> e.counter).toMap
  private val byCounter = entries.iterator.map(e => e.counter -> e.filmId).toMap

  def counterOf(filmId: String): Option[Long] = byFilm.get(filmId)
  def filmIdOf(counter: Long): Option[String] = byCounter.get(counter)
  def size: Int = entries.size

  /** The first counter no entry holds: one past the largest ever handed out. Computed once: a
   *  projection asks it of every film it drafts, and a walk of the whole map each time was seconds
   *  of every US projection. */
  lazy val nextCounter: Long = if (entries.isEmpty) 1L else entries.iterator.map(_.counter).max + 1L

  /** The entries mapping each of `films` not mapped yet, numbered by the rule above. Films already
   *  mapped add nothing, whatever their size today. */
  def additionsFor(films: Seq[IdSeeding.Film]): Seq[FilmIdCounter] = {
    val start = nextCounter
    films.filterNot(f => byFilm.contains(f.id)).distinctBy(_.id).sortBy(f => (-f.listings.size, f.id))
      .zipWithIndex.map { case (f, i) => FilmIdCounter(f.id, start + i) }
  }

  /** This map with `additions` appended, or why they cannot be. */
  def appended(additions: Seq[FilmIdCounter]): Either[String, FilmIdCounters] = FilmIdCounters.of(entries ++ additions)

  /** This map extended over `films` (for a caller that needs every film numbered, persisted or not). */
  def covering(films: Seq[IdSeeding.Film]): FilmIdCounters = cover(films)._1

  /** [[covering]], and the entries it extends this map by: this map itself and none when every film is mapped already. */
  def cover(films: Seq[IdSeeding.Film]): (FilmIdCounters, Seq[FilmIdCounter]) = {
    val additions = additionsFor(films)
    if (additions.isEmpty) (this, Nil) else (new FilmIdCounters(entries ++ additions), additions)
  }

  override def toString: String = s"FilmIdCounters(${entries.size} entries, next ${nextCounter})"
}

object FilmIdCounters {
  val empty: FilmIdCounters = new FilmIdCounters(Seq.empty)

  /** The map holding exactly `entries`, or why it cannot: a film id or a counter held twice, or a
   *  counter below 1. */
  def of(entries: Seq[FilmIdCounter]): Either[String, FilmIdCounters] = {
    def twice[K](key: FilmIdCounter => K): Seq[K] = entries.groupBy(key).collect { case (k, es) if es.sizeIs > 1 => k }.toSeq
    val films    = twice(_.filmId).map(_.toString).sorted
    val counters = twice(_.counter).sorted
    val invalid  = entries.filter(_.counter < 1).map(_.filmId).sorted
    if (films.nonEmpty) Left(s"film ids mapped twice: ${films.take(5).mkString(", ")}")
    else if (counters.nonEmpty) Left(s"counters mapped twice: ${counters.take(5).mkString(", ")}")
    else if (invalid.nonEmpty) Left(s"counters below 1: ${invalid.take(5).mkString(", ")}")
    else Right(new FilmIdCounters(entries.sortBy(_.counter)))
  }
}

/**
 * Where the map is kept — a store, no rules. Both implementations refuse, never overwrite, an
 * entry whose film id or counter is already stored (Mongo by its `_id` and a unique `counter`
 * index, which also settles two seeders racing), which is what makes the map append-only however
 * a caller misuses it.
 */
trait FilmIdCounterStore {
  /** Every stored entry — failed when the read did not succeed. An entry that cannot be decoded is
   *  skipped, never freed (see `MongoFilmIdCounterStore.decode`). */
  def allChecked(): tools.ReadOutcome[Seq[FilmIdCounter]]
  /** Insert `entries`; returns how many landed (a refused one did not). */
  def insert(entries: Seq[FilmIdCounter]): Int
}

/** The map in a keyed store ([[tools.KeyedStore]]) of counters by film id: `{_id: filmId, counter}`, `counter` unique
 *  (in Mongo a unique index, in memory a unique field). */
abstract class KeyedFilmIdCounterStore(entries: tools.KeyedStore[String, Long]) extends FilmIdCounterStore with Logging {
  def allChecked(): tools.ReadOutcome[Seq[FilmIdCounter]] = {
    val read = tools.ReadOutcome.of(entries.all().map { case (filmId, counter) => FilmIdCounter(filmId, counter) })
    read match {
      case tools.ReadOutcome.Failed(cause) => logger.warn(s"${MongoFilmIdCounterStore.Collection}: read failed: ${cause.explain}")
      case _                               => ()
    }
    read
  }

  /** Unordered inserts: a duplicate `_id` or `counter` is refused by the store and the rest land.
   *  Any other failure throws — the seeding tool must not read it as "nothing to add". */
  def insert(counters: Seq[FilmIdCounter]): Int = {
    val landed = entries.insertAll(counters.map(e => e.filmId -> e.counter))
    if (landed < counters.size) logger.warn(s"${MongoFilmIdCounterStore.Collection}: ${counters.size - landed} entr(ies) refused as already mapped")
    landed
  }
}

object KeyedFilmIdCounterStore {
  /** The unique field. */
  val Counter = "counter"

  /** A film id is its `_id` text; an `_id` that is no text is read as a film id no film has, so its counter stays reserved
   *  ([[MongoFilmIdCounterStore.decode]]). */
  val Keys: tools.KeyCodec[String] = new tools.KeyCodec[String] {
    def id(filmId: String): org.bson.BsonValue = new org.bson.BsonString(filmId)
    def key(id: org.bson.BsonValue): Option[String] =
      Some(if (id.isString) id.asString.getValue else s"${MongoFilmIdCounterStore.UndecodedPrefix}$id")
  }

  /** The counter beside the `_id`; a document with none that decodes (below 1, or no number) is skipped and logged. */
  val Counters: tools.DocCodec[Long] = new tools.DocCodec[Long] {
    def encode(counter: Long): Document = Document(Counter -> counter)
    def decode(document: Document): Option[Long] =
      MongoFilmIdCounterStore.decode(Seq(document.toBsonDocument)).headOption.map(_.counter)
  }
}

/** The map as [[MongoFilmIdCounterStore]] would hold it: one keyed store over memory's documents, `counter` unique. */
final class InMemoryFilmIdCounterStore extends KeyedFilmIdCounterStore(new tools.KeyedStore(
  tools.InMemoryStoreDocuments.uniqueBy(KeyedFilmIdCounterStore.Counter), KeyedFilmIdCounterStore.Keys, KeyedFilmIdCounterStore.Counters))

/** The `identity_film_ids` collection: `{_id: filmId, counter}`, `counter` uniquely indexed. */
final class MongoFilmIdCounterStore(database: MongoDatabase) extends KeyedFilmIdCounterStore(new tools.KeyedStore(
  tools.MongoStoreDocuments.of(database.getCollection(MongoFilmIdCounterStore.Collection), timeout = 120.seconds,
    beforeInsert = () => MongoFilmIdCounterStore.ensureCounterIndex(database)),
  KeyedFilmIdCounterStore.Keys, KeyedFilmIdCounterStore.Counters))

object MongoFilmIdCounterStore extends Logging {
  val Collection = "identity_film_ids"

  /** An entry's document: `{_id: filmId, counter}`. */
  private[identity] def encode(entry: FilmIdCounter): org.bson.BsonDocument =
    new org.bson.BsonDocument("_id", new org.bson.BsonString(entry.filmId)).append("counter", new org.bson.BsonInt64(entry.counter))

  /** The entries `docs` hold, one by one. A document that does not decode is logged and skipped —
   *  failing the whole read on it stopped every film id being assigned until someone edited the
   *  collection by hand — without ever freeing what it holds:
   *  - its `_id`, if it is a film id, stays refused by the store's `_id` (an insert of it is turned
   *    away as already mapped, never handed out twice);
   *  - its `counter`, if it is a number, stays reserved: it is kept as an entry under a film id no
   *    film has, so the next counter still starts past it.
   *  A counter below 1 is no counter ([[FilmIdCounters.of]] refuses the whole map over one): its
   *  entry is skipped, and nothing reserved for it. */
  private[identity] def decode(docs: Seq[org.bson.BsonDocument]): Seq[FilmIdCounter] = docs.flatMap { doc =>
    val id      = Option(doc.get("_id"))
    val counter = Option(doc.get("counter")).filter(_.isNumber).map(_.asNumber.longValue).filter(_ >= 1)
    (id.filter(_.isString).map(_.asString.getValue), counter) match {
      case (Some(filmId), Some(c)) => Some(FilmIdCounter(filmId, c))
      case (_, reserved) =>
        logger.warn(s"$Collection: entry ${id.getOrElse("(no _id)")} does not decode (${doc.toJson.take(200)}) — skipped" +
          reserved.fold("")(c => s", its counter $c kept reserved"))
        reserved.map(c => FilmIdCounter(s"$UndecodedPrefix${id.fold("")(_.toString)}", c))
    }
  }

  /** The film id a reserved counter of an undecodable entry is kept under: no film's id starts so. */
  private[identity] val UndecodedPrefix = "\u0000undecoded:"

  /** The unique `counter` index, made by the first INSERT, never by a read, so a dry run against production writes nothing;
   *  throws when it cannot be in place (a store without it would not refuse a counter held twice). */
  private[identity] def ensureCounterIndex(database: MongoDatabase): Unit =
    tools.MongoIndex.ensure(database, Collection, Indexes.ascending(KeyedFilmIdCounterStore.Counter),
      IndexOptions().unique(true), Collection) match {
      case tools.MongoIndex.Outcome.NotInPlace(reason) =>
        throw new IllegalStateException(s"identity_film_ids has no unique counter index: $reason")
      case _ => ()
    }
}

/**
 * The map's one write path: read what is stored, work out the additions [[FilmIdCounters]] owes
 * `films`, check the result is still a valid map, and insert only those. Shared by every store.
 */
final class FilmIdMapping(store: FilmIdCounterStore) {

  /** The stored map, or why it cannot be read. */
  def load(): Either[String, FilmIdCounters] = store.allChecked() match {
    case tools.ReadOutcome.Answered(entries) => FilmIdCounters.of(entries)
    case other                 => Left(s"the stored map could not be read: ${other.explain}")
  }

  /** What appending `films` would add — the dry run. */
  def plan(films: Seq[IdSeeding.Film]): Either[String, (FilmIdCounters, Seq[FilmIdCounter])] =
    for {
      current   <- load()
      additions  = current.additionsFor(films)
      _         <- current.appended(additions)
    } yield (current, additions)

  /** Append the entries `films` owes, returning those that landed. A store that refused some (a
   *  concurrent seeder took the counter) leaves them for the next run, which renumbers from the
   *  map as it then stands. */
  def append(films: Seq[IdSeeding.Film]): Either[String, Int] =
    plan(films).map { case (_, additions) => store.insert(additions) }
}
