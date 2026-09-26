package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{IndexOptions, Indexes, InsertManyOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

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

  /** The first counter no entry holds: one past the largest ever handed out. */
  def nextCounter: Long = if (entries.isEmpty) 1L else entries.iterator.map(_.counter).max + 1L

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
  def covering(films: Seq[IdSeeding.Film]): FilmIdCounters =
    new FilmIdCounters(entries ++ additionsFor(films))

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
  /** Every stored entry, plus whether the read succeeded. */
  def allChecked(): (Seq[FilmIdCounter], Boolean)
  /** Insert `entries`; returns how many landed (a refused one did not). */
  def insert(entries: Seq[FilmIdCounter]): Int
}

final class InMemoryFilmIdCounterStore extends FilmIdCounterStore {
  private val stored = scala.collection.mutable.LinkedHashMap.empty[String, Long]
  def allChecked(): (Seq[FilmIdCounter], Boolean) =
    synchronized((stored.iterator.map { case (f, c) => FilmIdCounter(f, c) }.toSeq, true))
  def insert(entries: Seq[FilmIdCounter]): Int = synchronized {
    entries.count { e =>
      val free = !stored.contains(e.filmId) && !stored.valuesIterator.contains(e.counter)
      if (free) stored.update(e.filmId, e.counter)
      free
    }
  }
}

/** The `identity_film_ids` collection: `{_id: filmId, counter}`, `counter` uniquely indexed. */
final class MongoFilmIdCounterStore(database: MongoDatabase) extends FilmIdCounterStore with Logging {
  private val coll: MongoCollection[Document] = database.getCollection[Document](MongoFilmIdCounterStore.Collection)

  // Built by the first INSERT, never by a read, so a dry run against production writes nothing.
  private lazy val counterIndex: Unit = {
    Await.result(coll.createIndex(Indexes.ascending("counter"), IndexOptions().unique(true)).toFuture(), 30.seconds)
    ()
  }

  def allChecked(): (Seq[FilmIdCounter], Boolean) =
    Try(Await.result(coll.find().toFuture(), 60.seconds)) match {
      case Success(docs) =>
        (docs.flatMap { d =>
          val doc = d.toBsonDocument
          Try(FilmIdCounter(doc.getString("_id").getValue, doc.getNumber("counter").longValue)).toOption
        }, true)
      case Failure(exception) =>
        logger.warn(s"${MongoFilmIdCounterStore.Collection}: read failed: ${exception.getMessage}")
        (Seq.empty, false)
    }

  /** Unordered inserts: a duplicate `_id` or `counter` is refused by the store and the rest land.
   *  Any other failure throws — the seeding tool must not read it as "nothing to add". */
  def insert(entries: Seq[FilmIdCounter]): Int =
    if (entries.isEmpty) 0
    else try {
      counterIndex
      Await.result(coll.insertMany(entries.map(e => Document("_id" -> e.filmId, "counter" -> e.counter)),
        InsertManyOptions().ordered(false)).toFuture(), 120.seconds).getInsertedIds.size()
    } catch {
      case bulk: com.mongodb.MongoBulkWriteException if bulk.getWriteErrors.asScala.forall(_.getCode == 11000) =>
        logger.warn(s"${MongoFilmIdCounterStore.Collection}: ${bulk.getWriteErrors.size()} entr(ies) refused as already mapped")
        bulk.getWriteResult.getInsertedCount
    }
}

object MongoFilmIdCounterStore {
  val Collection = "identity_film_ids"
}

/**
 * The map's one write path: read what is stored, work out the additions [[FilmIdCounters]] owes
 * `films`, check the result is still a valid map, and insert only those. Shared by every store.
 */
final class FilmIdMapping(store: FilmIdCounterStore) {

  /** The stored map, or why it cannot be read. */
  def load(): Either[String, FilmIdCounters] = store.allChecked() match {
    case (entries, true) => FilmIdCounters.of(entries)
    case (_, false)      => Left("the stored map could not be read")
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
