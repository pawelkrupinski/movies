package services.staging

import models.{MovieRecord, Source}

import scala.collection.mutable

/**
 * In-memory `StagingRepository` for tests — full write-through semantics without a
 * real Mongo cluster. Keyed by the same `cinema|sanitize(title)|year` `_id`
 * formula as `MongoStagingRepository`, and re-derives `StagingRecord` from that `_id`
 * on read exactly as the codec does, so case/diacritic variants collapse and the
 * fake differs from Mongo only at the storage boundary (a HashMap, not BSON).
 */
class InMemoryStagingRepository(seed: Seq[(Source, String, Option[Int], MovieRecord)] = Seq.empty) extends StagingRepository {

  // Store the REBUILT `StagingRecord` keyed by `_id`, in a `_id`-sorted map. The
  // promoter re-reads `findAll` O(films) times per staging drain, and on a
  // cold-corpus boot the staging set is the WHOLE corpus (~4800 rows) — the old
  // `sortBy + fromStorage`-per-read rebuilt every row on every call and dominated
  // fixture-server boot (~30s of a 47s drain, measured). Building the record once
  // at write time (writes are far rarer than reads) and keeping the map sorted
  // makes `findAll` a cheap ordered view, byte-identical to the old recompute.
  // (Prod's Mongo repo never hits this: prod staging holds a handful of trickling
  // newcomers, not the whole corpus.)
  private val store = mutable.TreeMap.empty[String, StagingRecord]
  private val lock  = new AnyRef
  val upserts       = mutable.ListBuffer.empty[(Source, String, Option[Int], MovieRecord)]
  val deletes       = mutable.ListBuffer.empty[(Source, String, Option[Int])]

  @volatile private var upsertWatcher: Option[StagingRecord => Unit] = None
  @volatile private var deleteWatcher: Option[String => Unit]        = None

  private def put(id: String, record: MovieRecord): Option[StagingRecord] = {
    // fromStorage returns None for an unknown/renamed cinema — match the old
    // findAll, which dropped those rows, by not retaining them.
    val built = StagingRecord.fromStorage(id, record)
    built.fold(store.remove(id))(sr => store.put(id, sr))
    built
  }

  seed.foreach { case (c, t, y, e) => put(StagingRecord.idFor(c, t, y), e) }

  def enabled: Boolean = true

  // `_id`-sorted (TreeMap order) like `MongoStagingRepository.findAll`, so callers
  // (the promoter) see a stable order independent of insertion/scrape order.
  def findAll(): Seq[StagingRecord] = lock.synchronized {
    store.values.toSeq
  }

  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = lock.synchronized {
    val id = StagingRecord.idFor(cinema, title, year)
    val built = put(id, record)
    upserts.append((cinema, title, year, record))
    upsertWatcher.foreach(w => built.foreach(w))
  }

  def delete(cinema: Source, title: String, year: Option[Int]): Unit = lock.synchronized {
    val id = StagingRecord.idFor(cinema, title, year)
    store.remove(id)
    deletes.append((cinema, title, year))
    deleteWatcher.foreach(_(id))
  }

  override def watchChanges(onUpsert: StagingRecord => Unit, onDelete: String => Unit): Option[AutoCloseable] = {
    upsertWatcher = Some(onUpsert)
    deleteWatcher = Some(onDelete)
    Some(new AutoCloseable { override def close(): Unit = { upsertWatcher = None; deleteWatcher = None } })
  }
}
