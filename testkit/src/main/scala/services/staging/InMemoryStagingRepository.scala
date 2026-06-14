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

  private val store = mutable.LinkedHashMap.empty[String, MovieRecord]
  private val lock  = new AnyRef
  val upserts       = mutable.ListBuffer.empty[(Source, String, Option[Int], MovieRecord)]
  val deletes       = mutable.ListBuffer.empty[(Source, String, Option[Int])]

  @volatile private var upsertWatcher: Option[StagingRecord => Unit] = None

  seed.foreach { case (c, t, y, e) => store.put(StagingRecord.idFor(c, t, y), e) }

  def enabled: Boolean = true

  // Sorted by `_id`, like `MongoStagingRepository.findAll` — so callers (the promoter)
  // see a stable order independent of insertion/scrape order.
  def findAll(): Seq[StagingRecord] = lock.synchronized {
    store.iterator.toSeq.sortBy(_._1).flatMap { case (id, record) => StagingRecord.fromStorage(id, record) }
  }

  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = lock.synchronized {
    val id = StagingRecord.idFor(cinema, title, year)
    store.put(id, record)
    upserts.append((cinema, title, year, record))
    upsertWatcher.foreach(w => StagingRecord.fromStorage(id, record).foreach(w))
  }

  def delete(cinema: Source, title: String, year: Option[Int]): Unit = lock.synchronized {
    store.remove(StagingRecord.idFor(cinema, title, year))
    deletes.append((cinema, title, year))
  }

  override def watchUpserts(onUpsert: StagingRecord => Unit): Option[AutoCloseable] = {
    upsertWatcher = Some(onUpsert)
    Some(new AutoCloseable { override def close(): Unit = upsertWatcher = None })
  }
}
