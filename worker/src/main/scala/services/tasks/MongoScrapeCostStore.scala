package services.tasks

import scala.jdk.CollectionConverters.*
import scala.util.Try

import org.bson.BsonInt32
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.bson.collection.immutable.Document
import play.api.Logging
import tools.{DocCodec, KeyedStore, StoreCappedLists, StoreReader}

/**
 * [[ScrapeCostStore]] over a keyed store ([[tools.KeyedStore]]): one `{_id: dedupKey, tasks: [n, ...]}` row per cinema,
 * the array capped at the latest [[ScrapeCostStore.RecentRuns]] by the store's capped append (`$push` + `$slice` in
 * Mongo, one server-side step), never a replace. A failed write is logged and dropped: bookkeeping, not a system of
 * record. The read is keyset-paged, not one unbounded find: one row per cinema is ~5,000 on the US worker, past the size
 * where a single cursor can StackOverflow the async driver.
 */
abstract class KeyedScrapeCostStore(costs: StoreReader[String, Seq[ScrapeCost]] & StoreCappedLists[String]) extends ScrapeCostStore with Logging {
  def record(dedupKey: String, cost: ScrapeCost): Unit =
    Try(costs.append(dedupKey, KeyedScrapeCostStore.Field, new BsonInt32(cost.tasks), ScrapeCostStore.RecentRuns))
      .failed.foreach(e => logger.warn(s"scrape cost for $dedupKey not recorded: ${e.getMessage}"))

  def recent(): Map[String, Seq[ScrapeCost]] = costs.all().toMap
}

object KeyedScrapeCostStore {
  private val Field = "tasks"

  /** A cinema's document: its recent costs, oldest first, as the capped append keeps them. */
  val Codec: DocCodec[Seq[ScrapeCost]] = new DocCodec[Seq[ScrapeCost]] {
    def encode(recent: Seq[ScrapeCost]): Document = Document(Field -> BsonArray.fromIterable(recent.map(c => new BsonInt32(c.tasks))))
    def decode(document: Document): Option[Seq[ScrapeCost]] =
      Some(document.get(Field).filter(_.isArray).toSeq.flatMap(_.asArray().getValues.asScala).filter(_.isNumber).map(v => ScrapeCost(v.asNumber().intValue())))
  }
}

/** The `scrape_costs` collection in the country's database, written with the task queue's relaxed write concern. */
final class MongoScrapeCostStore(db: MongoDatabase) extends KeyedScrapeCostStore(new KeyedStore(
  tools.MongoStoreDocuments.of(db.getCollection(MongoScrapeCostStore.Collection).withWriteConcern(tools.MongoWriteConcerns.Bookkeeping)),
  tools.KeyCodec.Text, KeyedScrapeCostStore.Codec))

object MongoScrapeCostStore {
  val Collection = "scrape_costs"
}
