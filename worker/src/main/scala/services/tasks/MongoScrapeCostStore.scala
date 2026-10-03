package services.tasks

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, PushOptions, Sorts, UpdateOptions, Updates}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import play.api.Logging
import services.movies.KeysetScan
import tools.ScanOutcome

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The `scrape_costs` collection in the country's database: one `{_id: dedupKey,
 * tasks: [n, ...]}` row per cinema, the array capped server-side at the latest
 * [[ScrapeCostStore.RecentRuns]] by `$push` + `$slice`. Written with the task queue's
 * relaxed write concern — recoverable bookkeeping, not a system of record.
 */
final class MongoScrapeCostStore(db: MongoDatabase) extends ScrapeCostStore with Logging {
  private val coll: MongoCollection[Document] =
    db.getCollection[Document](MongoScrapeCostStore.Collection).withWriteConcern(MongoTaskQueue.QueueWriteConcern)

  def record(dedupKey: String, cost: ScrapeCost): Unit =
    Try(Await.result(coll.updateOne(Filters.eq("_id", dedupKey),
      Updates.pushEach("tasks", PushOptions().slice(-ScrapeCostStore.RecentRuns), cost.tasks),
      UpdateOptions().upsert(true)).toFuture(), 10.seconds))
      .failed.foreach(e => logger.warn(s"scrape cost for $dedupKey not recorded: ${e.getMessage}"))

  // Keyset-paged, NOT one unbounded find(): one row per cinema is ~5,000 on the US
  // worker, past the size where a single cursor can StackOverflow the async driver
  // (see KeysetScan).
  def recent(): Map[String, Seq[ScrapeCost]] = {
    val costs = Map.newBuilder[String, Seq[ScrapeCost]]
    KeysetScan.scan[Document](
      label          = "MongoScrapeCostStore.recent",
      batchSize      = 2000,
      maxAttempts    = 3,
      initialBackoff = 500.millis,
      keyOf          = _.getString("_id"),
      fetchPage      = (afterId, limit) => {
        val find = afterId.fold(coll.find())(a => coll.find(Filters.gt("_id", a)))
        Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), 30.seconds)
      },
    )(batch => batch.foreach { document =>
      val tasks = document.get("tasks").filter(_.isArray).toSeq
        .flatMap(_.asArray().getValues.asScala).filter(_.isNumber).map(v => ScrapeCost(v.asNumber().intValue()))
      costs += document.getString("_id") -> tasks
    }) match {
      case ScanOutcome.Complete          => costs.result()
      case ScanOutcome.Incomplete(cause) => throw new IllegalStateException(s"${MongoScrapeCostStore.Collection} read incomplete", cause)
    }
  }
}

object MongoScrapeCostStore {
  val Collection = "scrape_costs"
}
