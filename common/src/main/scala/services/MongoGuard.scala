package services

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import org.bson.{BsonDocument, BsonValue}
import org.bson.conversions.Bson
import org.mongodb.scala.{MongoCollection, SingleObservableFuture}
import org.mongodb.scala.model.Filters

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

/**
 * The Mongo half of a [[tools.GuardedWrite]]: a filter that matches a row only while the fields a
 * writer decided on still hold what it read, and an upserting update that answers whether that
 * guard still matched.
 *
 * A field absent when read must still be absent, and a row absent when read must still be absent
 * — the upsert's insert then meets the row another writer created and fails on the duplicate `_id`,
 * which is the guard working, answered as a mismatch. Sub-documents compare whole, as stored: read
 * them as raw BSON (a projection of the guarded fields) and hand back what came, so the comparison
 * is the server's own.
 */
object MongoGuard {

  /** Matches `id`'s row only while each of `fields` holds the value `asRead` held (absent: still
   *  absent). `asRead` is the row as read, projected to at least `fields`; `None` when there was no
   *  row — which matches only a row that has none of them (and, under an upsert, inserts). */
  def unchanged(id: BsonValue, asRead: Option[BsonDocument], fields: Seq[String]): Bson =
    Filters.and((Filters.eq("_id", id) +: fields.map { field =>
      asRead.flatMap(row => Option(row.get(field))).fold(Filters.exists(field, false))(Filters.eq(field, _))
    })*)

  /** Matches `id`'s row only while it is, whole, the document `asRead` — every field, value and
   *  order as read. For a row whose version stamp alone cannot prove it unchanged: two writers in
   *  different processes can stamp the same instant, and a guard on that stamp then takes the
   *  other writer's row for the one it read. `$literal`, so a stored string that starts with `$`
   *  is compared as the string it is. */
  def wholeUnchanged(id: BsonValue, asRead: BsonDocument): Bson =
    Filters.and(Filters.eq("_id", id), Filters.expr(new BsonDocument("$eq",
      new org.bson.BsonArray(java.util.List.of(new org.bson.BsonString("$$ROOT"), new BsonDocument("$literal", asRead))))))

  /** `update` only where `guard` matches — whether it landed. With `insert`, a row absent when read is
   *  created; a duplicate `_id` then is the guard's insert meeting a row another writer created since
   *  the read: a mismatch, not a failure. Without it, a row deleted since the read stays deleted.
   *  Anything else thrown propagates. */
  def updateIfUnchanged[T](collection: MongoCollection[T], guard: Bson, update: Bson, timeout: FiniteDuration,
                           insert: Boolean): Boolean =
    landed(Await.result(collection.updateOne(guard, update, new UpdateOptions().upsert(insert)).toFuture(), timeout))

  /** `document` in place of the row only where `guard` matches — whether it landed; `insert` as for
   *  [[updateIfUnchanged]]. */
  def replaceIfUnchanged[T](collection: MongoCollection[T], guard: Bson, document: T, timeout: FiniteDuration,
                            insert: Boolean): Boolean =
    landed(Await.result(collection.replaceOne(guard, document, new ReplaceOptions().upsert(insert)).toFuture(), timeout))

  private def landed(write: => com.mongodb.client.result.UpdateResult): Boolean =
    try {
      val result = write
      result.getMatchedCount > 0 || result.getUpsertedId != null
    } catch {
      case duplicate: MongoWriteException if MongoErrors.isDuplicateKey(duplicate) => false
    }
}
