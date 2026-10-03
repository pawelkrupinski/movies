package services

import com.mongodb.MongoCommandException
import org.bson.conversions.Bson
import org.bson.{BsonArray, BsonDocument, BsonInt64, BsonNumber, BsonValue}
import org.mongodb.scala.bson.collection.immutable.{Document => ImmutableDocument}
import org.mongodb.scala.model.IndexOptions
import org.mongodb.scala.{Document, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** Ensure an index exists with the wanted options — and, when an earlier PLAIN index of
 *  the same keys is in the way of a wanted UNIQUE one, convert that index in place.
 *
 *  `createIndex` cannot alter an existing index. Asked for `unique` over a plain index of
 *  the same name it answers IndexKeySpecsConflict (86) — measured on 7.0 and the fleet's
 *  8.3, for a uniqueness change and a partial-filter change alike — and the callers used to
 *  handle only IndexOptionsConflict (85), log the 86 at WARN, and carry on WITHOUT the
 *  unique index: `movies.key` has been plain in all five country databases since it was
 *  made unique (096df7444), one WARN per boot.
 *
 *  NEVER DROP. Dropping and rebuilding leaves a window with no index at all, and when a
 *  duplicate exists the rebuild then fails and leaves the collection with NO index — the
 *  old 85 branch here did exactly that (and, since a modern mongod answers 85 only when the
 *  same spec already exists under ANOTHER name, it dropped an index that did not exist). The
 *  in-place conversion (MongoDB 6.0+) is two `collMod`s: `prepareUnique` makes the index
 *  refuse NEW duplicates, then `unique` converts it — and that second step fails cleanly
 *  with CannotConvertIndexToUnique (359), listing the violating documents, while the old
 *  index stays exactly as it was. Re-running either on an index that already has the flag
 *  is a no-op, so two pods booting together both succeed.
 *
 *  Anything other than "plain where unique is wanted" — a different partial filter, a
 *  different sparseness, unique where plain is wanted — cannot be converted in place and is
 *  reported at ERROR for an operator, with the old index left serving.
 *
 *  `collMod` is NOT in the `readWrite` role (see [[MongoTtlIndex]]), so a credential
 *  without it gets Unauthorized (13); that too is an ERROR naming the missing privilege,
 *  and the old index stays. */
object MongoIndex extends Logging {

  sealed trait Outcome { def inPlace: Boolean }
  object Outcome {
    /** Created, or already there with the wanted options. */
    case object Present extends Outcome { val inPlace = true }
    /** A plain index of the same keys was converted to unique in place. */
    case object ConvertedToUnique extends Outcome { val inPlace = true }
    /** The wanted index is not in place; whatever index was there is left untouched. */
    final case class NotInPlace(reason: String) extends Outcome { val inPlace = false }
  }

  private val IndexOptionsConflict       = 85
  private val IndexKeySpecsConflict      = 86
  private val Unauthorized               = 13
  private val CannotConvertIndexToUnique = 359
  private val Raced                      = Set(125 /* CommandFailed */, 72 /* InvalidOptions */)
  private val ConversionAttempts         = 5
  private[services] val RaceBackoffMillis = 200L
  private val Timeout                    = 30.seconds

  /** Ensure `collection` in `database` carries an index on `keys` with `options`.
   *  `label` names the caller in the log lines. Never throws. */
  def ensure(database: MongoDatabase, collection: String, keys: Bson, options: IndexOptions, label: String): Outcome = {
    val ns = s"${database.name}.$collection"
    Try(Await.result(database.getCollection[Document](collection).createIndex(keys, options).toFuture(), Timeout)) match {
      case Success(_) => Outcome.Present
      case Failure(conflict: MongoCommandException)
          if conflict.getErrorCode == IndexKeySpecsConflict || conflict.getErrorCode == IndexOptionsConflict =>
        reconcile(database, collection, ns, keys.toBsonDocument, options, label)
      case Failure(exception) =>
        logger.warn(s"$label: index on $ns ${keys.toBsonDocument.toJson} could not be created: ${exception.getMessage}")
        Outcome.NotInPlace(exception.getMessage)
    }
  }

  private def reconcile(database: MongoDatabase, collection: String, ns: String,
                        keys: BsonDocument, options: IndexOptions, label: String): Outcome = {
    val wantedName = Option(options.getName).getOrElse(defaultName(keys))
    val wantedKeys = normalisedKeys(keys)
    Try(Await.result(database.getCollection[Document](collection).listIndexes().toFuture(), Timeout).map(_.toBsonDocument)) match {
      case Failure(exception) =>
        logger.warn(s"$label: index `$wantedName` on $ns conflicts with an existing one, which could not be read: ${exception.getMessage}")
        Outcome.NotInPlace(exception.getMessage)
      case Success(indexes) =>
        val sameKeys = indexes.filter(index => Option(index.get("key")).collect { case key: BsonDocument => normalisedKeys(key) }.contains(wantedKeys))
        sameKeys.find(_.getString("name").getValue == wantedName).orElse(sameKeys.headOption) match {
          case None =>
            val reason = s"an index named `$wantedName` exists on other keys"
            logger.error(s"$label: cannot build `$wantedName` ${keys.toJson} on $ns — $reason; it is left as it is.")
            Outcome.NotInPlace(reason)
          case Some(existing) =>
            val existingUnique = existing.getBoolean("unique", org.bson.BsonBoolean.FALSE).getValue
            if (comparable(existing) != wantedComparable(options)) {
              val reason = s"the existing index ${existing.toJson} differs in more than uniqueness"
              logger.error(s"$label: cannot build `$wantedName` on $ns in place — $reason; the old index is kept. Rebuild it by hand.")
              Outcome.NotInPlace(reason)
            } else if (existingUnique == options.isUnique) Outcome.Present   // the same spec under another name (85)
            else if (options.isUnique) convertToUnique(database, collection, ns, existing.getString("name").getValue, label)
            else {
              val reason = s"the existing index ${existing.toJson} is unique and a plain one is wanted"
              logger.error(s"$label: cannot build `$wantedName` on $ns — $reason; the unique index is kept.")
              Outcome.NotInPlace(reason)
            }
        }
    }
  }

  /** Run `convert`, retrying a race with another pod. Two pods booting together race their
   *  `collMod`s on one index: mongod answers the loser with CommandFailed (125, "modified by another
   *  thread"), or with InvalidOptions (72) when the other pod's failed conversion cleared
   *  `prepareUnique` between this one's two steps. Both are retried after a growing `pause` (in
   *  millis) — back to back, all the attempts landed while the other pod was still mid-conversion;
   *  one the other pod already won (`alreadyWon`) is a success. */
  private[services] def retryRaced(attempts: Int, pause: Long => Unit)(convert: () => Unit, alreadyWon: () => Boolean): Try[Unit] = {
    @scala.annotation.tailrec
    def attempt(number: Int): Try[Unit] =
      Try(convert()) match {
        case Failure(raced: MongoCommandException) if Raced(raced.getErrorCode) =>
          if (alreadyWon()) Success(())
          else if (number < attempts) { pause(RaceBackoffMillis * number); attempt(number + 1) }
          else Failure(raced)
        case other => other
      }
    attempt(1)
  }

  /** Back to exactly the index that was there after a conversion the duplicates refused:
   *  `prepareUnique` alone would start refusing writes that a plain index accepts, which is a
   *  decision for whoever dedupes the rows. A rollback that fails leaves the index doing exactly
   *  that, so it is named in the `reason` the ERROR line and the outcome carry, never dropped. */
  private[services] def afterRollback(reason: String, rollback: () => Unit): String =
    Try(rollback()) match {
      case Success(_)         => reason
      case Failure(exception) =>
        s"$reason, and clearing prepareUnique failed (${exception.getMessage}) — the index now REFUSES new duplicate writes " +
          "until `collMod` sets prepareUnique back to false"
    }

  private def convertToUnique(database: MongoDatabase, collection: String, ns: String, index: String, label: String): Outcome = {
    def collMod(flag: String, value: Boolean) =
      Await.result(database.runCommand(ImmutableDocument(
        "collMod" -> collection, "index" -> ImmutableDocument("name" -> index, flag -> value))).toFuture(), Timeout)
    def uniqueNow: Boolean =
      Try(Await.result(database.getCollection[Document](collection).listIndexes().toFuture(), Timeout)).toOption.exists(_.exists { spec =>
        spec.get("name").exists(_.asString.getValue == index) && spec.get("unique").exists(_.asBoolean.getValue)
      })
    retryRaced(ConversionAttempts, Thread.sleep)(
      () => { collMod("prepareUnique", value = true); collMod("unique", value = true) }, () => uniqueNow) match {
      case Success(_) =>
        logger.info(s"$label: converted the plain index `$index` on $ns to unique in place.")
        Outcome.ConvertedToUnique
      case Failure(duplicates: MongoCommandException) if duplicates.getErrorCode == CannotConvertIndexToUnique =>
        val groups = Option(duplicates.getResponse.get("violations")).collect { case array: BsonArray => array.getValues.asScala.toSeq }.getOrElse(Nil)
        val documents = groups.map(_.asDocument.getArray("ids", new BsonArray()).size).sum
        val reason = afterRollback(s"${groups.size} key value(s) are held by $documents documents",
          () => { collMod("prepareUnique", value = false); () })
        logger.error(s"$label: cannot make `$index` on $ns unique — $reason. The non-unique index is kept; " +
          "remove the duplicates and the next boot converts it.")
        Outcome.NotInPlace(reason)
      case Failure(denied: MongoCommandException) if denied.getErrorCode == Unauthorized =>
        val reason = "this credential may not run collMod"
        logger.error(s"$label: cannot convert `$index` on $ns to unique — $reason (`readWrite` does not grant it). " +
          s"The non-unique index is kept; grant collMod on ${database.name} or run the two collMods by hand.")
        Outcome.NotInPlace(reason)
      case Failure(exception) =>
        logger.error(s"$label: converting `$index` on $ns to unique failed; the non-unique index is kept: ${exception.getMessage}")
        Outcome.NotInPlace(exception.getMessage)
    }
  }

  /** Mongo's own default name: `field_1_other_-1`. */
  private def defaultName(keys: BsonDocument): String =
    keys.entrySet.asScala.map(entry => s"${entry.getKey}_${render(entry.getValue)}").mkString("_")

  private def render(value: BsonValue): String = value match {
    case number: BsonNumber if number.isInt32 || number.isInt64 => number.longValue.toString
    case number: BsonNumber                                     => number.doubleValue.toString
    case other if other.isString                                => other.asString.getValue
    case other                                                  => other.toString
  }

  /** The options an index spec carries that matter here, uniqueness aside. */
  private val Ignored = Set("v", "key", "name", "ns", "background", "unique", "prepareUnique")

  private def comparable(index: BsonDocument): Map[String, BsonValue] =
    index.entrySet.asScala.collect {
      case entry if !Ignored(entry.getKey) => entry.getKey -> normalised(entry.getValue)
    }.toMap.filterNot { case (key, value) => key == "sparse" && value == org.bson.BsonBoolean.FALSE }

  private def wantedComparable(options: IndexOptions): Map[String, BsonValue] =
    Seq(
      Option.when(options.isSparse)("sparse" -> (org.bson.BsonBoolean.TRUE: BsonValue)),
      Option(options.getPartialFilterExpression).map(filter => "partialFilterExpression" -> (filter.toBsonDocument: BsonValue)),
      Option(options.getExpireAfter(TimeUnit.SECONDS)).map(seconds => "expireAfterSeconds" -> normalised(new BsonInt64(seconds)))
    ).flatten.toMap

  /** A whole number in whichever BSON width it was stored: `1`, `1L` and `1.0` alike. */
  private def normalised(value: BsonValue): BsonValue = value match {
    case number: BsonNumber if number.doubleValue == number.longValue.toDouble => new BsonInt64(number.longValue)
    case other                                                                 => other
  }

  private def normalisedKeys(keys: BsonDocument): Seq[(String, BsonValue)] =
    keys.entrySet.asScala.toSeq.map(entry => entry.getKey -> normalised(entry.getValue))
}
