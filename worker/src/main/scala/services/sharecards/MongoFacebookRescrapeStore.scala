package services.sharecards

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{FindOneAndUpdateOptions, ReturnDocument, UpdateOptions}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Indexes, Sorts, Updates}
import play.api.Logging
import tools.MongoErrors
import services.facebook.MongoFacebookSlots
import java.time.Instant
import java.util.Date
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/**
 * The store in one Mongo collection of the fleet database: an entry per key (`_id`), and the
 * quota as the one document `_id: "quota"`. `add` is an upsert that only sets on insert, so an
 * entry already waiting is untouched; `takeSlot` is one conditional upsert — a slot not yet due
 * fails the filter, and the upsert's insert then collides with the quota's `_id`.
 */
class MongoFacebookRescrapeStore(collection: MongoCollection[Document]) extends FacebookRescrapeStore with Logging {
  import MongoFacebookRescrapeStore.*

  locally {
    val thread = new Thread(() => {
      // Equality, sort, range: `claim` asks for the OLDEST due entry, so `enqueuedAt` comes before
      // the `notBefore` range — that way the index walk is already in claim order and stops at the
      // first due key. With the range first (the index's first shape), every due entry was fetched
      // and sorted in memory on every claim.
      Try(Await.result(collection.createIndex(
        Indexes.ascending("country", "kind", "enqueuedAt", "notBefore")).toFuture(), Timeout))
        .fold(e => logger.warn(s"facebook_rescrapes index creation failed: ${e.getMessage}"),
              // The range-first order it replaced, kept by nothing: dropped only once the claim order is there.
              _ => tools.MongoIndex.dropSuperseded(collection, SupersededClaimIndex, "facebook_rescrapes"))
    }, "facebook-rescrapes-init")
    thread.setDaemon(true)
    thread.start()
  }

  private def await[T](f: scala.concurrent.Future[T]): T = Await.result(f, Timeout)
  private def date(at: Instant) = new Date(at.toEpochMilli)

  def add(entries: Seq[RescrapeEntry]): Int = entries.count { entry =>
    val target = entry.target match {
      case RescrapeTarget.FilmPages(_, filmId) => Updates.setOnInsert("filmId", filmId)
      case RescrapeTarget.Page(url, _)         => Updates.setOnInsert("url", url)
    }
    await(collection.updateOne(Filters.eq("_id", entry.key), Updates.combine(
      Updates.setOnInsert("country", entry.country), kindOf(entry.kind), target,
      Updates.max("notBefore", date(entry.notBefore)), Updates.setOnInsert("enqueuedAt", date(entry.notBefore)),
      Updates.setOnInsert("attempts", 0), Updates.max(AskedAgainAt, date(entry.notBefore))),
      new UpdateOptions().upsert(true)).toFuture()).getUpsertedId != null
  }

  // A page asked for as previewed is one from then on, whatever it waited as: Facebook fetched it
  // since it was queued. Never the other way — a preview does not go away with one request.
  private def kindOf(kind: RescrapeKind) =
    if (kind == RescrapeKind.Page) Updates.set("kind", kind.name) else Updates.setOnInsert("kind", kind.name)

  private def due(country: String, kind: RescrapeKind, now: Instant) =
    Filters.and(Filters.eq("country", country), Filters.eq("kind", kind.name), Filters.lte("notBefore", date(now)))

  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean =
    await(collection.find(due(country, kind, now)).limit(1).toFuture()).nonEmpty

  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry] =
    Option(await(collection.findOneAndUpdate(due(country, kind, now),
      Updates.combine(Updates.set("notBefore", date(now.plusMillis(lease.toMillis))), Updates.inc("attempts", 1), Updates.unset(AskedAgainAt)),
      new FindOneAndUpdateOptions().sort(Sorts.ascending("enqueuedAt")).returnDocument(ReturnDocument.AFTER)).toFutureOption()).orNull)
      .map(entryOf)

  private def claimed(entry: RescrapeEntry) = Filters.and(Filters.eq("_id", entry.key), Filters.eq("attempts", entry.attempts))

  // `askedAgainAt` is the latest due time asked for since the entry was last claimed (set by every
  // `add`, cleared by `claim`): present at completion, the entry waits again instead of going.
  def complete(entry: RescrapeEntry): Unit =
    if (await(collection.deleteOne(Filters.and(claimed(entry), Filters.exists(AskedAgainAt, false))).toFuture()).getDeletedCount == 0) {
      await(collection.updateOne(Filters.and(claimed(entry), Filters.exists(AskedAgainAt)), Seq(
        Document("$set" -> Document("notBefore" -> s"$$$AskedAgainAt", "attempts" -> 0)),
        Document("$unset" -> AskedAgainAt))).toFuture())
      ()
    }

  def retry(entry: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit = {
    val due = Updates.set("notBefore", date(at))
    await(collection.updateOne(claimed(entry), if (countAttempt) due else Updates.combine(due, Updates.inc("attempts", -1))).toFuture())
    ()
  }

  // An update PIPELINE, so the next slot is computed from the one taken, in the same write:
  // nextSlot = max(nextSlot, now - slack) + spacing. On the first insert `$nextSlot` is missing,
  // which `$max` ranks below any date.
  def takeSlot(now: Instant, spacing: FiniteDuration, slack: FiniteDuration): Boolean =
    Try(await(collection.updateOne(
      Filters.and(Filters.eq("_id", MongoFacebookSlots.QuotaId), Filters.lte("nextSlot", date(now))),
      Seq(Document("$set" -> Document("nextSlot" -> Document("$add" -> BsonArray(
        Document("$max" -> BsonArray(BsonString("$nextSlot"), BsonDateTime(now.toEpochMilli - slack.toMillis))),
        BsonInt64(spacing.toMillis)))))),
      new UpdateOptions().upsert(true)).toFuture())) match {
      case Success(_)                                                            => true
      case Failure(e: MongoWriteException) if MongoErrors.isDuplicateKey(e)      => false
      case Failure(e)                                                            => throw e
    }

  private val slots = new MongoFacebookSlots(collection)
  def holdSlots(until: Instant): Unit = slots.holdSlots(until)

  def waitingPages(country: String): Long =
    await(collection.countDocuments(Filters.and(Filters.eq("country", country), Filters.in("kind", RescrapeKind.Pages.map(_.name)*))).toFuture())
}

object MongoFacebookRescrapeStore {
  /** A waiting entry's document, as an `add` inserts it and a claim leaves it. */
  def entryDocument(entry: RescrapeEntry, enqueuedAt: Instant): Document = {
    val target = entry.target match {
      case RescrapeTarget.FilmPages(_, filmId) => Document("filmId" -> filmId)
      case RescrapeTarget.Page(url, _)         => Document("url" -> url)
    }
    Document("_id" -> entry.key, "country" -> entry.country, "kind" -> entry.kind.name, "notBefore" -> new Date(entry.notBefore.toEpochMilli),
      "enqueuedAt" -> new Date(enqueuedAt.toEpochMilli), "attempts" -> entry.attempts) ++ target
  }

  /** When a stored entry was enqueued. */
  def enqueuedAtOf(d: Document): Instant = d.get("enqueuedAt").map(v => Instant.ofEpochMilli(v.asDateTime().getValue)).getOrElse(Instant.EPOCH)

  def entryOf(d: Document): RescrapeEntry = {
    def text(field: String) = d.get(field).map(_.asString().getValue).getOrElse("")
    val country = text("country")
    val target  = if (text("kind") == RescrapeKind.Film.name) RescrapeTarget.FilmPages(country, text("filmId"))
                  else RescrapeTarget.Page(text("url"), previewed = text("kind") != RescrapeKind.UnfetchedPage.name)
    RescrapeEntry(country, target, d.get("notBefore").map(v => Instant.ofEpochMilli(v.asDateTime().getValue)).getOrElse(Instant.EPOCH),
      d.get("attempts").map(_.asInt32().getValue).getOrElse(0))
  }

  private val AskedAgainAt = "askedAgainAt"
  private val Timeout = 10.seconds
  /** The claim index's first shape, the `notBefore` range before `enqueuedAt`'s sort. */
  private val SupersededClaimIndex = "country_1_kind_1_notBefore_1_enqueuedAt_1"
}
