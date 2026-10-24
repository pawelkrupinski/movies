package services.identity.staging

import com.mongodb.client.model.{BulkWriteOptions, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Projections}
import play.api.libs.json.{Json, OFormat}

import java.time.{Clock, Instant}
import java.util.Date
import scala.collection.concurrent.TrieMap
import scala.concurrent.Await
import scala.concurrent.duration.*

/** One listing of a staged cluster, as its venue bills it. */
final case class StagedListing(venue: String, rawTitle: String)

/**
 * One question a staged cluster waits on, and what the worker's memory of asking it says (`QuestionMemory.outcome`):
 * `outcome` is one of [[StagedQuestion.Outcomes]] — `failed` with how many times in a row, when it is asked again
 * (epoch millis) and the host the failing request went to, when known.
 */
final case class StagedQuestion(kind: String, asker: String, text: String, outcome: String, attempts: Int = 0,
                                nextRetryAt: Option[Long] = None, host: Option[String] = None)

object StagedQuestion {
  val NotAttempted = "not-attempted"
  val Answered     = "answered"
  val Absent       = "absent"
  val Failed       = "failed"
  val Outcomes: Seq[String] = Seq(NotAttempted, Failed, Absent, Answered)
  given OFormat[StagedQuestion] = Json.format[StagedQuestion]
}

/** A cluster's take as the projection last wrote it: its film (and title, when the resolve read its record), basis and
 *  confidence; the film a no-match leans to; a fallback film's `source:id title`. */
final case class StagedTake(film: Option[Int], title: Option[String], basis: String, confidence: Double,
                            leaning: Option[Int] = None, leaningTitle: Option[String] = None, fallback: Option[String] = None)

object StagedTake {
  given OFormat[StagedTake] = Json.format[StagedTake]
}

/**
 * One listing cluster on the staging screen (docs/design/identity-questions.md, "The staging screen"): STAGING while
 * its take waits on an answer (`resolvedAt` empty), RECENTLY RESOLVED once it stopped waiting — `resolution` says how:
 * `settled` (the same cluster waits on nothing now) or `regrouped` (its listings moved to another cluster, or left).
 *
 * `listings` and `questions` are capped for the page; `listingCount`, `questionCount` and `attempted` (asked and come to
 * an answer, a durable non-answer or a failure: `QuestionMemory.attempted`) count them all. Times are epoch millis.
 */
final case class StagedCluster(key: String, listings: Seq[StagedListing], listingCount: Int, take: StagedTake,
                               modelGaps: Int, onTmdb: Boolean, questions: Seq[StagedQuestion], questionCount: Int,
                               attempted: Int, since: Long, resolvedAt: Option[Long] = None, resolution: Option[String] = None) {
  def staging: Boolean = resolvedAt.isEmpty
}

object StagedCluster {
  val Settled   = "settled"
  val Regrouped = "regrouped"
  given OFormat[StagedListing] = Json.format[StagedListing]
  given OFormat[StagedCluster] = Json.format[StagedCluster]
}

/** A country's snapshot as a whole, written on every publish: when (`at`, epoch millis — a stale one is a worker that
 *  stopped publishing), how many clusters stage (`staging`, of which `shown` are listed) and resolved lately, and the
 *  open questions of the staging clusters by kind. */
final case class StagingBeat(at: Long, staging: Int, shown: Int, resolved: Int, openByKind: Map[String, Int], modelGaps: Int)

object StagingBeat {
  given OFormat[StagingBeat] = Json.format[StagingBeat]
}

/** One country's part of a read: its beat, the clusters written since the reader's cursor, and the keys of every
 *  cluster it holds now (a key the reader holds and this lacks is gone). `cursor`: what the next read passes for it. */
final case class StagingCountry(country: String, beat: Option[StagingBeat], changed: Seq[StagedCluster], keys: Seq[String],
                                cursor: Option[Instant])

/** A stored document of the staging collection, as the store holds it: a cluster's or a country's beat. */
final case class StagingDoc(id: String, country: String, kind: String, updatedAt: Instant, body: String)

/** The index entry of a [[StagingDoc]]: everything but its body. */
final case class StagingEntry(id: String, country: String, kind: String, updatedAt: Instant)

/** Where the staging documents are kept: the narrow storage seam under [[IdentityStaging]], which owns every rule. */
trait StagingDocuments {
  def index(): Seq[StagingEntry]
  def bodies(ids: Seq[String]): Map[String, String]
  def put(docs: Seq[StagingDoc]): Unit
  def remove(ids: Seq[String]): Unit
}

/**
 * The staging screen's store: what each country's worker publishes of its identity stage's waits
 * (`IdentityStagingPublisher`) and what the web's `/admin/identity/staging` reads, every country at once. In the FLEET
 * database ([[services.FleetDatabase]]), which every country's worker and web already hold rights on — the waits live
 * in each worker's memory, not in its country's Mongo, so they are published here, one document per cluster.
 *
 * Bounded by its writer: a country's documents are its staging clusters (capped) and its recently resolved ones
 * (capped, aged out), and the publisher deletes every other one it wrote (`CollectionRetention`: `Sweep`).
 */
final class IdentityStaging(docs: StagingDocuments, clock: Clock) {
  import IdentityStaging.*

  /** Every cluster `country` holds, as last published: what its worker resumes from after a restart. */
  def ofCountry(country: String): Seq[StagedCluster] = {
    val ids = docs.index().filter(e => e.country == country && e.kind == ClusterKind).map(_.id)
    if (ids.isEmpty) Nil else docs.bodies(ids).values.toSeq.flatMap(body => Json.parse(body).asOpt[StagedCluster])
  }

  /** `country`'s clusters written or rewritten (`upserts`), those it holds no more (`removed`, by key), and its beat. */
  def publish(country: String, upserts: Seq[StagedCluster], removed: Iterable[String], beat: StagingBeat): Unit = {
    val now = clock.instant()
    if (removed.nonEmpty) docs.remove(removed.iterator.map(clusterId(country, _)).toSeq)
    docs.put(upserts.map(c => StagingDoc(clusterId(country, c.key), country, ClusterKind, now, Json.stringify(Json.toJson(c)))) :+
      StagingDoc(beatId(country), country, BeatKind, now, Json.stringify(Json.toJson(beat))))
  }

  /** Every country's part, its clusters written at or after its cursor in `since` (every one, without a cursor). At, not
   *  after: a publish whose writes straddle a read stamps all of them alike, and the rest arrive on the next read. */
  def read(since: Map[String, Instant]): Seq[StagingCountry] = {
    val byCountry = docs.index().groupBy(_.country)
    val wanted    = byCountry.toSeq.flatMap { case (country, entries) =>
      entries.filter(e => e.kind == BeatKind || since.get(country).forall(cursor => !e.updatedAt.isBefore(cursor))).map(_.id)
    }
    val bodies = if (wanted.isEmpty) Map.empty[String, String] else docs.bodies(wanted)
    byCountry.toSeq.sortBy(_._1).map { case (country, entries) =>
      val clusters = entries.filter(_.kind == ClusterKind)
      StagingCountry(country,
        beat    = bodies.get(beatId(country)).flatMap(Json.parse(_).asOpt[StagingBeat]),
        changed = clusters.flatMap(e => bodies.get(e.id)).flatMap(Json.parse(_).asOpt[StagedCluster]),
        keys    = clusters.map(e => keyOf(country, e.id)),
        cursor  = clusters.map(_.updatedAt).maxOption.orElse(since.get(country)))
    }
  }
}

object IdentityStaging {
  /** The collection, in the fleet database ([[services.FleetDatabase]]). */
  val Collection = "identity_staging"
  /** The store in `fleet`'s [[Collection]] (the fleet database: [[services.FleetDatabase]]); in memory without one. */
  def in(fleet: Option[org.mongodb.scala.MongoDatabase], clock: Clock): IdentityStaging =
    new IdentityStaging(fleet.fold[StagingDocuments](new InMemoryStagingDocuments)(db => new MongoStagingDocuments(db.getCollection(Collection))), clock)
  private val ClusterKind = "cluster"
  private val BeatKind    = "beat"
  private def clusterId(country: String, key: String): String = s"$country|c|$key"
  private def keyOf(country: String, id: String): String      = id.stripPrefix(s"$country|c|")
  private def beatId(country: String): String                 = s"$country|beat"
}

/** [[StagingDocuments]] in the fleet database's [[IdentityStaging.Collection]]. */
final class MongoStagingDocuments(collection: MongoCollection[Document]) extends StagingDocuments {
  import MongoStagingDocuments.*

  def index(): Seq[StagingEntry] =
    Await.result(collection.find().projection(Projections.exclude(Body)).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
      .flatMap(d => for {
        id      <- d.get("_id").map(_.asString.getValue)
        country <- d.get(Country).map(_.asString.getValue)
        kind    <- d.get(Kind).map(_.asString.getValue)
        at      <- d.get(UpdatedAt).map(v => Instant.ofEpochMilli(v.asDateTime.getValue))
      } yield StagingEntry(id, country, kind, at))

  def bodies(ids: Seq[String]): Map[String, String] =
    ids.distinct.grouped(Batch).flatMap { batch =>
      Await.result(collection.find(Filters.in("_id", batch*)).projection(Projections.include(Body))
        .batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
        .flatMap(d => for { id <- d.get("_id"); body <- d.get(Body) } yield id.asString.getValue -> body.asString.getValue)
    }.toMap

  def put(docs: Seq[StagingDoc]): Unit =
    docs.grouped(Batch).foreach { batch =>
      val writes = batch.map(doc => new ReplaceOneModel[Document](Filters.eq("_id", doc.id),
        Document("_id" -> doc.id, Country -> doc.country, Kind -> doc.kind, UpdatedAt -> new Date(doc.updatedAt.toEpochMilli), Body -> doc.body),
        new ReplaceOptions().upsert(true)))
      Await.result(collection.bulkWrite(writes, new BulkWriteOptions().ordered(false)).toFuture(), Timeout)
    }

  def remove(ids: Seq[String]): Unit =
    ids.distinct.grouped(Batch).foreach(batch => Await.result(collection.deleteMany(Filters.in("_id", batch*)).toFuture(), Timeout))
}

object MongoStagingDocuments {
  private val Country   = "country"
  private val Kind      = "kind"
  private val UpdatedAt = "updatedAt"
  private val Body      = "body"
  private val Timeout   = 10.seconds
  private val Batch     = 500
}

/** [[StagingDocuments]] in memory: a spec's, and a worker's or web's with no database. */
final class InMemoryStagingDocuments extends StagingDocuments {
  private val held = TrieMap.empty[String, StagingDoc]
  def index(): Seq[StagingEntry]                    = held.values.toSeq.map(d => StagingEntry(d.id, d.country, d.kind, d.updatedAt))
  def bodies(ids: Seq[String]): Map[String, String] = ids.flatMap(id => held.get(id).map(id -> _.body)).toMap
  def put(docs: Seq[StagingDoc]): Unit              = docs.foreach(d => held.update(d.id, d))
  def remove(ids: Seq[String]): Unit                = ids.foreach(held.remove)
}
