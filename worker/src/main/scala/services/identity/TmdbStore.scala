package services.identity

import org.mongodb.scala.bson.BsonArray
import org.bson.{BsonBoolean, BsonDocument, BsonDouble, BsonInt32, BsonInt64, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{BulkWriteOptions, DeleteOneModel, Filters, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.libs.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** The kinds of document the normalized TMDB store keeps, each its own collection. */
enum TmdbKind(val collection: String, val answerFields: Option[Seq[String]]) {
  /** A film: its record's two partial responses, the record parsed from them, and its hit fields. An
   *  answer reads the record or the hit, and of the partials (~650 of ~1,000 bytes on UK, the write path's,
   *  which re-parses the record when one changes) only the IMDb id: a record filed before records carried
   *  its number has none ([[StoredTmdbLookups]] takes it from here). */
  case Film   extends TmdbKind("tmdb_films", Some(Seq("record", "hit", "local.imdb_id", "english.imdb_id")))
  /** A person: the films they are credited as directing and as writing. */
  case Person extends TmdbKind("tmdb_people", None)
  /** A question: a title search's or person search's ranked ids, a find's films, IMDb's suggestions. */
  case Query  extends TmdbKind("tmdb_queries", None)
  /** What another film database FAMILY answered ([[FamilyAnswerStore]]): its title and person searches' films, and its
   *  records of films — kept long, since a released film's facts do not move. */
  case Family extends TmdbKind("identity_family_answers", None)
}

/** Where the normalized documents live: the storage seam, and nothing else. Every rule — what a
 *  response becomes, when a document changed, what a question reads — is [[TmdbStore]]'s. */
trait TmdbDocuments {
  /** These documents, by id — any number of ids: a store batches its own reads. */
  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument]
  /** These documents as an ANSWER reads them: only `kind`'s [[TmdbKind.answerFields]] — a dotted one as a Mongo
   *  projection gives it, the sub-document (empty when it lacks the field) holding only that field. A store that
   *  can leave the rest on the server does. */
  def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = kind.answerFields.fold(get(kind, ids)) { fields =>
    get(kind, ids).view.mapValues { d =>
      val kept = new BsonDocument()
      fields.foreach(_.split('.') match {
        case Array(field)        => Option(d.get(field)).foreach(kept.put(field, _))
        case Array(field, inner) => Option(d.get(field)).filter(_.isDocument).map(_.asDocument).foreach { sub =>
          val projected = Option(kept.get(field)).map(_.asDocument).getOrElse(new BsonDocument())
          Option(sub.get(inner)).foreach(projected.put(inner, _))
          kept.put(field, projected)
        }
        case _                   => ()
      })
      kept
    }.toMap
  }
  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit
}

/** The storage half of the store's retention ([[TmdbStoreSweep]] holds its rules): which documents
 *  were last fetched long ago, and deleting one only while it still carries the stamp that was read —
 *  so a document re-fetched after the scan is kept. Apart from [[TmdbDocuments]]: only the sweep asks. */
trait TmdbDocumentRetention {
  /** Every document of `kind` whose [[TmdbStore.FetchedAt]] is before `cutoff` (epoch millis), with that
   *  stamp; one carrying no stamp is never named. Throws when the scan cannot be completed. */
  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)]
  /** Delete each of `stamped` whose stamp is still the one given; how many were deleted. */
  def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int
}

object TmdbDocuments {
  /** How many of one read's batches are in flight at once — each holds one of the worker's pooled
   *  connections, so a take-up's reads overlap without taking the pool from everything else. */
  val InFlight = 4

  /** `fetch` each batch, at most [[InFlight]] at a time, and every result together: a read of many
   *  batches waits on its round-trips side by side, not one after another. */
  def inBatches[A](batches: Seq[Seq[String]], timeout: FiniteDuration)(fetch: Seq[String] => scala.concurrent.Future[Seq[A]]): Seq[A] =
    batches.grouped(InFlight).toSeq.flatMap { group =>
      Await.result(scala.concurrent.Future.sequence(group.map(fetch))(using implicitly, scala.concurrent.ExecutionContext.parasitic), timeout)
        .flatten
    }
}

final class InMemoryTmdbDocuments extends TmdbDocuments with TmdbDocumentRetention {
  private val byKind = TmdbKind.values.map(_ -> new ConcurrentHashMap[String, BsonDocument]()).toMap
  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] =
    byKind(kind).asScala.toSeq.flatMap { case (id, d) => TmdbStore.fetchedAt(d).filter(_ < cutoff).map(id -> _) }
  def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int = stamped.count { case (id, at) =>
    var deleted = false
    byKind(kind).computeIfPresent(id, (_, d) => if (TmdbStore.fetchedAt(d).contains(at)) { deleted = true; null } else d)
    deleted
  }
  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] =
    ids.flatMap(id => Option(byKind(kind).get(id)).map(id -> _.clone())).toMap
  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = docs.foreach { case (id, d) => byKind(kind).put(id, d.clone()) }
  def size(kind: TmdbKind): Int = byKind(kind).size
}

/** `tmdb_films` / `tmdb_people` / `tmdb_queries`: one document per entity, `_id` its key. */
final class MongoTmdbDocuments(db: MongoDatabase) extends TmdbDocuments with TmdbDocumentRetention {
  private val Timeout = 30.seconds
  private val Batch   = 500
  private def coll(kind: TmdbKind): MongoCollection[BsonDocument] = db.getCollection[BsonDocument](kind.collection)

  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = read(kind, ids, None)

  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = read(kind, ids, kind.answerFields)

  private def read(kind: TmdbKind, ids: Seq[String], fields: Option[Seq[String]]): Map[String, BsonDocument] =
    TmdbDocuments.inBatches(ids.distinct.grouped(Batch).toSeq, Timeout) { batch =>
      val found = coll(kind).find(Filters.in("_id", batch*))
      fields.fold(found)(f => found.projection(org.mongodb.scala.model.Projections.include(f*)))
        .batchSize(tools.MongoReplies.Default).toFuture().map(_.map { d =>
        val id = d.getString("_id").getValue
        d.remove("_id")
        id -> d
      })(using scala.concurrent.ExecutionContext.parasitic)
    }.toMap

  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] =
    services.retention.StampedRows.scanBefore(coll(kind), Filters.lt(TmdbStore.FetchedAt, cutoff), TmdbStore.FetchedAt)(
      _.getString("_id").getValue)(TmdbStore.fetchedAt)

  def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int = stamped.grouped(Batch).map { batch =>
    Await.result(coll(kind).bulkWrite(batch.map { case (id, at) =>
      DeleteOneModel(Filters.and(Filters.equal("_id", id), Filters.equal(TmdbStore.FetchedAt, BsonInt64(at))))
    }, BulkWriteOptions().ordered(false)).toFuture(), Timeout).getDeletedCount
  }.sum

  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = docs.grouped(Batch).foreach { batch =>
    Await.result(coll(kind).bulkWrite(batch.map { case (id, d) =>
      ReplaceOneModel(Filters.equal("_id", id), d.clone().append("_id", BsonString(id)), ReplaceOptions().upsert(true))
    }, BulkWriteOptions().ordered(false)).toFuture(), Timeout)
    ()
  }
}

/**
 * TMDB's and IMDb's answers to the identity model's questions, normalized: a film once by its id,
 * a person once by theirs, and a question as the ids it named — native BSON holding only what the
 * resolver reads (the record fields `TmdbFilmRecord` parses, each hit's title/original/year and
 * popularity BUCKET). Written as responses arrive ([[TmdbNormalizer]], wherever the pipeline's or
 * the fill's client fetches), so no raw body is kept and nothing is parsed twice.
 *
 * A document is rewritten, and its key announced to [[onChanged]], only when its VALUE changes: a
 * re-fetch whose only news is TMDB's daily popularity or vote drift (54 of 60 sampled UK re-fetches)
 * changes nothing the model reads, and wakes nothing.
 */
final class TmdbStore(docs: TmdbDocuments, clock: java.time.Clock) {
  import TmdbStore._

  private val listeners = new java.util.concurrent.CopyOnWriteArrayList[String => Unit]()
  /** Call `listener` with a document's key ([[keyOf]]) whenever its value changes. */
  def onChanged(listener: String => Unit): Unit = { listeners.add(listener); () }

  // ── writes: one document's new value, and whether it moved ─────────────────────────

  private def update(kind: TmdbKind, id: String)(change: Option[BsonDocument] => BsonDocument): Unit =
    updateAll(kind, Seq(id))((_, before) => change(before))

  // Each write reads its documents and writes them back, so two writes of ONE document must not
  // interleave (a film's two partials would lose one) — but writes of different documents need not
  // wait for each other: a take-up of an empty store files every film it names from the prefetch's
  // threads, and one store-wide lock made those round-trips serial. So a lock per document, striped.
  private val locks = new StripedLocks()

  /** `change` each of `ids`' documents; write, and announce, only those whose value moved — in one
   *  read and one write however many there are. One whose value did not move is only re-stamped
   *  (`fetchedAt`), at most once per [[TmdbStore.RenewEvery]], and announced to no one. Announced once
   *  written and unlocked, so a listener never runs holding a document's lock. */
  private def updateAll(kind: TmdbKind, ids: Seq[String], renew: Boolean = true)(change: (String, Option[BsonDocument]) => BsonDocument): Unit = {
    val distinct = ids.distinct
    val moved = locks.locking(distinct.map(keyOf(kind, _))) {
      val now     = clock.millis()
      val before  = docs.get(kind, distinct)
      val written = distinct.flatMap { id =>
        def value = before.get(id).map { d => val c = d.clone(); Stamps.foreach(c.remove); c }
        val after = change(id, value)   // `change` may edit what it is given: compare with a fresh copy
        if (!value.contains(after)) Some((id, after.append(ChangedAt, BsonInt64(now)).append(FetchedAt, BsonInt64(now)), true))
        else before.get(id).filter(d => renew && fetchedAt(d).forall(_ < now - RenewEvery.toMillis))
          .map(d => (id, d.clone().append(FetchedAt, BsonInt64(now)), false))
      }
      if (written.nonEmpty) docs.put(kind, written.map { case (id, d, _) => id -> d })
      written.collect { case (id, _, true) => id }
    }
    moved.foreach(id => listeners.forEach(_(keyOf(kind, id))))
  }

  /** A title search's (or find's) films, in its order, each hit's fields kept on its film. */
  private[identity] def question(id: String, films: Seq[Hit]): Unit = {
    hitsSeen(films)
    update(TmdbKind.Query, id)(_ => new BsonDocument("ids", ints(films.map(_.tmdbId))))
  }

  /** A person search's people, in the order the walk tries them. */
  private[identity] def people(id: String, persons: Seq[Int]): Unit =
    update(TmdbKind.Query, id)(_ => new BsonDocument("ids", ints(persons)))

  /** IMDb's movie suggestions for a title, as `ImdbClient` reads them. */
  private[identity] def suggestions(id: String, entries: Seq[services.enrichment.ImdbClient.Suggestion]): Unit =
    update(TmdbKind.Query, id)(_ => new BsonDocument("suggestions", BsonArray.fromIterable(entries.map { s =>
      val d = new BsonDocument("id", BsonString(s.id)).append("rank", BsonInt32(s.rank))
      s.title.foreach(t => d.append("title", BsonString(t)))
      s.year.foreach(y => d.append("year", BsonInt32(y)))
      d
    })))

  /** IMDb's record of a title (`ImdbClient.identityRecord`) — `None`, filed as null, when IMDb has no such title: what a
   *  fallback candidate is scored on ([[FallbackIds]]). */
  private[identity] def imdbRecord(id: String, record: Option[IdentityMeasures.Film]): Unit =
    update(TmdbKind.Query, id)(_ => new BsonDocument("record", IdentityAnswerBson.film(record)))

  /** Every title IMDb lists one of its titles under (`ImdbClient.titlesOf`). */
  private[identity] def imdbTitles(id: String, titles: Seq[String]): Unit =
    update(TmdbKind.Query, id)(_ => new BsonDocument("titles", BsonArray.fromIterable(titles.map(BsonString(_)))))

  /** A person's directing and writing credits. */
  private[identity] def person(id: Int, directed: Seq[Hit], wrote: Seq[Hit]): Unit = {
    hitsSeen(directed ++ wrote)
    update(TmdbKind.Person, id.toString)(_ => new BsonDocument("directed", ints(directed.map(_.tmdbId))).append("wrote", ints(wrote.map(_.tmdbId))))
  }

  /** One of a film's two record responses — `Partial.Local` or `Partial.English` — as the minimal
   *  document `TmdbFilmRecord` reads; the record is re-parsed once both are known. */
  private[identity] def filmPartial(id: Int, partial: Partial, minimal: JsValue): Unit =
    update(TmdbKind.Film, id.toString) { before =>
      val d = before.getOrElse(new BsonDocument())
      d.put(partial.field, bsonOf(minimal))
      (Option(d.get(Partial.Local.field)), Option(d.get(Partial.English.field))) match {
        case (Some(local), Some(english)) =>
          val record = TmdbFilmRecord.parse(Seq(jsonOf(local), jsonOf(english))).map(_._1)
          d.put("record", IdentityAnswerBson.film(record))
          // A film with a record is read from it alone; a hit's fields only stand in until then.
          if (record.isDefined) d.remove("hit")
        case _ => d.remove("record")
      }
      d
    }

  /** A search or credit naming films: it files a hit where no record is held, and never renews a film it
   *  only named — it did not fetch the film's record, and renewing each named film once it was a day old
   *  made every re-asked question ~20 film writes (DE's whole fill pace, for records that had not moved). */
  private def hitsSeen(hits: Seq[Hit]): Unit = {
    val byId = hits.map(hit => hit.tmdbId.toString -> hit).toMap
    updateAll(TmdbKind.Film, hits.map(_.tmdbId.toString), renew = false) { (id, before) =>
      val d = before.getOrElse(new BsonDocument())
      if (Option(d.get("record")).exists(_.isDocument)) d else d.append("hit", hitDoc(byId(id)))
    }
  }

  // ── reads ─────────────────────────────────────────────────────────────────────────

  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = docs.get(kind, ids)
  /** What answers read of these documents ([[TmdbDocuments.answers]]). */
  def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = docs.answers(kind, ids)
}

object TmdbStore {
  /** When a document's value last moved (epoch millis): the watermark a restart replays after. */
  val ChangedAt = "changedAt"
  /** When TMDB last gave the document, changed or not (epoch millis, re-stamped at most once per
   *  [[RenewEvery]]): how old an answer is when the fill picks what to ask again. */
  val FetchedAt = "fetchedAt"
  /** How often an unchanged answer is re-stamped at most — each re-stamp is a write. */
  val RenewEvery: FiniteDuration = 1.day
  private val Stamps = Seq(ChangedAt, FetchedAt)
  def fetchedAt(d: BsonDocument): Option[Long] = Option(d.get(FetchedAt)).filter(_.isInt64).map(_.asInt64.getValue)

  /** A film's hit as the questions naming it read it: from its record once known, else the hit
   *  fields a search or credit gave. `None` when neither is held (the film was never named). */
  def filmHit(id: Int, film: BsonDocument): Option[Hit] =
    Option(film.get("record")).filter(_.isDocument).flatMap(IdentityAnswerBson.filmOf)
      .map(f => Hit(id, f.title, f.originalTitle, f.year, f.popularity.getOrElse(PopularityBucket.representative(PopularityBucket.of(0.0)))))
      .orElse(Option(film.get("hit")).map(h => hitOf(id, h.asDocument)))

  /** The key a document is announced and tracked under (`ObservationReads`). */
  def keyOf(kind: TmdbKind, id: String): String = s"${kind.collection}:$id"

  /** Which of a film's two record responses: the deployment language's `…?append_to_response=
   *  credits,release_dates`, or en-US `…?append_to_response=alternative_titles`. */
  enum Partial(val field: String) {
    case Local   extends Partial("local")
    case English extends Partial("english")
  }

  /** The document a candidate question is answered from first: its search, person search or IMDb
   *  suggestions — the one whose age says when it was last asked. */
  def questionId(language: String, query: CandidateQuery): String = query match {
    case CandidateQuery.Title(text)    => titleSearchId(language, text)
    case CandidateQuery.Director(name) => personSearchId(CandidateQuery.personName(name))
    case CandidateQuery.Imdb(title)    => suggestionsId(services.enrichment.ImdbClient.suggestionUrl(title))
    case CandidateQuery.ImdbTitled(t)  => suggestionsId(services.enrichment.ImdbClient.suggestionUrl(t))
  }

  // Question ids, by the parameters that decide their answer.
  def titleSearchId(language: String, query: String): String = s"movie|$language|$query"
  def personSearchId(query: String): String                  = s"person|$query"
  def findId(imdbId: String): String                         = s"find|$imdbId"
  def imdbTitlesId(imdbId: String): String                   = s"imdbtitles|$imdbId"
  def imdbRecordId(imdbId: String): String                   = s"imdbrecord|$imdbId"
  def suggestionsId(url: String): String                     = s"imdb|${url.stripPrefix(services.enrichment.ImdbClient.SuggestionBase)}"

  private def ints(values: Seq[Int]): BsonArray = BsonArray.fromIterable(values.map(BsonInt32(_)))
  def intsOf(value: BsonValue): Seq[Int] = value.asArray.getValues.asScala.toSeq.map(_.asInt32.getValue)

  /** A hit's fields as the resolver reads them — popularity as its bucket. */
  def hitDoc(hit: Hit): BsonDocument = {
    val d = new BsonDocument("title", BsonString(hit.title)).append("popularity", BsonInt32(PopularityBucket.of(hit.popularity)))
    hit.originalTitle.foreach(t => d.append("originalTitle", BsonString(t)))
    hit.year.foreach(y => d.append("year", BsonInt32(y)))
    d
  }
  def hitOf(id: Int, d: BsonDocument): Hit =
    Hit(id, d.getString("title").getValue, Option(d.get("originalTitle")).map(_.asString.getValue),
      Option(d.get("year")).map(_.asInt32.getValue), PopularityBucket.representative(d.getInt32("popularity").getValue))

  /** Plain JSON as BSON and back: the minimal record partials, whose shape is TMDB's. */
  def bsonOf(js: JsValue): BsonValue = js match {
    case JsObject(fields) => val d = new BsonDocument(); fields.foreach { case (k, v) => d.append(k, bsonOf(v)) }; d
    case JsArray(values)  => BsonArray.fromIterable(values.map(bsonOf))
    case JsString(s)      => BsonString(s)
    case JsNumber(n)      => if (n.isValidInt) BsonInt32(n.toInt) else if (n.isValidLong) BsonInt64(n.toLong) else BsonDouble(n.toDouble)
    case b: JsBoolean     => BsonBoolean(b.value)
    case JsNull           => BsonNull()
  }
  def jsonOf(value: BsonValue): JsValue =
    if (value.isDocument) JsObject(value.asDocument.entrySet.asScala.toSeq.map(e => e.getKey -> jsonOf(e.getValue)))
    else if (value.isArray) JsArray(value.asArray.getValues.asScala.toSeq.map(jsonOf))
    else if (value.isString) JsString(value.asString.getValue)
    else if (value.isInt32) JsNumber(value.asInt32.getValue)
    else if (value.isInt64) JsNumber(value.asInt64.getValue)
    else if (value.isDouble) JsNumber(BigDecimal(value.asDouble.getValue))
    else if (value.isBoolean) JsBoolean(value.asBoolean.getValue)
    else JsNull
}
