package services.readmodel

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument, OperationType}
import models.{CityScreening, ResolvedMovie}
import org.bson.{BsonDocumentReader, BsonTimestamp}
import org.bson.codecs.{Codec, DecoderContext}
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.{Aggregates, CountOptions, Filters, Indexes, Projections, ReplaceOneModel, Sorts}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import play.api.Logging
import services.movies.{KeysetScan, RepositoryWrite}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * MongoDB-backed read model. Persists the denormalised projection to two
 * collections — `web_movies` ([[ResolvedMovie]]) and `web_screenings`
 * ([[CityScreening]]) — and streams their changes (incl. deletes) to consumers.
 *
 * Both case classes carry their `_id` directly, so writes filter on `_id` and
 * change-stream delete events hand us that same id (no ObjectId→id mapping).
 * Neither collection carries a secondary index — every read here is by `_id`;
 * see the note above the collection handles.
 *
 * When `sharedDb` is `None` (Mongo disabled in local dev / tests without a
 * cluster) the repository is a silent no-op, mirroring `MongoMovieRepository`.
 */
class MongoReadModelRepository(
  sharedDb:             Option[MongoDatabase],
  // Cursor page size for the keyset-paged full scans (findAllMovies / findAllScreenings).
  // Injectable so tests force multiple pages with a handful of rows; see [[KeysetScan]].
  findAllBatchSize:     Int            = 500,
  findAllBatchAttempts: Int            = 4,
  findAllBatchBackoff:  FiniteDuration = 500.millis,
  // Counts each document a full scan skips as undecodable (see [[decodeTolerant]]).
  decodeFailures:       DecodeFailureMetrics = DecodeFailureMetrics.noop
) extends ReadModelReader with ReadModelWriter with Logging {

  // Relaxed write concern (w:1, j:false): the read model is a pure projection of
  // `movies` — every document is re-derived by the projector's reconcile, so a write
  // lost to a crash is self-healing. Skipping the journal sync cuts per-write cost
  // on the shared-CPU Mongo, which the worker's projection cascade was saturating.
  private val RelaxedWrites = WriteConcern.W1.withJournal(false)
  private val movies: Option[MongoCollection[ResolvedMovie]] =
    sharedDb.map(_.withCodecRegistry(ReadModelCodecs.registry).getCollection[ResolvedMovie](MongoReadModelRepository.MoviesCollection).withWriteConcern(RelaxedWrites))
  private val screenings: Option[MongoCollection[CityScreening]] =
    sharedDb.map(_.withCodecRegistry(ReadModelCodecs.registry).getCollection[CityScreening](MongoReadModelRepository.ScreeningsCollection).withWriteConcern(RelaxedWrites))

  // NO SECONDARY INDEXES ON `web_screenings`, deliberately. There used to be two, on `city`
  // and `filmId`, justified by "the web's per-city read filters on `city`; the projector's
  // per-film prune filters on `filmId`". Neither query exists: EVERY read in this class goes
  // through `_id` — the two full scans page by `Filters.gt("_id", …)` and the point
  // reads/writes use `Filters.eq("_id", …)` — and the prune filters `findAllScreeningRefs`
  // client-side rather than asking Mongo per film. `$indexStats` agreed: 0 operations against
  // either index over 73 hours of uptime, against 246,309 on `_id_`.
  //
  // An index nothing reads is not free. This is the collection the projector rewrites, so
  // each one was an extra index write on every upsert and delete, paid forever for a query
  // that was never written. Re-adding one is fine — but add the QUERY in the same commit, or
  // it will read as unused again to whoever measures next.

  def enabled: Boolean = movies.isDefined

  /** Test seam: the write concern configured on the derived collections. */
  def collectionWriteConcerns: Seq[WriteConcern] = Seq(movies, screenings).flatten.map(_.writeConcern)

  // ── Reads ─────────────────────────────────────────────────────────────────

  // A whole-collection decode goes through KeysetScan, NOT one unbounded `find().toFuture()`.
  // At corpus scale (~6.5k web_screenings) that single cursor timed out at 60s (and can
  // StackOverflow the async driver, Sentry KINOWO-19) and returned Seq.empty — which made the
  // projector's BOOT SEED empty, so every boot reproject saw all screenings as new and rewrote
  // the whole corpus (the reproject's phantom "did_work", ~6.5k writes per restart). Paged,
  // bounded, retried reads complete quickly. An incomplete scan is a FAILED read
  // (`ScanOutcome.collected`): the whole-collection reads throw on it, the checked ones answer it.
  //
  // Each page is read as raw BsonDocuments and decoded PER-DOCUMENT (see [[decodeTolerant]]),
  // so ONE malformed/legacy row (e.g. a `web_movies` doc missing the required `ratings`) is
  // skipped rather than sinking the whole keyset PAGE — a single-doc decode failure inside the
  // batch `find().toFuture()` would otherwise fail the page after its retries and drop up to
  // `findAllBatchSize` valid films. Keyset advances on the raw `_id` (always present), so a
  // skipped doc still moves the scan forward. Matches the change-stream apply path, which
  // already swallows per-doc decode failures.
  private def pagedFindAll[A: ClassTag](coll: Option[MongoCollection[A]], label: String): tools.ReadOutcome[Seq[A]] = {
    val buf = Vector.newBuilder[A]
    pagedForeach(coll, label)(buf += _).collected(buf.result())
  }

  /** Every document of `coll`, decoded and handed to `f` one keyset page at a time — never the
   *  whole collection at once — plus whether the scan reached the end. */
  private def pagedForeach[A: ClassTag](coll: Option[MongoCollection[A]], label: String,
                                        projection: Option[org.bson.conversions.Bson] = None)(f: A => Unit): tools.ScanOutcome =
    coll match {
      case Some(c) =>
        val codec = ReadModelCodecs.registry.get(implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]])
        KeysetScan.scan[BsonDocument](
          label          = label,
          batchSize      = findAllBatchSize,
          maxAttempts    = findAllBatchAttempts,
          initialBackoff = findAllBatchBackoff,
          keyOf          = _.getString("_id").getValue,
          fetchPage      = (afterId, limit) => {
            val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
            val found  = c.find[BsonDocument](filter)
            Await.result(projection.fold(found)(found.projection).sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(), 60.seconds)
          },
          onIncomplete   = exception =>
            logger.warn(s"$label keyset scan failed after retries: ${exception.getClass.getSimpleName}: ${exception.getMessage} — scan incomplete")
        )(batch => decodeTolerant(batch, codec, label, c.namespace.getCollectionName).foreach(f))
      case None => tools.ScanOutcome.complete
    }

  /** Decode a page of raw documents into `A`, SKIPPING (and logging with the `_id`) any that
   *  fail — one malformed/legacy document must sink only itself, never the whole keyset page.
   *  `private[readmodel]` so the tolerance is unit-testable without a live Mongo. */
  private[readmodel] def decodeTolerant[A](docs: Seq[BsonDocument], codec: Codec[A], label: String, collection: String): Seq[A] =
    docs.flatMap { doc =>
      Try(codec.decode(new BsonDocumentReader(doc), DecoderContext.builder().build())) match {
        case scala.util.Success(a) => Some(a)
        case scala.util.Failure(exception) =>
          val id = Try(doc.getString("_id").getValue).getOrElse("<unknown>")
          logger.warn(s"$label: skipping undecodable document _id=$id: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          decodeFailures.recordDecodeFailure(collection)
          None
      }
    }

  /** Throws when the read cannot be completed — read [[findAllMoviesChecked]] to branch on it. */
  def findAllMovies():     Seq[ResolvedMovie] = findAllMoviesChecked().required
  override def findAllMoviesChecked(): tools.ReadOutcome[Seq[ResolvedMovie]] =
    pagedFindAll(movies, "ReadModelRepository.findAllMovies")
  /** Throws when the read cannot be completed. */
  def findAllScreenings(): Seq[CityScreening] = pagedFindAll(screenings, "ReadModelRepository.findAllScreenings").required
  override def foreachScreening(f: CityScreening => Unit): tools.ScanOutcome =
    pagedForeach(screenings, "ReadModelRepository.foreachScreening")(f)
  override def foreachServedScreening(f: CityScreening => Unit): tools.ScanOutcome =
    pagedForeach(screenings, "ReadModelRepository.foreachServedScreening", Some(Projections.exclude(ServedScreening.WorkerOnlyFields*)))(f)

  // ── Id-only projections (the reconcile prune) ───────────────────────────────
  // The prune needs only ids/filmIds to spot orphaned documents; projecting them
  // server-side (read as BsonDocument, not the full case-class codec) keeps the
  // worker's 30-min reconcile from decoding the whole read model onto the heap —
  // the transient that, stacked on the resident corpus, exhausted the 320m heap.

  /**
   * Keyset-paged id projection — [[pagedFindAll]]'s shape, minus the payload decode.
   *
   * These read the same large collections `pagedFindAll` does and were the last
   * unpaged reads in this file. That is not merely a slow query: one unbounded
   * cursor over a big collection recurses the async driver's completion chain into
   * `StackOverflowError` on an I/O thread, which never reaches the caller's
   * `Await` — the read just times out with no cause, exactly as `movies` and
   * `screenings` did before [[KeysetScan]] existed. `web_screenings` is the
   * largest collection we hold (a country's showtimes, hundreds of thousands of
   * rows), so it is the likeliest of all of them to hit it.
   *
   * An incomplete scan is a FAILED read, never an empty one: empty reads to a heal as "every
   * card / venue is missing" and rewrites the corpus, so every caller branches on it.
   * It is logged loudly all the same, because the quiet version of this failure is a
   * prune that stops working and lets stale cards accumulate with nothing but a `warn`
   * to show for it.
   */
  private def pagedIdsChecked[A](
    collection: Option[MongoCollection[?]],
    label:      String,
    projection: org.bson.conversions.Bson
  )(decode: BsonDocument => A): tools.ReadOutcome[Seq[A]] = collection match {
    case Some(c) =>
      val buf = Vector.newBuilder[A]
      val complete = KeysetScan.scan[BsonDocument](
        label          = label,
        batchSize      = findAllBatchSize,
        maxAttempts    = findAllBatchAttempts,
        initialBackoff = findAllBatchBackoff,
        keyOf          = _.getString("_id").getValue,
        fetchPage      = (afterId, limit) => {
          val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
          Await.result(
            c.find[BsonDocument](filter).projection(projection).sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(),
            60.seconds)
        },
        onIncomplete   = exception =>
          logger.warn(s"$label keyset scan failed after retries: ${exception.getClass.getSimpleName}: " +
            s"${exception.getMessage} — the read fails; nothing is pruned or healed on it")
      )(batch => buf ++= batch.map(decode))
      complete.collected(buf.result())
    case None => tools.ReadOutcome.Answered(Seq.empty)
  }

  override def findAllMovieIdsChecked(): tools.ReadOutcome[Seq[String]] =
    pagedIdsChecked(movies, "ReadModelRepository.findAllMovieIds", Projections.include("_id"))(_.getString("_id").getValue)

  override def findAllScreeningRefsChecked(): tools.ReadOutcome[Seq[ScreeningRef]] =
    pagedIdsChecked(screenings, "ReadModelRepository.findAllScreeningRefs", Projections.include("_id", "filmId"))(d =>
      ScreeningRef(d.getString("_id").getValue, d.getString("filmId").getValue))

  override def findAllShareCardRefsChecked(): tools.ReadOutcome[Seq[ShareCardRef]] =
    pagedIdsChecked(movies, "ReadModelRepository.findAllShareCardRefs", Projections.include("_id", "shareCard"))(d =>
      ShareCardRef(d.getString("_id").getValue, Option(d.getString("shareCard", null)).map(_.getValue)))

  // Two reads by `_id`, so no secondary index is owed (see the note above the collection handles):
  // a screening's `_id` is `<card>|<city>|<cinema>`, so every row of one card sits in the `_id`
  // range [`<card>|`, `<card>}`) — `}` is the character after `|` — and a variant card's rows
  // (`<card>~<variant>|…`) sort after that range, never inside it. Decoded strictly: a document
  // that cannot be decoded fails the read rather than reading as absent.
  override def findCard(id: String): Option[StoredCard] = (movies, screenings) match {
    case (Some(movieColl), Some(screeningColl)) =>
      Try {
        val movie = Await.result(movieColl.find(Filters.eq("_id", id)).batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds).headOption
        val rows  = Await.result(
          screeningColl.find(Filters.and(Filters.gte("_id", s"$id|"), Filters.lt("_id", s"$id}"))).batchSize(tools.MongoReplies.Default).toFuture(), 30.seconds)
        StoredCard(movie, rows.filter(_.filmId == id))
      } match {
        case Success(card) => Some(card)
        case Failure(exception) =>
          logger.warn(s"ReadModelRepository.findCard($id) failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          None
      }
    case _ => None
  }

  // Server-side document counts — the read model's cheap integrity probe, so the web's
  // backstop can detect drift without re-reading the whole corpus. Hinted onto `_id`, a
  // count is a COUNT_SCAN of index keys; unhinted, an empty-filter `countDocuments` is a
  // COLLSCAN that reads every document (all of web_screenings, every 30 minutes, per pod).
  // Exact, unlike `estimatedDocumentCount`'s metadata, which an unclean shutdown leaves off
  // until the next validate: a wrong estimate would read as drift and reload every tick.
  def countMovies():     tools.ReadOutcome[Long] = count(movies, "countMovies")
  def countScreenings(): tools.ReadOutcome[Long] = count(screenings, "countScreenings")

  private def count[T](coll: Option[MongoCollection[T]], op: String): tools.ReadOutcome[Long] = coll match {
    case Some(c) =>
      val counted = tools.MongoRead(10.seconds)(c.countDocuments(Filters.empty(), CountOptions().hint(Indexes.ascending("_id"))).toFuture())
      counted match {
        case tools.ReadOutcome.Failed(cause) => logger.warn(s"ReadModelRepository.$op failed: ${cause.explain}")
        case _                 => ()
      }
      counted
    case None => tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new IllegalStateException(s"ReadModelRepository.$op: no read model configured")))
  }

  // ── Writes ──────────────────────────────────────────────────────────────────

  def upsertMovie(m: ResolvedMovie): Unit =
    replace(movies, m._id, m, "upsertMovie")

  def upsertScreening(s: CityScreening): Unit =
    replace(screenings, s._id, s, "upsertScreening")

  override def upsertScreenings(batch: Seq[CityScreening]): Unit =
    if (batch.sizeIs == 1) upsertScreening(batch.head)
    else if (batch.nonEmpty) screenings.foreach(c => write("upsertScreenings", s"${batch.size} screening(s) from ${batch.head._id}")(
      c.bulkWrite(batch.map(s => ReplaceOneModel(Filters.eq("_id", s._id), s, new ReplaceOptions().upsert(true)))).toFuture()))

  def deleteMovie(id: String): Unit     = removeById(movies, id, "deleteMovie")
  def deleteScreening(id: String): Unit = removeById(screenings, id, "deleteScreening")

  private def replace[T](coll: Option[MongoCollection[T]], id: String, document: T, op: String): Unit =
    coll.foreach(c => write(op, id)(c.replaceOne(Filters.eq("_id", id), document, new ReplaceOptions().upsert(true)).toFuture()))

  private def removeById[T](coll: Option[MongoCollection[T]], id: String, op: String): Unit =
    coll.foreach(c => write(op, id)(c.deleteOne(Filters.eq("_id", id)).toFuture()))

  /** Await one write, and THROW when it failed — see [[ReadModelWriter]]. Only the shutdown
   *  race (the client closed under an in-flight write) is swallowed: nothing will read the memo. */
  private def write(op: String, id: String)(body: => scala.concurrent.Future[?]): Unit =
    try { Await.result(body, 10.seconds); () }
    catch {
      case exception: Throwable if RepositoryWrite.isClientClosed(exception) => ()
      case exception: Throwable =>
        logger.warn(s"ReadModelRepository.$op($id) failed: ${exception.getMessage}")
        throw exception
    }

  // ── Change streams ──────────────────────────────────────────────────────────

  /** The server's `operationTime` on a `hello` — every write already applied is at or before
   *  it, so a stream started AT it replays each write a read made after this call could have
   *  missed. A standalone server reports no operation time (and cannot stream anyway): `None`.
   *
   *  The reply is read as a raw `BsonDocument`, which every codec registry decodes. Read as the
   *  Scala `Document` it needed the Scala driver's registry: on a database built from the Java
   *  `MongoClientSettings` defaults the decode threw ("The BsonCodec can only encode to Bson"),
   *  every checkpoint was `None`, and the watches started "from now" — whenever their cursors
   *  opened, so a write landing before that was in neither the hydrate nor the stream. */
  def streamCheckpoint(): Option[StreamCheckpoint] = sharedDb.flatMap { db =>
    Try(Await.result(db.runCommand[org.bson.BsonDocument](Document("hello" -> 1)).toFuture(), 10.seconds)) match {
      case Success(reply) =>
        Option(reply.get("operationTime")).filter(_.isTimestamp).map(time => StreamCheckpoint(time.asTimestamp.getValue))
      case Failure(exception) =>
        logger.warn(s"ReadModelRepository.streamCheckpoint failed, watching from now: ${exception.getMessage}")
        None
    }
  }

  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit, from: Option[StreamCheckpoint]): Option[StreamSubscription] =
    movies.map(watch(_, onUpsert, onDelete, MongoReadModelRepository.MoviesCollection, from, pipeline = Nil))

  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit, from: Option[StreamCheckpoint]): Option[StreamSubscription] =
    screenings.map(watch(_, onUpsert, onDelete, MongoReadModelRepository.ScreeningsCollection, from,
      pipeline = Seq(Aggregates.project(Projections.exclude(ServedScreening.WorkerOnlyFields.map(field => s"fullDocument.$field")*)))))

  /** Route each insert / update / replace to `onUpsert` (full post-image via
   *  `UPDATE_LOOKUP`) and each delete to `onDelete(_id)`. The driver auto-
   *  resumes across transient blips; a terminal error flips `live` to false so
   *  the caller's periodic reload takes over. Requires a replica set. `pipeline` runs on the server
   *  over each event — the screenings stream drops the post-image's worker-only fields there.
   *
   *  A document the codec refuses is SKIPPED and counted, never the end of the stream — see
   *  [[services.movies.ChangeEventDecoder]]: decoded inside the driver, one such document
   *  ended the cursor, and every later write waited on the periodic reload. */
  private def watch[T: ClassTag](coll: MongoCollection[T], onUpsert: T => Unit, onDelete: String => Unit, label: String,
                                 from: Option[StreamCheckpoint], pipeline: Seq[org.bson.conversions.Bson]): StreamSubscription = {
    val subRef = new AtomicReference[Subscription]()
    val alive  = new AtomicBoolean(true)
    val decoder = services.movies.ChangeEventDecoder.of[T](label, coll.codecRegistry, decodeFailures)
    val stream = coll.watch[org.bson.BsonDocument](pipeline).fullDocument(FullDocument.UPDATE_LOOKUP)
    from.fold(stream)(checkpoint => stream.startAtOperationTime(new BsonTimestamp(checkpoint.value)))
      .subscribe(new Observer[ChangeStreamDocument[org.bson.BsonDocument]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[org.bson.BsonDocument]): Unit = change.getOperationType match {
          case OperationType.DELETE =>
            Option(change.getDocumentKey).flatMap(k => Option(k.getString("_id")))
              .foreach(v => try onDelete(v.getValue) catch { case exception: Throwable => logger.warn(s"$label delete-apply failed: ${exception.getMessage}") })
          case _ =>
            decoder.postImage(change).presentOption
              .foreach(d => try onUpsert(d) catch { case exception: Throwable => logger.warn(s"$label upsert-apply failed: ${exception.getMessage}") })
        }
        override def onError(e: Throwable): Unit = {
          alive.set(false)
          logger.warn(s"$label change stream ended (${e.getMessage}) — relying on the periodic reload.")
        }
        override def onComplete(): Unit = alive.set(false)
      })
    logger.info(s"MongoReadModelRepository: watching $label change stream.")
    new StreamSubscription {
      override def live: Boolean  = alive.get()
      override def close(): Unit  = { alive.set(false); Option(subRef.get()).foreach(_.unsubscribe()) }
    }
  }

  // Shared MongoClient owned by `MongoConnection`; this repository doesn't close it.
  def close(): Unit = ()
}

object MongoReadModelRepository {
  /** The two derived collections. Named here rather than inline so
   *  [[services.DebugMirror]] can state what the local /debug mirror has to carry. */
  val MoviesCollection     = "web_movies"
  val ScreeningsCollection = "web_screenings"
}
