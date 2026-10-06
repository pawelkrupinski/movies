package services.scrapes

import com.mongodb.WriteConcern
import com.mongodb.client.model.UpdateOptions
import org.bson.{BsonDocument, BsonDocumentWriter, BsonString}
import org.bson.codecs.EncoderContext
import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.bson.codecs.configuration.CodecRegistry
import org.bson.codecs.configuration.CodecRegistries.{fromCodecs, fromProviders, fromRegistries}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.model.{Filters, Projections, Updates}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import services.PersistedCodecs
import services.movies.{JavaTimeCodecs, ShowtimeCodec}

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** Storage mirror of one `CinemaMovie`. Identical to the domain type minus
 *  `cinema` — every film in a row belongs to the row's cinema, so repeating it
 *  10-30 times per document would be pure waste, and `Cinema` is a sealed
 *  hierarchy the driver could not encode anyway. Re-attached on read from
 *  `Cinema.byDisplayName`. */
case class ArchivedFilmDto(
  movie:       Movie,
  posterUrl:   Option[String],
  filmUrl:     Option[String],
  synopsis:    Option[String],
  cast:        Seq[String],
  director:    Seq[String],
  showtimes:   Seq[Showtime],
  externalIds: Map[String, String],
  trailerUrl:  Option[String],
  ageRating:   Option[String],
  // `showtimes.##`, written with them: what a reader that wants the film without its showtimes — the identity
  // model's take-up, the projection's listing read — tells a change in them by, leaving them on the server. Absent
  // on a film written before it existed, whose reader reads the showtimes instead.
  showtimesDigest: Option[Int] = None
)

/** The newest attempt that produced nothing, stored beside the listing it failed
 *  to refresh. `outcome` is the wire label of a [[ScrapeOutcome]]. */
case class BarrenAttemptDto(
  at:      Instant,
  outcome: String,
  error:   Option[String],
  // When the current unbroken barren run began; absent on rows written before the
  // field existed, which decode as "since `at`".
  since:   Option[Instant] = None,
  // Consecutive failed runs; absent on rows written before the count existed,
  // which decode as an uncounted run.
  failedRuns: Option[Int] = None,
  // Written only when true, so a row from before the field existed — and every
  // unvouched empty or failure — decodes as false.
  noScheduleListed: Option[Boolean] = None
) {
  def toDomain: Option[BarrenAttempt] =
    ScrapeOutcome.byLabel(outcome).map(o => BarrenAttempt(at, o, error, since, failedRuns, noScheduleListed.contains(true)))
}

object BarrenAttemptDto {
  def from(b: BarrenAttempt): BarrenAttemptDto =
    BarrenAttemptDto(b.at, b.outcome.label, b.error, b.since, b.failedRuns, Option.when(b.noScheduleListed)(true))
}

/** Storage DTO for one cinema's archive row — the macro codec target for the
 *  `cinema_scrapes` collection. `_id` is the cinema's `displayName`, the same
 *  wire key every per-cinema row is stored under elsewhere.
 *
 *  `scrapedAt` / `listingComplete` / `films` describe the last scrape WITH
 *  content and are absent only on a row that has never had any — hence optional,
 *  so a barren-only row (a cinema failing since before the archive began) still
 *  decodes. */
case class StoredScrapeDto(
  _id:             String,
  city:            Option[String],
  scrapedAt:       Option[Instant],
  listingComplete: Option[Boolean],
  films:           Option[Seq[ArchivedFilmDto]],
  lastBarren:      Option[BarrenAttemptDto]
)

/** The projection the content census reads: a cinema, when it last produced films,
 *  and its barren marker's no-schedule flag. Its own DTO because decoding
 *  `StoredScrapeDto` would pull every archived film along with the one timestamp
 *  wanted. */
case class ContentStampDto(_id: String, scrapedAt: Option[Instant], lastBarren: Option[NoScheduleFlagDto]) {
  def toDomain: ContentStamp = ContentStamp(scrapedAt, lastBarren.exists(_.noScheduleListed.contains(true)))
}

/** `lastBarren` projected down to its one flag. */
case class NoScheduleFlagDto(noScheduleListed: Option[Boolean])

object StoredScrapeDto {

  def toFilmDto(f: CinemaMovie): ArchivedFilmDto =
    ArchivedFilmDto(f.movie, f.posterUrl, f.filmUrl, f.synopsis, f.cast, f.director,
      f.showtimes, f.externalIds, f.trailerUrl, f.ageRating, Some(f.showtimes.##))

  def fromSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): StoredScrapeDto =
    StoredScrapeDto(
      _id             = cinema.displayName,
      city            = city,
      scrapedAt       = Some(scrape.at),
      listingComplete = Some(scrape.listingComplete),
      films           = Some(scrape.films.map(toFilmDto)),
      // A scrape that just succeeded is by definition the newest thing that has
      // happened to this cinema, so nothing barren can still apply.
      lastBarren      = None
    )

  /** `None` for a row whose cinema no longer exists in the catalog — a venue that
   *  was renamed or dropped. Its films can't be attributed to anything, so the
   *  row is skipped rather than guessed at. */
  def toDomain(dto: StoredScrapeDto): Option[ArchivedScrape] =
    Cinema.byDisplayName.get(dto._id).map { cinema =>
      ArchivedScrape(
        cinema      = cinema,
        city        = dto.city,
        lastSuccess = dto.scrapedAt.map(at => SuccessfulScrape(
          at              = at,
          listingComplete = dto.listingComplete.getOrElse(true),
          films           = dto.films.getOrElse(Seq.empty).map(f => CinemaMovie(
            f.movie, cinema, f.posterUrl, f.filmUrl, f.synopsis, f.cast, f.director,
            f.showtimes, f.externalIds, f.trailerUrl, f.ageRating))
        )),
        lastBarren  = dto.lastBarren.flatMap(_.toDomain)
      )
    }
}

/** BSON wiring for `cinema_scrapes`. `IgnoreNone` throughout so an absent
 *  `synopsis`/`room`/`ageRating` costs nothing on the wire and decodes back to
 *  `None` — the same trade `MovieCodecs` makes for `Showtime`. */
object ScrapeArchiveCodecs extends PersistedCodecs {
  type OmittingNone = (ContentStampDto, NoScheduleFlagDto, Movie, ArchivedFilmDto, BarrenAttemptDto, StoredScrapeDto)
  type WritingNone  = EmptyTuple

  /** Every showtime through the movies' hand-written [[ShowtimeCodec]] (`Showtime` has no macro
   *  codec), and every film through [[StreamingArchivedFilmCodec]], which can store its booking URLs
   *  split at their shared prefix: a venue's listing carries every showtime it scraped, the identity
   *  intake reads a venue's back as each scrape lands, and the projection the whole archive — on the
   *  US corpus the macro's per-showtime machinery was most of the archive's decode, ~10% of a
   *  convergence leg's CPU (JFR). `ScrapeArchiveCodecsSpec` pins both to the macro's reading. */
  val registry: CodecRegistry = {
    val macros = fromRegistries(
      fromCodecs(JavaTimeCodecs.localDateTime, ShowtimeCodec),
      fromProviders(PersistedCodecs.omittingNone[OmittingNone]*),
      DEFAULT_CODEC_REGISTRY)
    fromRegistries(fromCodecs(new StreamingArchivedFilmCodec(macros.get(classOf[Movie]))), macros)
  }

  /** [[registry]] reading every film WITHOUT its showtimes (`Nil`, their digest kept): for a read that projects
   *  `films.showtimes` out on the server, and wants a venue's listing, not its screenings. */
  val leanRegistry: CodecRegistry =
    fromRegistries(fromCodecs(new StreamingArchivedFilmCodec(registry.get(classOf[Movie]), withShowtimes = false)), registry)
}

/**
 * Mongo-backed `ScrapeArchiveRepository`, collection `cinema_scrapes` — one row
 * per cinema, its listing replaced on every scrape that has content.
 *
 * A successful scrape overwrites its row's listing outright. A barren attempt reads the row's
 * `scrapedAt` and `lastBarren` first — the "only if newer" rule and the run it continues — and
 * writes the marker only while the row still holds them (`tools.GuardedWrite`), so a listing
 * that lands in between is never marked barren behind it.
 *
 * Relaxed write concern, and every operation `Try`-guarded: this collection is a
 * side-record of a scrape that has already happened, so a failed write must
 * never break the scrape that produced it. The next scrape rewrites the row.
 */
class MongoScrapeArchiveRepository(
  sharedDb:   Option[MongoDatabase],
  // `cinema_scrapes`, unless the same rules keep another venue-keyed listing set (the identity
  // projection's accepted listings, `IdentityListingIntake.Collection`).
  collection: String = ScrapeArchiveRepository.Collection,
  // Called once per lean page whose stage relays' days could not be read (`screeningDays`).
  daysUnread: () => Unit = () => ()
) extends ScrapeArchiveRepository with Logging {

  private lazy val coll: Option[MongoCollection[StoredScrapeDto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(ScrapeArchiveCodecs.registry)
      .getCollection[StoredScrapeDto](collection)
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    // No secondary index: every read and write here addresses a venue by `_id` or pages by it.
    // The `scrapedAt` and `lastBarren.at` indexes this used to build were for "which venues went
    // stale / are failing" reads that were never written — paid on every scrape's write for nothing.
    c
  }

  def enabled: Boolean = coll.isDefined

  /** `$set`s the listing's fields and `$unset`s the ones it leaves empty (the barren marker
   *  among them), rather than replacing the row: the row also carries fields this repository
   *  does not own (the scrape guards' state, `MongoScrapeGuardLedger`), which a replace on
   *  every scrape would wipe. An empty field encodes as nothing at all, so without the unset
   *  the previous scrape's value would outlive it. */
  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit =
    coll.foreach { c =>
      val dto     = StoredScrapeDto.fromSuccess(cinema, city, scrape)
      val encoded = new BsonDocument()
      ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto])
        .encode(new BsonDocumentWriter(encoded), dto, EncoderContext.builder().build())
      val fields  = encoded.entrySet().asScala.toSeq.filterNot(_.getKey == "_id")
        .map(e => Updates.set(e.getKey, e.getValue))
      val emptied = dto.productElementNames.filterNot(encoded.containsKey).map(Updates.unset).toSeq
      guard(cinema, "record") {
        Await.result(c.updateOne(Filters.eq("_id", dto._id), Updates.combine((fields ++ emptied)*),
          new UpdateOptions().upsert(true)).toFuture(), 30.seconds)
      }
    }

  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit =
    coll.foreach { c =>
      // Decided on the row as read — the "only if newer" rule and the run the marker continues — and
      // written only while the row still holds what was read (`MongoGuard`): a listing, or another
      // blank scrape's marker, landing in between sends it round again, decided afresh. The upsert
      // branch (first-ever sighting of a cinema that has only failed) creates a content-less row,
      // which decodes as `lastSuccess = None`.
      guard(cinema, "recordBarren") {
        val written = tools.GuardedWrite(MongoScrapeArchiveRepository.BarrenWriteAttempts)(() => barrenGuardRead(cinema)) { asRead =>
          val existing = asRead.map(decodeStored)
          Option.when(!existing.flatMap(_.scrapedAt).exists(_.isAfter(attempt.at))) {
            // The decision itself is the shared pure one, never this store's own idea of when a run began.
            val run    = BarrenAttempt.continuing(existing.flatMap(_.lastBarren).flatMap(_.toDomain), attempt)
            val marker = Updates.set("lastBarren", BarrenAttemptDto.from(run))
            city.fold(marker)(name => Updates.combine(marker, Updates.setOnInsert("city", name)))
          }
        } { (asRead, update) =>
          services.MongoGuard.updateIfUnchanged(c,
            services.MongoGuard.unchanged(new BsonString(cinema.displayName), asRead, MongoScrapeArchiveRepository.BarrenGuardFields),
            update, 30.seconds, insert = true)
        }
        written match {
          case tools.GuardedWrite.ChangedUnderYou(attempts) =>
            logger.warn(s"ScrapeArchiveRepository.recordBarren(${cinema.displayName}): the row changed between its read and " +
              s"its write $attempts times in a row — the marker is not written; the next blank scrape writes it")
          case _ => ()
        }
      }
    }

  /** The fields a blank scrape's marker is decided on, as stored — the guard compares them raw. A
   *  seam a spec uses to land a write between this read and the marker's. */
  protected def barrenGuardRead(cinema: Cinema): Option[BsonDocument] = coll.flatMap { c =>
    Await.result(c.withDocumentClass[BsonDocument]().find(Filters.eq("_id", cinema.displayName))
      .projection(Projections.include(MongoScrapeArchiveRepository.BarrenGuardFields*)).headOption(), 30.seconds)
  }

  private def decodeStored(document: BsonDocument): StoredScrapeDto =
    ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto])
      .decode(new org.bson.BsonDocumentReader(document), org.bson.codecs.DecoderContext.builder().build())

  def find(cinema: Cinema): Option[ArchivedScrape] = read(cinema).toOption.flatten

  /** Read raw and decoded apart: a row the codec refuses is NOT a failed read. Answered as one, the
   *  identity intake would leave every scrape of its venue undecided for good; read as absent, the
   *  next scrape's write replaces it. */
  override def read(cinema: Cinema): Try[Option[ArchivedScrape]] = coll.fold[Try[Option[ArchivedScrape]]](Success(None)) { c =>
    val row = Try(Await.result(c.withDocumentClass[BsonDocument]().find(Filters.eq("_id", cinema.displayName)).headOption(), 30.seconds))
    row.failed.foreach(e => logger.warn(s"ScrapeArchiveRepository.find(${cinema.displayName}) failed: ${e.getMessage}"))
    row.map(_.flatMap { document =>
      Try(ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto])
        .decode(new org.bson.BsonDocumentReader(document), org.bson.codecs.DecoderContext.builder().build())) match {
        case Success(dto) => StoredScrapeDto.toDomain(dto)
        case Failure(exception) =>
          logger.warn(s"ScrapeArchiveRepository.find(${cinema.displayName}): undecodable row " +
            s"(${exception.getClass.getSimpleName}: ${exception.getMessage}) — read as absent, for the next scrape to replace")
          None
      }
    })
  }

  /** Paged exactly like `scan` and for the same reason — the row COUNT, not
   *  the row size, is what recurses the driver's completion chain into a
   *  StackOverflowError — but projected down to `_id`, `scrapedAt` and
   *  `lastBarren.noScheduleListed` so a reading that only wants timestamps doesn't
   *  drag every archived film across with it.
   *
   *  An incomplete scan yields an EMPTY map, never the rows it managed to get.
   *  The caller counts cinemas that have produced nothing; a short read would
   *  hand it a list of cinemas that merely weren't fetched, and it would publish
   *  that as an outage. */
  def contentStamps(): Map[String, ContentStamp] = coll.toSeq.flatMap { c =>
    val stamps    = c.withDocumentClass[ContentStampDto]()
    val collected = Seq.newBuilder[ContentStampDto]
    val complete  = services.movies.KeysetScan.scan[ContentStampDto](
      label          = "ScrapeArchiveRepository content-stamp batch",
      batchSize      = MongoScrapeArchiveRepository.FindAllBatchSize,
      maxAttempts    = 5,
      initialBackoff = 2.seconds,
      keyOf          = _._id,
      fetchPage      = (afterId, limit) => {
        val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
        Await.result(
          stamps.find(filter)
            .projection(Projections.include("scrapedAt", "lastBarren.noScheduleListed"))
            .sort(org.mongodb.scala.model.Sorts.ascending("_id"))
            .limit(limit).batchSize(tools.MongoReplies.ScrapeArchive).toFuture(),
          60.seconds)
      },
      onIncomplete   = exception =>
        logger.warn(s"ScrapeArchiveRepository.contentStamps incomplete after retries: " +
          s"${exception.getClass.getSimpleName}: ${exception.getMessage}")
    )(batch => collected ++= batch)

    if (complete.isComplete) collected.result().map(d => d._id -> d.toDomain)
    else {
      logger.warn(s"ScrapeArchiveRepository.contentStamps discarding ${collected.result().size} row(s) from an " +
        "incomplete scan — returning empty so unfetched cinemas are never counted as barren")
      Seq.empty
    }
  }.toMap

  /**
   * Every archived scrape, read in keyset-paged batches rather than through one
   * unbounded `find()`, each page decoded and handed over before the next is fetched.
   *
   * The unbounded form did not merely run slowly on a large archive — it CRASHED.
   * A single cursor over a big collection recurses the async driver's per-message
   * completion chain deep enough to throw `StackOverflowError` on a driver I/O
   * thread (see [[services.movies.KeysetScan]], which exists because `movies` and
   * `screenings` hit exactly this). The crash lands on an uncaught I/O thread, not
   * on the caller's `Await`, so nothing here catches it: the future simply never
   * completes and the caller sees a 120s timeout with no cause attached. That is
   * precisely what the country-convergence legs saw against Germany's 1,515-row
   * archive — nine consecutive timeouts and two `StackOverflowError`s in threads
   * nobody was watching — and it will reach any caller as a country's archive grows.
   *
   * Paging caps how many rows one cursor delivers, keeping the completion chain
   * shallow. Each batch is an independently retried, idempotent `_id > afterId`
   * query, so a partial failure costs a page rather than the whole read.
   *
   * Still best-effort, like every read here: an incomplete scan logs and answers
   * incomplete, which `findAll` turns into an empty archive.
   */
  def scan(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome = scanVenues(_ => true)(consume)

  /** [[scan]], fetching only the rows whose `_id` names a venue `keep` admits: the ids are read
   *  first anyway, so a venue left out costs its id and nothing else. */
  override def scanVenues(keep: Cinema => Boolean)(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome =
    scanRows(keep, lean = false)(page => consume(page.flatMap(StoredScrapeDto.toDomain)))

  /** [[scanVenues]] with `films.showtimes` left on the server and in the reader: each film's showtimes digest is read
   *  instead, as it was written — and of a film billing a stage work, the days it screens on ([[LeanListing.leanFilm]]),
   *  asked of the server for those films alone ([[screeningDays]]). A venue holding a film written before the digest
   *  existed is read again whole once the lean scan is done — a row's next scrape writes it with one. */
  override def scanLean(keep: Cinema => Boolean)(consume: Seq[LeanListing] => Unit): tools.ScanOutcome = {
    val undigested = Set.newBuilder[String]
    val stageTitles = new LeanListing.StageTitles
    val lean = scanRows(keep, lean = true) { page =>
      val (digested, older) = page.partition(_.films.getOrElse(Nil).forall(_.showtimesDigest.isDefined))
      undigested ++= older.map(_._id)
      val rows = digested.flatMap(dto => StoredScrapeDto.toDomain(dto).flatMap(row => row.lastSuccess.map(s =>
        LeanListing(row.cinema, s.at, s.films.zip(dto.films.getOrElse(Nil)).map { case (film, stored) => film -> stored.showtimesDigest.get }))))
      val staged = rows.flatMap(row => row.films.iterator.zipWithIndex.collect {
        case ((film, _), index) if stageTitles(film) => (row.cinema.displayName, index, film.movie.rawTitle.getOrElse(film.movie.title)) })
      // a failed read of the days is no days: each relay's are UNKNOWN, which its next read asks again
      val days = if (staged.isEmpty) Some(Map.empty[(String, Int), (Int, Seq[java.time.LocalDate])])
        else Try(screeningDays(staged)) match {
          case Success(read) => Some(read)
          case Failure(e) =>
            logger.warn(s"ScrapeArchiveRepository.scanLean: the days of ${staged.size} stage relays could not be read — unknown " +
              s"until the next read: ${e.getClass.getSimpleName}: ${e.getMessage}")
            daysUnread()
            None
        }
      val wanted = staged.iterator.map { case (venue, index, _) => venue -> index }.toSet
      consume(if (wanted.isEmpty) rows else rows.map(row => row.copy(films = row.films.zipWithIndex.map { case ((film, digest), index) =>
        val key = row.cinema.displayName -> index
        if (!wanted(key)) film -> digest
        else days.fold(film.copy(showtimes = LeanListing.unread)) { read =>
          // the days of the very showtimes the digest was read with: a row re-scraped between the two reads keeps none
          read.get(key).filter(_._1 == digest).fold(film)(dated => film.copy(showtimes = LeanListing.days(dated._2)))
        } -> digest
      })))
    }
    val whole = undigested.result()
    if (whole.isEmpty) lean else lean.andThen(scanVenues(c => whole(c.displayName))(page => consume(page.flatMap(LeanListing.of))))
  }

  /** The days each of `films` — a venue, the film's place in its row and its raw title — screens on, with its showtimes'
   *  digest, by venue and place: the server reads them for those films alone (a US page of venues listing a Met relay
   *  holds ~500k showtimes, a few hundred of them the relays'). A failed read THROWS. */
  protected def screeningDays(films: Seq[(String, Int, String)]): Map[(String, Int), (Int, Seq[java.time.LocalDate])] =
    coll.fold(Map.empty[(String, Int), (Int, Seq[java.time.LocalDate])]) { c =>
      import org.bson.BsonArray
      val wanted = films.map { case (venue, index, _) => venue -> index }.toSet
      val titles = new BsonArray(films.map(_._3).distinct.map(t => BsonString(t): org.bson.BsonValue).asJava)
      val pick   = BsonDocument.parse(
        """{"$project": {"f": {"$filter": {"input": {"$map": {"input": {"$range": [0, {"$size": {"$ifNull": ["$films", []]}}]}, "as": "i",
          |  "in": {"$let": {"vars": {"f": {"$arrayElemAt": ["$films", "$$i"]}}, "in": {"i": "$$i",
          |    "t": {"$ifNull": ["$$f.movie.rawTitle", "$$f.movie.title"]}, "g": "$$f.showtimesDigest", "d": "$$f.showtimes.dateTime"}}}}},
          |  "cond": {"$in": ["$$this.t", []]}}}}}""".stripMargin)
      pick.getDocument("$project").getDocument("f").getDocument("$filter").getDocument("cond").getArray("$in").set(1, titles)
      val matched = new BsonDocument("$match", new BsonDocument("_id", new BsonDocument("$in",
        new BsonArray(films.map(_._1).distinct.map(v => BsonString(v): org.bson.BsonValue).asJava))))
      val docs = Await.result(c.withDocumentClass[BsonDocument]().aggregate[BsonDocument](Seq(matched, pick))
        .batchSize(tools.MongoReplies.Default).toFuture(), 60.seconds)
      docs.iterator.flatMap { d =>
          val venue = d.getString("_id").getValue
          d.getArray("f").getValues.asScala.iterator.map(_.asDocument).collect {
            case f if wanted(venue -> f.getInt32("i").getValue) && f.isInt32("g") =>
              val days = Option(f.get("d")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala).filter(_.isDateTime)
                .map(at => Instant.ofEpochMilli(at.asDateTime.getValue).atZone(java.time.ZoneOffset.UTC).toLocalDate)
              (venue, f.getInt32("i").getValue) -> (f.getInt32("g").getValue, days)
          }
        }.toMap
    }

  /** The stored rows of the venues `keep` admits, a page at a time; `lean`: without their films' showtimes. */
  private def scanRows(keep: Cinema => Boolean, lean: Boolean)(consume: Seq[StoredScrapeDto] => Unit): tools.ScanOutcome =
    coll.fold(tools.ScanOutcome.complete) { c =>
      // Budget enough retries to outlast a tunnel restart. The proxy dies mid-run and its supervisor
      // brings it back within a couple of seconds; 3 attempts at 1s backoff could expire inside that
      // window, turning a blip into an empty corpus. 5 attempts backing off 2s→32s covers it with room
      // to spare.
      def failed(exception: Throwable): Unit =
        logger.warn(s"ScrapeArchiveRepository.scan incomplete after retries — the rows read so far are a partial " +
          s"archive, not a smaller one: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
      // Every venue's id first — a keyset read of ids alone, a few small pages — then the rows a page
      // of FindAllBatchSize at a time, several pages side by side (`KeysetScan.byKeys`): a row is a
      // venue's whole listing, so a page is several round trips and a decode, and the US archive read
      // page after page was ~6 s of every identity projection.
      val ids      = Vector.newBuilder[String]
      val idsWhole = services.movies.KeysetScan.scan[org.bson.BsonDocument](
        label          = "ScrapeArchiveRepository id batch",
        batchSize      = MongoScrapeArchiveRepository.IdBatchSize,
        maxAttempts    = 5,
        initialBackoff = 2.seconds,
        keyOf          = _.getString("_id").getValue,
        fetchPage      = (afterId, limit) => Await.result(
          c.withDocumentClass[org.bson.BsonDocument]().find(afterId.fold(Filters.empty())(Filters.gt("_id", _)))
            .projection(Projections.include("_id")).sort(org.mongodb.scala.model.Sorts.ascending("_id")).limit(limit)
            .batchSize(tools.MongoReplies.Default).toFuture(),
          60.seconds),
        onIncomplete   = failed
      )(page => page.foreach { row =>
        val id = row.getString("_id").getValue
        // A row whose id names no venue decodes to nothing (`StoredScrapeDto.toDomain`), kept or not.
        if (Cinema.byDisplayName.get(id).exists(keep)) ids += id
      })
      idsWhole.andThen(services.movies.KeysetScan.byKeys[StoredScrapeDto](
        label          = "ScrapeArchiveRepository keyset batch",
        keys           = ids.result(),
        batchSize      = MongoScrapeArchiveRepository.FindAllBatchSize,
        inFlight       = MongoScrapeArchiveRepository.ScanPagesInFlight,
        maxAttempts    = 5,
        initialBackoff = 2.seconds,
        fetchKeys      = page => Await.result(
          (if (lean) c.withCodecRegistry(ScrapeArchiveCodecs.leanRegistry).find(Filters.in("_id", page*))
            .projection(Projections.exclude("films.showtimes"))
          else c.find(Filters.in("_id", page*))).sort(org.mongodb.scala.model.Sorts.ascending("_id"))
            .batchSize(if (lean) tools.MongoReplies.ScrapeArchive else tools.MongoReplies.ScrapeArchiveWithShowtimes).toFuture(),
          60.seconds),
        onIncomplete   = failed
      )(consume))
    }

  /** Every archive operation is best-effort: it records something that already
   *  happened, so its failure must not propagate into the scrape. */
  private def guard[A](cinema: Cinema, op: String)(body: => A): Option[A] =
    Try(body) match {
      case Success(value) => Some(value)
      case Failure(e)     =>
        logger.warn(s"ScrapeArchiveRepository.$op(${cinema.displayName}) failed: ${e.getMessage}")
        None
    }
}

object MongoScrapeArchiveRepository {
  /** The fields a blank scrape's marker is decided on — and guarded on. */
  val BarrenGuardFields: Seq[String] = Seq("scrapedAt", "lastBarren")

  /** How many times a blank scrape's marker reads and decides again when the row moved under it. */
  val BarrenWriteAttempts = 3

  /**
   * Rows per keyset page — sized by BYTES, not by row count.
   *
   * 200 was chosen as "a handful of round-trips" and still overflowed the driver.
   * An archive row is a whole venue's listing with every showtime, averaging 26–60
   * KB (measured: PL 44, UK 60, DE 26), so 200 rows is a 9–12 MB page. The
   * recursion this paging exists to avoid is per SOCKET READ, not per document, so
   * what matters is how many partial reads one message takes to arrive — and across
   * a `flyctl proxy` a multi-megabyte message takes plenty. Poland's entire
   * collection is only 12.9 MB, which is why 200 barely paged it at all and it
   * failed exactly as the unpaged version had.
   *
   * 25 keeps a page near 1 MB for every country. That is more round-trips than is
   * strictly elegant on a LAN, and irrelevant next to a read that does not complete.
   */
  val FindAllBatchSize = 25

  /** How many of [[FindAllBatchSize]]'s pages a scan reads side by side — ~1 MB each. */
  val ScanPagesInFlight = 4

  /** Ids per page of the id read that opens a scan: an id is a venue's name, so a page is small. */
  val IdBatchSize = 1000
}
