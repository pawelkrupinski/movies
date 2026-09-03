package services.staging

import com.mongodb.WriteConcern
import com.mongodb.client.model.{BulkWriteOptions, ReplaceOneModel, ReplaceOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.{MovieRecord, Source}
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, Observer, SingleObservableFuture, Subscription}
import play.api.Logging
import services.movies.{MovieCodecs, StoredMovieDto, TitleNormalizer}

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** One per-cinema staging row: a single cinema's report of a film that has not
 *  yet been TMDB-concluded. Unlike `movies` (one merged row per film across all
 *  cinemas), staging is one row per `cinema|title|year` — the raw, un-merged
 *  facts a newcomer arrives with. The `record` carries that one cinema's slot
 *  (plus a `Tmdb` slot once resolution runs); folding into `movies` unions the
 *  cinemas back together using the existing merge rules. */
/** `id` is the row's PERSISTED `_id`. It's normally `idFor(cinema, title, year)`,
 *  but the two can DRIFT: a row is keyed once at creation, while `title` is
 *  re-derived (`fromStorage` → `displayTitle`) on every read and re-cased by
 *  `recase`. Because `sanitize` is not perfectly casing-invariant (e.g. the
 *  case-sensitive "Gwiezdne Wojny: " strip), a re-cased title can sanitize to a
 *  DIFFERENT key than the one baked into `id`. So mutations/deletes of an EXISTING
 *  row MUST go through `id` (see `upsertRow`/`deleteRow`) — recomputing the id from
 *  the drifted title would spawn a duplicate under the new key and strand the
 *  original, which the staging reaper then re-resolves forever. */
case class StagingRecord(cinema: Source, title: String, year: Option[Int], record: MovieRecord, id: String)

object StagingRecord {
  /** Build a row whose `id` is the canonical `idFor` of its fields — the right
   *  default for a FRESH row (a scrape divert, a test seed). Rows read back from
   *  storage carry their persisted `_id` instead (see `fromStorage`). */
  def apply(cinema: Source, title: String, year: Option[Int], record: MovieRecord, normalizer: TitleNormalizer): StagingRecord =
    StagingRecord(cinema, title, year, record, idFor(cinema, title, year, normalizer))

  /** The Mongo `_id` for a staging row: `cinemaDisplayName|sanitize(title)|year`.
   *  A `Cinema.displayName` never contains `|` and `sanitize` never emits one, so
   *  the first `|` ends the cinema and the last `|` precedes the year — the middle
   *  is the sanitized title (the same prefix `movies` keys its `_id` on). */
  def idFor(cinema: Source, title: String, year: Option[Int], normalizer: TitleNormalizer): String =
    s"${cinema.displayName}|${normalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** Rebuild a staging row from its persisted `_id` + `MovieRecord`. The display
   *  title is derived from the record's slots (same as `StoredMovieRecord`), the
   *  year + cinema from the `_id`. Returns None for a row whose cinema segment is
   *  unknown (a dropped/renamed cinema), matching the codec's drop-unknown-source
   *  behaviour. */
  def fromStorage(id: String, record: MovieRecord, normalizer: TitleNormalizer): Option[StagingRecord] = {
    val firstSep = id.indexOf('|')
    val lastSep  = id.lastIndexOf('|')
    if (firstSep < 0 || lastSep <= firstSep) None
    else {
      val cinemaName = id.substring(0, firstSep)
      val prefix     = id.substring(firstSep + 1, lastSep)
      val year       = id.substring(lastSep + 1).toIntOption
      Source.byDisplayName.get(cinemaName).map(src => StagingRecord(src, record.displayTitle(prefix, normalizer), year, record, id))
    }
  }
}

/**
 * Persistent store for per-cinema newcomer rows awaiting TMDB conclusion — the
 * `pending_movies` collection. A genuinely-new `(title, year)` incubates here
 * (one row per cinema) until it resolves, then a transactional fold moves it
 * into `movies` and deletes the staging rows. The trait is what consumers see;
 * `MongoStagingRepository` (prod) and `InMemoryStagingRepository` (tests) are the impls,
 * wired by the trait per CLAUDE.md's DIP guidance.
 */
trait StagingRepository {
  /** Whether the persistence layer is wired up; writes no-op when false. */
  def enabled: Boolean

  /** Every staging row, ordered by `_id`. Returns empty when disabled. */
  def findAll(): Seq[StagingRecord]

  /**
   * The rows of ONE film — the sanitized-title anchor the staging state machine works on.
   *
   * Defaults to filtering [[findAll]], which is what callers did inline, so a store that
   * cannot do better is unchanged. It earns a method because the reaper asks it on every
   * staging event: against Mongo the default decodes every staged document to return a
   * handful, and during an ingest that grows the backlog that is quadratic in it. Measured
   * on a convergence leg, the scrape rate decayed from 28 venues per 37s to 28 per 243s
   * (Germany 110/38s to 110/429s; the UK 78/180s to 78/850s) — all of it here.
   *
   * The anchor is `normalizer.sanitize(row.title)` for the row `findAll` would have
   * returned — NOT anything derivable from the `_id`. An earlier attempt read it off the
   * key, which encodes the sanitized title as it was at the row's FIRST write, while
   * callers derive it at read time from `record.displayTitle`. Any override must answer
   * the same question the default does; [[StagingRepository]]'s own spec asserts the two
   * agree.
   */
  def findByAnchor(anchor: String): Seq[StagingRecord] = {
    findAll().filter(row => normalizer.sanitize(row.title) == anchor)
  }

  /**
   * The rows of ONE cinema — what a scrape tick needs to carry a newcomer's prior slot
   * forward and to prune the venue's rows it no longer lists.
   *
   * Same bargain as [[findByAnchor]], for the same reason: `MovieCache.recordCinemaScrape`
   * asked it once per VENUE as `findAll().collect { _.cinema == cinema }`, so against
   * Mongo every venue decoded the whole country's staging backlog to keep a handful, and
   * the cost grew with the backlog the tick itself was filling. That is the quadratic
   * this method's sibling already fixed for the reaper.
   *
   * `row.cinema` is the `Source` the row was diverted under; unlike the anchor it does
   * NOT drift, since a row's venue is fixed at its first write. The default still
   * filters [[findAll]], so a store that cannot do better is unchanged.
   */
  def findByCinema(cinema: models.Cinema): Seq[StagingRecord] =
    findAll().filter(row => models.Source.cinemaOf(row.cinema).contains(cinema))

  /** The country whose rules anchor these rows. ABSTRACT, like
   *  `MovieRepository.normalizer`; the worker wires its own. */
  def normalizer: TitleNormalizer

  /** Write-through upsert of one cinema's row, keyed by `idFor(cinema, title,
   *  year)` — the scrape-divert path, called on every tick a newcomer is still
   *  incubating. When the row already exists, its enrichment is carried forward
   *  (`carryForwardEnrichment`) so a re-scrape only refreshes the cinema slot and
   *  can't blank the resolve step's stamp. Best-effort — never throws. */
  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit

  /**
   * Stage every row ONE venue's scrape diverted, in one pass.
   *
   * The scrape path writes a row per diverted listing, and each was three serial round
   * trips — read the existing row, range-query its siblings, replace it. A venue lists
   * 16 films in Germany and 28 in the United States, and a tick walks every venue on one
   * thread, so the whole ingest ran at the latency of `3 x listings` sequential queries.
   * Sampled mid-tick, the scraping thread was parked on the database in 52 of 60 stacks;
   * this is what it was parked on.
   *
   * The DEFAULT is the loop it replaces, so a repository with no batch of its own — the
   * in-memory one, the stubs — inherits identical behaviour rather than a second
   * implementation of it. Only the Mongo one overrides, and only to change how many
   * round trips the same writes take.
   *
   * Best-effort per row, like [[upsert]]: one row that cannot be written must not cost
   * the venue its other fifteen.
   */
  def upsertAll(rows: Seq[(Source, String, Option[Int], MovieRecord)]): Unit =
    rows.foreach { case (cinema, title, year, record) => upsert(cinema, title, year, record) }

  /** Write `row.record` back under the row's PERSISTED `id`. Use to re-stamp an
   *  EXISTING row (detail/resolve/imdb) so a title whose casing drifted updates
   *  the SAME row instead of spawning a duplicate. The real impls key by `row.id`;
   *  this default delegation suffices for lightweight stubs and for rows that never
   *  drift (`id == idFor(cinema, title, year)`). Best-effort — never throws. */
  def upsertRow(row: StagingRecord): Unit = upsert(row.cinema, row.title, row.year, row.record)

  /** Remove one cinema's row by `idFor(cinema, title, year)`. Best-effort. */
  def delete(cinema: Source, title: String, year: Option[Int]): Unit

  /** Remove an EXISTING row by its persisted `id` (drift-proof in the real impls;
   *  the default delegation is correct for non-drifting rows). Best-effort. */
  def deleteRow(row: StagingRecord): Unit = delete(row.cinema, row.title, row.year)

  /** Stream inserts/updates (`onUpsert`) and deletes (`onDelete`, given the row's
   *  `_id`) so consumers can react as newcomers land and graduate. Best-effort;
   *  `None` when unsupported (disabled, or standalone Mongo). */
  def watchChanges(onUpsert: StagingRecord => Unit, onDelete: String => Unit): Option[AutoCloseable] = None

  /** Stream inserted/updated rows so the promoter can enrich them as they land.
   *  Derived from `watchChanges` (deletes ignored). */
  def watchUpserts(onUpsert: StagingRecord => Unit): Option[AutoCloseable] = watchChanges(onUpsert, _ => ())

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit = ()
}

object StagingRepository {

  /** Rows per keyset page for [[MongoStagingRepository.findAll]]. `pending_movies`
   *  rows are whole `MovieRecord`s, so this is sized well under the read-model's
   *  500 to keep a page comfortably small. */
  val FindAllBatchSize = 300

  /** The staging collection — see [[services.DebugMirror]] for why the name is a
   *  constant rather than an inline literal. */
  val Collection = "pending_movies"

  /** Merge a fresh scrape-divert `fresh` record onto the `existing` row already
   *  stored under the same `_id`, so a RE-SCRAPE refreshes the cinema's slot
   *  WITHOUT clobbering the enrichment the resolve step stamped.
   *
   *  `MovieCache.recordCinemaScrape` re-diverts a newcomer through `upsert` on
   *  EVERY scrape tick until it folds, rebuilding a BLANK `MovieRecord` (one
   *  cinema slot, no tmdbId/imdbId/tmdbNoMatch) each time. A blind replace nulled
   *  the resolution between the resolve step and the fold, so the film folded
   *  un-enriched into `movies` and the reaper re-resolved it forever ("stuck in
   *  staging").
   *
   *  `fresh` is the base so a write that DOES carry resolution still wins (a row
   *  resolving in place); each enrichment field falls back to `existing` only when
   *  `fresh` lacks it (the blank re-scrape). `data ++ fresh.data` lets the fresh
   *  cinema slot win (new showtimes replace stale ones, not accumulate) while
   *  keeping the existing `Tmdb` slot. */
  def carryForwardEnrichment(existing: MovieRecord, fresh: MovieRecord): MovieRecord =
    fresh.copy(
      tmdbId      = fresh.tmdbId.orElse(existing.tmdbId),
      imdbId      = fresh.imdbId.orElse(existing.imdbId),
      tmdbNoMatch = fresh.tmdbNoMatch || existing.tmdbNoMatch,
      searchTitle = fresh.searchTitle.orElse(existing.searchTitle),
      data        = existing.data ++ fresh.data)

  /** The `cinema|sanitize(title)|` prefix every year-variant of one film at one
   *  cinema shares — `idFor`'s output up to (and including) the final `|`. Reads
   *  the persisted `_id`, so it's drift-proof (the title re-derived on read can
   *  re-sanitize differently; the `_id` can't). */
  def cinemaTitlePrefix(id: String): String = id.substring(0, id.lastIndexOf('|') + 1)

  /** The `_id`s already in the store that are the SAME (cinema, sanitized title)
   *  as `newId` — its year-variant siblings (yearless, 2025, 2026, …), excluding
   *  `newId` itself. Compared on the `_id` prefix, not the re-derived title. */
  def sameFilmSiblings(newId: String, existingIds: Iterable[String]): Seq[String] = {
    val prefix = cinemaTitlePrefix(newId)
    existingIds.iterator.filter(id => id != newId && id.startsWith(prefix)).toSeq
  }

  /** The warning to log when an `upsert` of `newId` is a fresh INSERT (the row was
   *  `!alreadyPresent`) that joins existing same-(cinema, sanitized-title)
   *  `siblings` — a movie with the same sanitized title + source cinema entering
   *  `pending_movies` while another year-variant is already incubating. `None`
   *  when it's a re-upsert of an existing row (no new entry) or there's no sibling,
   *  so a normal per-tick re-divert stays silent. */
  def duplicateEntryWarning(newId: String, alreadyPresent: Boolean, siblings: Seq[String]): Option[String] =
    Option.when(!alreadyPresent && siblings.nonEmpty)(
      s"Staging RE-ENTRY (same film + cinema): '$newId' entered pending_movies while the same " +
        s"(cinema, sanitized title) is already staged as ${siblings.mkString(", ")} — a movie with the same " +
        s"sanitized title and source cinema is incubating under more than one key. The fold collapses the " +
        s"year-variants, but a recurring pair points at scrape title/year churn worth fixing at source.")

  /** A disabled, empty no-op `StagingRepository` — the default for callers that don't
   *  wire staging (e.g. the web `/debug` controller in tests, or any non-staging
   *  build). `findAll` is empty and writes are dropped. */
  val empty: StagingRepository = new StagingRepository {
    def enabled: Boolean = false
    // Holds no rows, so nothing is ever anchored — but the member is abstract
    // now, and naming the fallback HERE is the point: it is visible rather than
    // inherited by omission.
    val normalizer: TitleNormalizer = TitleNormalizer.deployment
    def findAll(): Seq[StagingRecord] = Seq.empty
    def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = ()
    def delete(cinema: Source, title: String, year: Option[Int]): Unit = ()
  }
}

/**
 * MongoDB-backed `StagingRepository` over the `pending_movies` collection. Reuses the
 * `movies` storage shape (`StoredMovieDto` + `MovieCodecs.registry`) — a staging
 * row is just a `MovieRecord` with a single-cinema `data` map — differing only in
 * the collection name and the cinema-scoped `_id`. Mirrors `MongoMovieRepository`'s
 * relaxed write concern: `pending_movies` is re-scraped continuously and its rows
 * are transient, so a write lost to a crash is recovered by the next scrape.
 */
class MongoStagingRepository(
  sharedDb: Option[MongoDatabase] = None,
  // See `StagingRepository.normalizer` — the rules that anchor a row's `_id`.
  // REQUIRED here: production persistence must never fall back.
  override val normalizer: TitleNormalizer
) extends StagingRepository with Logging {


  private lazy val coll: Option[MongoCollection[StoredMovieDto]] =
    sharedDb.map { db =>
      db.withCodecRegistry(MovieCodecs.registry)
        .getCollection[StoredMovieDto](StagingRepository.Collection)
        .withWriteConcern(WriteConcern.W1.withJournal(false))
    }

  def enabled: Boolean = coll.isDefined

  /**
   * Keyset-paged, not one unbounded cursor: `pending_movies` is corpus-shaped
   * (every newcomer awaiting its fold), and an unbounded `find()` over a big
   * collection recurses the async driver into `StackOverflowError` on an I/O
   * thread — which the caller never sees, because it lands off the `Await` and
   * surfaces only as a timeout. See [[services.movies.KeysetScan]].
   *
   * The silent-empty degradation matters more here than the crash, because
   * nothing downstream deletes on this read — it STALLS instead, invisibly.
   * `StagingReaper` enqueues nothing, `stepCounts` reports zeros to Prometheus,
   * and `StagingStuckAlerter` finds nothing stuck: the alarm for "staging is
   * wedged" goes quiet at exactly the moment Mongo is unwell. So an incomplete
   * scan now says so at WARN with the count it managed, rather than returning a
   * confident empty list.
   */
  def findAll(): Seq[StagingRecord] = coll match {
    case None => Seq.empty
    case Some(c) =>
      val buf = Vector.newBuilder[StagingRecord]
      val complete = services.movies.KeysetScan.scan[StoredMovieDto](
        label          = "StagingRepository.findAll",
        batchSize      = StagingRepository.FindAllBatchSize,
        maxAttempts    = 3,
        initialBackoff = 500.millis,
        keyOf          = _._id,
        fetchPage      = (afterId, limit) => {
          val find = afterId.fold(c.find())(a => c.find(Filters.gt("_id", a)))
          Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds)
        },
        onIncomplete   = exception =>
          logger.warn(s"StagingRepository.findAll keyset scan failed: ${exception.getClass.getSimpleName}: " +
            s"${exception.getMessage} — callers will see a SHORT staging view this tick (the reaper enqueues " +
            "less, the stuck-alerter reports less); it is not evidence that staging is empty")
      )(batch => buf ++= batch.flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto, normalizer).record, normalizer)))

      val records = buf.result()
      if (!complete) logger.warn(s"StagingRepository.findAll returned ${records.size} record(s) from an INCOMPLETE scan")
      records
  }

  /**
   * `_id` → anchor for every staged row, in memory, maintained on write.
   *
   * Built once by a single decoding pass and then kept current by `upsertId`/`deleteId` —
   * every write to this collection goes through one of them. A lookup is then a scan of a
   * few thousand strings and a fetch of the matching handful, instead of decoding the
   * whole collection on every staging event.
   *
   * The anchor is computed exactly as the callers compute it — `sanitize` of the title
   * `StagingRecord.fromStorage` derives — so this answers the same question the inherited
   * default answers, by construction rather than by coincidence. That is the correction:
   * a previous version inferred it from the `_id`, which holds the sanitized title from
   * the row's FIRST write and diverges as soon as one is normalised.
   */
  /** The id fetch, as a seam a test can fail on purpose — the fallback below is the
   *  interesting behaviour and is otherwise unreachable without a broken database. */
  protected def fetchByIds(c: MongoCollection[StoredMovieDto], ids: Seq[String]): Try[Seq[StoredMovieDto]] =
    Try(Await.result(c.find(Filters.in("_id", ids*)).toFuture(), 30.seconds))

  private val anchorById = new java.util.concurrent.ConcurrentHashMap[String, String]()
  /** The cinema half of the same row index — built in the same pass as the anchor, so a
   *  scrape tick's per-venue read costs a keyed lookup and a keyed fetch rather than
   *  decoding the whole backlog. */
  private val cinemaById = new java.util.concurrent.ConcurrentHashMap[String, models.Cinema]()

  /**
   * The three INVERSES of the maps above — the direction every caller actually asks in.
   *
   * `anchorById`/`cinemaById` answer "what is this row?", but nothing asks that. The
   * lookups ask "which rows does this anchor / this cinema / this (cinema, title) have?",
   * and answered it by walking the whole id map: a scan of every staged row in the
   * country, per venue and per fold. The doc above called that "a scan of a few thousand
   * strings", which it was for Poland — the United States stages 121,544, and the scan is
   * inside a loop over 4,304 venues.
   *
   * The forward maps stay, because a re-key has to find the buckets a row is LEAVING.
   */
  private val idsByAnchor      = new java.util.concurrent.ConcurrentHashMap[String, java.util.Set[String]]()
  private val idsByCinema      = new java.util.concurrent.ConcurrentHashMap[models.Cinema, java.util.Set[String]]()
  /** Keyed by [[StagingRepository.cinemaTitlePrefix]] — the `cinema|title|` an `_id`
   *  carries, NOT the row's current anchor. `siblingIds` is defined on that prefix (a row
   *  keeps the sanitized title of its FIRST write), so this must be too. */
  private val idsByCinemaTitle = new java.util.concurrent.ConcurrentHashMap[String, java.util.Set[String]]()
  @volatile private var anchorIndexBuilt = false

  private def bucketAdd[K](index: java.util.concurrent.ConcurrentHashMap[K, java.util.Set[String]],
                           key: K, id: String): Unit =
    index.computeIfAbsent(key, _ => java.util.concurrent.ConcurrentHashMap.newKeySet[String]()).add(id)

  private def bucketRemove[K](index: java.util.concurrent.ConcurrentHashMap[K, java.util.Set[String]],
                              key: K, id: String): Unit =
    Option(index.get(key)).foreach { ids =>
      ids.remove(id)
      // `remove(key, value)` — the two-arg form — so an empty bucket is only dropped if
      // it is still the SAME set another thread has not just added to.
      if (ids.isEmpty) index.remove(key, ids)
    }

  private def bucketIds[K](index: java.util.concurrent.ConcurrentHashMap[K, java.util.Set[String]],
                           key: K): Seq[String] = {
    import scala.jdk.CollectionConverters._
    Option(index.get(key)).map(_.asScala.toVector).getOrElse(Vector.empty)
  }

  /** Index one row under all four maps, moving it off whatever it was filed under
   *  before — a re-key changes a row's anchor, and a bucket it silently stayed in would
   *  hand `findByAnchor` an id that no longer belongs to it. */
  private def indexRow(id: String, anchor: String, cinema: Option[models.Cinema]): Unit = {
    Option(anchorById.put(id, anchor)).filter(_ != anchor).foreach(bucketRemove(idsByAnchor, _, id))
    bucketAdd(idsByAnchor, anchor, id)
    cinema.foreach { c =>
      Option(cinemaById.put(id, c)).filter(_ != c).foreach(bucketRemove(idsByCinema, _, id))
      bucketAdd(idsByCinema, c, id)
    }
    bucketAdd(idsByCinemaTitle, StagingRepository.cinemaTitlePrefix(id), id)
  }

  private def unindexRow(id: String): Unit = {
    Option(anchorById.remove(id)).foreach(bucketRemove(idsByAnchor, _, id))
    Option(cinemaById.remove(id)).foreach(bucketRemove(idsByCinema, _, id))
    bucketRemove(idsByCinemaTitle, StagingRepository.cinemaTitlePrefix(id), id)
  }

  private def ensureAnchorIndex(): Unit =
    if (!anchorIndexBuilt) synchronized {
      if (!anchorIndexBuilt) {
        findAll().foreach(row =>
          indexRow(row.id, normalizer.sanitize(row.title), models.Source.cinemaOf(row.cinema)))
        anchorIndexBuilt = true
      }
    }

  /** Decoded rows are re-checked against `cinema` before being returned, so a stale index
   *  entry can only ever cost a wasted fetch — never a wrong row. Degrades to the full
   *  scan on a read failure for the reason spelled out on [[findByAnchor]]: a short answer
   *  here would read as "this venue stages nothing" and silently skip its prune. */
  override def findByCinema(cinema: models.Cinema): Seq[StagingRecord] = coll.toSeq.flatMap { c =>
    ensureAnchorIndex()
    val ids = bucketIds(idsByCinema, cinema)
    if (ids.isEmpty) Seq.empty
    else fetchByIds(c, ids) match {
      case Success(rows) =>
        rows.flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto, normalizer).record, normalizer).toSeq)
          .filter(row => models.Source.cinemaOf(row.cinema).contains(cinema))
      case Failure(exception) =>
        logger.warn(s"StagingRepository.findByCinema('${cinema.displayName}') could not fetch ${ids.size} row(s): " +
          s"${exception.getClass.getSimpleName}: ${exception.getMessage} — falling back to a full scan")
        super.findByCinema(cinema)
    }
  }

  /** Decoded rows are re-checked against `anchor` before being returned, so a stale index
   *  entry can only ever cost a wasted fetch — never a wrong row. */
  override def findByAnchor(anchor: String): Seq[StagingRecord] = coll.toSeq.flatMap { c =>
    ensureAnchorIndex()
    val ids = bucketIds(idsByAnchor, anchor)
    if (ids.isEmpty) Seq.empty
    else fetchByIds(c, ids) match {
      case Success(rows) =>
        rows.flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto, normalizer).record, normalizer).toSeq)
          .filter(row => normalizer.sanitize(row.title) == anchor)
      // NOT `Seq.empty`. A short answer here tells the reaper this film has no rows, so it
      // skips the film's next step — indistinguishable from the film being finished, and
      // permanent, because nothing revisits it. Degrade to the slower full scan instead:
      // the cost of a read failure should be time, not a silently abandoned film.
      case Failure(exception) =>
        logger.warn(s"StagingRepository.findByAnchor('$anchor') could not fetch ${ids.size} row(s): " +
          s"${exception.getClass.getSimpleName}: ${exception.getMessage} — falling back to a full scan")
        super.findByAnchor(anchor)
    }
  }

  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = {
    val id       = StagingRecord.idFor(cinema, title, year, normalizer)
    val existing = recordAt(id)
    // On a fresh INSERT only (not the per-tick re-divert of an existing row), warn
    // if a movie with the same (cinema, sanitized title) is already staged under
    // another year-key — a same-film+cinema duplicate entering pending_movies.
    if (existing.isEmpty)
      StagingRepository.duplicateEntryWarning(id, alreadyPresent = false, siblingIds(id)).foreach(logger.warn(_))
    // Carry forward any enrichment already on the row so a re-scrape can't blank
    // the resolve step's tmdbId/imdbId/tmdbNoMatch (see `carryForwardEnrichment`).
    upsertId(id, existing.fold(record)(StagingRepository.carryForwardEnrichment(_, record)))
  }

  /** Sibling `_id`s of `id`'s (cinema, sanitized title), via an index-friendly `_id`
   *  range over the shared `cinema|sanitize|` prefix — no collection scan, no regex
   *  escaping. The `|` separator (0x7C) sorts after every digit + letter, so the
   *  range can't bleed into a different film whose prefix is a superstring. */
  private def siblingIds(id: String): Seq[String] = {
    ensureAnchorIndex()
    // NO ROUND TRIP. This was a projected range query over the `cinema|title|` prefix,
    // run on EVERY fresh insert — and it exists solely to decide whether to log a
    // duplicate-entry warning. A cold pass inserts every row fresh, so a country paid one
    // query per staged listing (121,544 of them for the United States) for a log line.
    //
    // Projecting it to `_id` was the previous round of this: fetching whole documents
    // made the warning cost a full decode of every sibling, showtimes array and all
    // (19,748 bytes returned unprojected, 311 projected), which took `bootCorpus` from 30
    // seconds to 3,360 and timed the leg out at CI's ceiling. The query was always the
    // wrong shape rather than the wrong projection: `_id` is the only thing it reads, and
    // the repository already keeps every `_id` in memory.
    bucketIds(idsByCinemaTitle, StagingRepository.cinemaTitlePrefix(id)).filterNot(_ == id)
  }

  override def upsertRow(row: StagingRecord): Unit = upsertId(row.id, row.record)

  /** The `MovieRecord` currently stored under `id`, if any. Used to preserve
   *  enrichment across a re-scrape. Best-effort — None on any read failure. */
  private def recordAt(id: String): Option[MovieRecord] = coll.flatMap { c =>
    Try(Await.result(c.find(Filters.eq("_id", id)).limit(1).toFuture(), 10.seconds))
      .toOption.flatMap(_.headOption).map(dto => StoredMovieDto.toDomain(dto, normalizer).record)
  }

  /**
   * A venue's diverted rows in TWO round trips — one read, one bulk write — instead of
   * three per row. See the trait's [[StagingRepository.upsertAll]] for why that was the
   * ingest's whole cost.
   *
   * Both of this repository's standing hazards are handled, and neither is hypothetical:
   *
   *  - A FAILED READ IS NOT DATA. If the prefetch fails, treating "no rows came back" as
   *    "no rows exist" would carry no enrichment forward and blank the resolve stamp on
   *    every row of the venue at once — the shape that has cost this repository a
   *    production outage. It falls back to the per-row path, which reads each row itself.
   *  - ONE BAD ROW MUST NOT KILL THE BATCH. A bulk write is all-or-nothing per request,
   *    and a single undecodable document taking a whole venue's writes with it is the
   *    other shape this repository has shipped. On any bulk failure it re-runs the rows
   *    ONE AT A TIME, so a poison row costs itself and nothing else.
   */
  override def upsertAll(rows: Seq[(Source, String, Option[Int], MovieRecord)]): Unit =
    if (rows.nonEmpty) coll.foreach { c =>
      ensureAnchorIndex()
      val keyed = rows.map { case (cinema, title, year, record) =>
        StagingRecord.idFor(cinema, title, year, normalizer) -> record
      }
      fetchByIds(c, keyed.map(_._1).distinct) match {
        case Failure(exception) =>
          logger.warn(s"StagingRepository.upsertAll could not prefetch ${keyed.size} row(s): " +
            s"${exception.getClass.getSimpleName}: ${exception.getMessage} — writing them one at a time, " +
            "because an unread row is not an absent one and carrying nothing forward would blank its enrichment")
          super.upsertAll(rows)
        case Success(dtos) =>
          val existing = dtos.map(dto => dto._id -> StoredMovieDto.toDomain(dto, normalizer).record).toMap
          // Ordered, and later rows see earlier ones: two listings of one venue can key to
          // the same `cinema|title|year`, and the serial path they replace had the second
          // carry the first forward.
          val pending = scala.collection.mutable.LinkedHashMap.empty[String, MovieRecord]
          keyed.foreach { case (id, record) =>
            val prior = pending.get(id).orElse(existing.get(id))
            // On a fresh INSERT only, exactly as the per-row path warns.
            if (prior.isEmpty)
              StagingRepository.duplicateEntryWarning(id, alreadyPresent = false, siblingIds(id)).foreach(logger.warn(_))
            pending.update(id, prior.fold(record)(StagingRepository.carryForwardEnrichment(_, record)))
          }
          val writes = pending.toSeq.map { case (id, record) =>
            ReplaceOneModel(Filters.eq("_id", id), stagedDto(id, record), new ReplaceOptions().upsert(true))
          }
          Try(Await.result(c.bulkWrite(writes, new BulkWriteOptions().ordered(false)).toFuture(), 30.seconds))
            .recover { case exception =>
              logger.warn(s"StagingRepository.upsertAll bulk write of ${writes.size} row(s) failed: " +
                s"${exception.getClass.getSimpleName}: ${exception.getMessage} — retrying them one at a time")
              pending.foreach { case (id, record) => upsertId(id, record) }
            }
      }
    }

  /** File the row under every index and render its document — the half of a write that is
   *  the same whether one row is going out or a whole venue's. Both halves of the row
   *  index move with the write, or `findByAnchor` / `findByCinema` answer from a snapshot
   *  that predates it. */
  private def stagedDto(id: String, record: MovieRecord): StoredMovieDto = {
    StagingRecord.fromStorage(id, record, normalizer).foreach(row =>
      indexRow(id, normalizer.sanitize(row.title), models.Source.cinemaOf(row.cinema)))
    StoredMovieDto.fromDomain(id, record, Instant.now())
  }

  private def upsertId(id: String, record: MovieRecord): Unit = coll.foreach { c =>
    val dto = stagedDto(id, record)
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"StagingRepository.upsert($id) failed: ${exception.getMessage}")
    }
  }

  def delete(cinema: Source, title: String, year: Option[Int]): Unit =
    deleteId(StagingRecord.idFor(cinema, title, year, normalizer))

  override def deleteRow(row: StagingRecord): Unit = deleteId(row.id)

  private def deleteId(id: String): Unit = coll.foreach { c =>
    unindexRow(id)
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"StagingRepository.delete($id) failed: ${exception.getMessage}")
    }
  }

  override def watchChanges(onUpsert: StagingRecord => Unit, onDelete: String => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit =
          try Option(change.getFullDocument) match {
            case Some(dto) => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto, normalizer).record, normalizer).foreach(onUpsert)
            // A delete (or drop/invalidate) carries no full document — the change's
            // document key holds the `_id` of the row that graduated/left.
            case None => Option(change.getDocumentKey).map(_.getString("_id").getValue).foreach(onDelete)
          }
          catch { case exception: Throwable => logger.warn(s"StagingRepository change-stream apply failed: ${exception.getMessage}") }
        override def onError(e: Throwable): Unit =
          logger.warn(s"StagingRepository change stream ended (${e.getMessage}) — relying on the periodic backstop.")
        override def onComplete(): Unit = ()
      })
    logger.info("MongoStagingRepository: watching pending_movies change stream.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }
}
