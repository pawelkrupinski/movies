package services.scrapes

import scala.util.{Success, Try}

import models.{ArchivedScrape, BarrenAttempt, Cinema, CinemaMovie, ContentStamp, LeanListing, ScrapeAttempt, ScrapeOutcome, SuccessfulScrape}

/**
 * The read side of the scrape archive: a venue's row, scans of them all, and the content stamps.
 * A component that only reads the archive (the closure sweep, the content census, the gone-venue
 * check, the identity intake's fallback copy) takes this; [[ScrapeArchiveRepository]] extends it
 * with the writes and their rules.
 */
trait ScrapeArchiveReader {

  /** Is there anywhere to write? `false` when Mongo is absent, which makes every
   *  write a silent no-op rather than an error. */
  def enabled: Boolean

  def find(cinema: Cinema): Option[ArchivedScrape]

  /** [[find]], telling a read that FAILED from a row that is absent — `find` answers `None` for
   *  both, and a caller that decides from the row (the identity intake, judging a scrape against
   *  the venue's accepted listing) must not read the one as the other. The in-memory stores
   *  cannot fail, so the default is `find`'s answer. */
  def read(cinema: Cinema): Try[Option[ArchivedScrape]] = Success(find(cinema))

  /** Every archived scrape, handed to `consume` a page at a time, in no promised order; complete
   *  when the whole archive was read. A page is the caller's to keep or drop: a caller that
   *  reduces each page (the identity intake, turning rows into listings) never holds the whole
   *  archive's parsed films at once — tens of kilobytes a venue, hundreds of megabytes a country.
   *
   *  On an incomplete scan the pages already handed over are a PARTIAL archive, and the caller must not
   *  mistake them for a smaller one — a failed read is not data ([[findAll]]). */
  def scan(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome

  /** [[scan]] of only the venues `keep` admits: the same answer as filtering every page, but a
   *  repository that can tell a row's venue before reading it (Mongo, by its `_id`) never fetches
   *  or decodes the others — a cut-over country's projection reads the archive only for the venues
   *  with no accepted listing, a handful of the US archive's 5,000 rows, every five minutes. */
  def scanVenues(keep: Cinema => Boolean)(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome =
    scan(page => consume(page.filter(row => keep(row.cinema))))

  /** [[scanVenues]] of each venue's last successful listing WITHOUT its showtimes ([[LeanListing]]): what a reader
   *  that keeps none wants — the identity model's take-up, the projection's listing read. Here the full rows,
   *  reduced; a repository that can leave the showtimes where they are (Mongo) does, and reads only their digest. */
  def scanLean(keep: Cinema => Boolean, keepsDays: CinemaMovie => Boolean)(consume: Seq[LeanListing] => Unit): tools.ScanOutcome =
    scanVenues(keep)(page => consume(page.flatMap(LeanListing.of(_, keepsDays))))

  /** Every archived scrape, all at once — the replay/repopulate entry point, for a caller that
   *  needs the rows themselves; one that only reduces them should [[scan]]. Empty on an
   *  INCOMPLETE read, not the rows it managed to get: a partial archive looks exactly like a
   *  smaller one, and it nearly wrote a corpus FIXTURE missing 45 of 281 venues — a truncated
   *  read that would then have been replayed as authoritative on every future run. */
  def findAll(): Seq[ArchivedScrape] = {
    val rows = Seq.newBuilder[ArchivedScrape]
    if (scan(rows ++= _).isComplete) rows.result() else Seq.empty
  }

  /** When each archived cinema last produced ANY films, and whether its newest
   *  attempt since was its source affirmatively listing no schedule, keyed by
   *  display name. Absent from the map, or a `None` stamp, both mean the same
   *  thing: no content-bearing scrape has ever been recorded for it.
   *
   *  Deliberately not `findAll().map(...)`. An `ArchivedScrape` carries its entire
   *  parsed listing, so decoding every row to read one timestamp each hauls the
   *  whole corpus across the wire to answer a question about a few hundred
   *  instants — on the country whose archive is largest, exactly the read that has
   *  already crashed this collection once.
   *
   *  Empty on a read that could not be completed, NOT a partial map: the caller is
   *  measuring which cinemas have gone quiet, and a short read would hand it a
   *  pile of cinemas that merely weren't fetched. A failed read is not data. */
  def contentStamps(): Map[String, ContentStamp]
}


/**
 * The last content-bearing scrape of every cinema, one row per cinema, plus the
 * newest barren attempt on top of it. Sized for the whole corpus: ~2,700 rows /
 * ~28 MB across the three countries (each country's worker writes its own
 * database).
 *
 * Two implementations share this contract: [[MongoScrapeArchiveRepository]]
 * (durable) and [[InMemoryScrapeArchiveRepository]] (tests / Mongo-less dev).
 * The rules either could get wrong — content is never replaced by nothing, a
 * stale barren marker never lands on a fresher listing — live in [[record]]
 * here, above the seam, so neither implementation can diverge on them.
 */
trait ScrapeArchiveRepository extends ScrapeArchiveReader {

  /** File one scrape attempt.
   *
   *  With films, it becomes the row's content and clears any barren marker — the
   *  cinema is healthy again. Without films it is recorded as a marker only,
   *  never overwriting the listing already stored: a blank or throwing scrape is
   *  almost always a failed read (a blocked host, a changed layout, a 503), and
   *  a failed read is not data. The last good listing is the whole point of this
   *  collection, so nothing barren is ever allowed to consume it. `lastBarren.at`
   *  is what tells you how long the cinema has been in that state. */
  final def record(attempt: ScrapeAttempt): Unit = attempt.outcome match {
    case ScrapeOutcome.Ok =>
      storeSuccess(attempt.cinema, attempt.city,
        SuccessfulScrape(attempt.at, attempt.listingComplete, attempt.films))
    case barren =>
      storeBarren(attempt.cinema, attempt.city, BarrenAttempt(attempt.at, barren, attempt.error,
        noScheduleListed = barren == ScrapeOutcome.Empty && attempt.noScheduleListed, sourceKey = attempt.sourceKey))
  }

  /** Persist a scrape that HAS content: replace the row's listing and drop any
   *  barren marker. */
  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit

  /** Record an attempt that produced nothing, leaving any stored listing intact.
   *  Ignored when the row's listing is NEWER than `attempt` — an out-of-order
   *  arrival must not make a current row look stale. */
  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit

  def close(): Unit = ()
}

object ScrapeArchiveRepository {

  val Collection: String = "cinema_scrapes"

  /** A repository that stores nothing — for wiring with no archive (scripts, the
   *  web tier, specs that don't care). */
  val empty: ScrapeArchiveRepository = new ScrapeArchiveRepository {
    def enabled: Boolean                             = false
    protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit = ()
    protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit    = ()
    def find(cinema: Cinema): Option[ArchivedScrape] = None
    def scan(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome = tools.ScanOutcome.complete
    def contentStamps(): Map[String, ContentStamp]   = Map.empty
  }
}

/** In-memory `ScrapeArchiveRepository` for tests and Mongo-less dev. Same
 *  semantics as the Mongo one: one row per cinema, content replaced only by
 *  content, barren markers applied only when newer than the stored listing. Each row
 *  is kept as written through the archive's codec to BSON and read back. */
class InMemoryScrapeArchiveRepository extends ScrapeArchiveRepository {

  private val byCinema = scala.collection.mutable.Map.empty[String, ArchivedScrape]

  private def stored(row: ArchivedScrape): ArchivedScrape =
    StoredScrapeDto.toDomain(row.cinema, tools.BsonRoundTrip.throughCodec(ScrapeArchiveCodecs.registry.get(classOf[StoredScrapeDto]))(StoredScrapeDto.of(row)))

  def enabled: Boolean = true

  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit =
    byCinema.synchronized(byCinema.update(cinema.displayName,
      stored(ArchivedScrape(cinema, city, Some(scrape), lastBarren = None))))

  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit =
    byCinema.synchronized {
      val row = byCinema.getOrElse(cinema.displayName, ArchivedScrape(cinema, city, None, None))
      if (!row.contentAt.exists(_.isAfter(attempt.at)))
        byCinema.update(cinema.displayName,
          stored(row.copy(lastBarren = Some(BarrenAttempt.continuing(row.lastBarren, attempt)))))
    }

  def find(cinema: Cinema): Option[ArchivedScrape] =
    byCinema.synchronized(byCinema.get(cinema.displayName))

  def scan(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome = {
    consume(byCinema.synchronized(byCinema.values.toSeq))
    tools.ScanOutcome.complete
  }

  // No projection to make here — the rows are already in memory, so reading the
  // stamp off each is the whole job. The Mongo one earns its own query.
  def contentStamps(): Map[String, ContentStamp] =
    byCinema.synchronized(byCinema.view.mapValues(ContentStamp.of).toMap)
}
