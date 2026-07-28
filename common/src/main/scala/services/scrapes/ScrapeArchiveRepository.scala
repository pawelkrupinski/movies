package services.scrapes

import models.{Cinema, CinemaMovie}

import java.time.Instant

/** What a scrape attempt amounted to. Mirrors how the uptime view reads a
 *  cinema: it produced a listing, it came back blank, or it blew up. */
sealed abstract class ScrapeOutcome(val label: String)

object ScrapeOutcome {
  /** The client returned films. The only outcome that carries content. */
  case object Ok extends ScrapeOutcome("ok")

  /** The client returned successfully with ZERO films — the "white" scrape. Nearly
   *  always a silent failure (a changed layout, a blocked page that still renders),
   *  occasionally a cinema that genuinely has nothing on. */
  case object Empty extends ScrapeOutcome("empty")

  /** The scrape threw — the "red" scrape. */
  case object Failed extends ScrapeOutcome("failed")

  val all: Seq[ScrapeOutcome]              = Seq(Ok, Empty, Failed)
  def byLabel(l: String): Option[ScrapeOutcome] = all.find(_.label == l)
}

/** A scrape that produced films, kept verbatim as the client emitted it. */
case class SuccessfulScrape(
  at:              Instant,
  // Did the client see the cinema's WHOLE listing? A chunked scrape that lost a
  // chunk records `false` — a replay must not treat such a row as the cinema's
  // full repertoire.
  listingComplete: Boolean,
  films:           Seq[CinemaMovie]
) {
  def showtimeCount: Int = films.iterator.map(_.showtimes.size).sum
}

/** A scrape attempt that produced nothing — empty or thrown. Carries no content
 *  by definition, so it is recorded as a marker beside the last good listing
 *  rather than in place of it. */
case class BarrenAttempt(
  at:      Instant,
  outcome: ScrapeOutcome,
  // The exception's message for a [[ScrapeOutcome.Failed]]; `None` for an empty.
  error:   Option[String]
)

/**
 * One cinema's archive row: its last scrape that had content, plus — when the
 * cinema has since come back blank or thrown — the newest such attempt.
 *
 * The content is the consolidated `Seq[CinemaMovie]` a client produced, after a
 * chunked scraper's chunks have been reduced and before `MovieCache` folds it
 * into the corpus. Parsed domain output, not the raw HTML/JSON behind it, so a
 * scrape can be replayed — into a test, into an empty database, into a local
 * stack — without going back to the network.
 */
case class ArchivedScrape(
  cinema:      Cinema,
  // The city this cinema is listed under (`Cinema.cityOf`), carried so a replay
  // can group by city without re-deriving it. `None` for a synthetic
  // chain-detail source belonging to no single city.
  city:        Option[String],
  // The last scrape with films in it. `None` only for a cinema that has never
  // produced any since the archive began.
  lastSuccess: Option[SuccessfulScrape],
  // The newest attempt that produced nothing, kept only while it is NEWER than
  // [[lastSuccess]] — so the pair reads as "here is the listing, and here is
  // what has happened since". Cleared the moment a scrape succeeds again.
  lastBarren:  Option[BarrenAttempt]
) {
  def films: Seq[CinemaMovie] = lastSuccess.map(_.films).getOrElse(Seq.empty)

  /** How this cinema's most recent attempt ended. */
  def outcome: ScrapeOutcome = lastBarren.map(_.outcome).getOrElse(ScrapeOutcome.Ok)

  /** When we last saw a real listing from this cinema. */
  def contentAt: Option[Instant] = lastSuccess.map(_.at)

  /** Is the stored listing still what the cinema last actually served? False once
   *  a blank or failing scrape has landed on top of it. */
  def current: Boolean = lastBarren.isEmpty
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
trait ScrapeArchiveRepository {

  /** Is there anywhere to write? `false` when Mongo is absent, which makes every
   *  write a silent no-op rather than an error. */
  def enabled: Boolean

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
      storeBarren(attempt.cinema, attempt.city, BarrenAttempt(attempt.at, barren, attempt.error))
  }

  /** Persist a scrape that HAS content: replace the row's listing and drop any
   *  barren marker. */
  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit

  /** Record an attempt that produced nothing, leaving any stored listing intact.
   *  Ignored when the row's listing is NEWER than `attempt` — an out-of-order
   *  arrival must not make a current row look stale. */
  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit

  def find(cinema: Cinema): Option[ArchivedScrape]

  /** Every archived scrape. The replay/repopulate entry point — the corpus is a
   *  few thousand rows, so this is a bounded read, unlike the film collections. */
  def findAll(): Seq[ArchivedScrape]

  def close(): Unit = ()
}

/** One scrape attempt as the runner observed it — the archive's input. Its
 *  [[outcome]] is derived, not passed, so "empty means barren" is decided in one
 *  place rather than at each call site. */
case class ScrapeAttempt(
  cinema:          Cinema,
  city:            Option[String],
  at:              Instant,
  listingComplete: Boolean,
  films:           Seq[CinemaMovie],
  error:           Option[String] = None
) {
  def outcome: ScrapeOutcome =
    if (error.isDefined) ScrapeOutcome.Failed
    else if (films.isEmpty) ScrapeOutcome.Empty
    else ScrapeOutcome.Ok
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
    def findAll(): Seq[ArchivedScrape]               = Seq.empty
  }
}

/** In-memory `ScrapeArchiveRepository` for tests and Mongo-less dev. Same
 *  semantics as the Mongo one: one row per cinema, content replaced only by
 *  content, barren markers applied only when newer than the stored listing. */
class InMemoryScrapeArchiveRepository extends ScrapeArchiveRepository {

  private val byCinema = scala.collection.mutable.Map.empty[String, ArchivedScrape]

  def enabled: Boolean = true

  protected def storeSuccess(cinema: Cinema, city: Option[String], scrape: SuccessfulScrape): Unit =
    byCinema.synchronized(byCinema.update(cinema.displayName,
      ArchivedScrape(cinema, city, Some(scrape), lastBarren = None)))

  protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit =
    byCinema.synchronized {
      val row = byCinema.getOrElse(cinema.displayName, ArchivedScrape(cinema, city, None, None))
      if (!row.contentAt.exists(_.isAfter(attempt.at)))
        byCinema.update(cinema.displayName, row.copy(lastBarren = Some(attempt)))
    }

  def find(cinema: Cinema): Option[ArchivedScrape] =
    byCinema.synchronized(byCinema.get(cinema.displayName))

  def findAll(): Seq[ArchivedScrape] =
    byCinema.synchronized(byCinema.values.toSeq)
}
