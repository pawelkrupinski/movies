package services.identity

import models.{Cinema, CinemaMovie, VenueClock}
import services.movies.{ScrapeGuardLedger, ScrapeGuardState, ScrapeLandingMetrics, ScrapeSink, TitleNormalizer}
import services.scrapes.{ScrapeArchiveRepository, ScrapeAttempt}

import java.time.Clock
import scala.util.{Failure, Success, Try}

/**
 * A cut-over country's scrape sink (docs/design/identity-resolver.md §8, phase 5): each venue's
 * scrape becomes the venue's ACCEPTED listing by [[ListingIntake]]'s rules — the old landing's
 * scrape-health guards, deciding the evidence rather than which slots to write — and nothing is
 * written to `movies`: the identity projection makes the films from the accepted listings.
 *
 * `accepted` keeps one listing per venue (`identity_listings`, the scrape archive's own rules and
 * shape). A venue with none yet — every venue, the first time a country is cut over — reads the
 * scrape archive's last listing (`archive`), which is what the old path last landed from.
 * `guards` is the landing's own ledger, so a country switched back and forth keeps one count.
 */
final class IdentityListingIntake(
  accepted:      ScrapeArchiveRepository,
  archive:       ScrapeArchiveRepository,
  guards:        ScrapeGuardLedger,
  normalizer:    TitleNormalizer,
  maxRejections: Int,
  clock:         Clock,
  metrics:       ScrapeLandingMetrics,
  published:     (Cinema, Seq[CinemaMovie]) => Unit = (_, _) => ()
) extends ScrapeSink with play.api.Logging {

  /** The listing `cinema` is taken to publish now. */
  def listingOf(cinema: Cinema): Seq[CinemaMovie] = knownListing(cinema).getOrElse(Nil)

  /** [[listingOf]], or the failure of a read it rests on: an accepted listing that could not be read
   *  is not an absent one, and judged against the archive's copy in its place, a scrape the guards
   *  held back from it would replace it. */
  private def knownListing(cinema: Cinema): Try[Seq[CinemaMovie]] =
    accepted.read(cinema).flatMap(_.flatMap(_.lastSuccess) match {
      case Some(scrape) => Success(scrape.films)
      case None         => archive.read(cinema).map(_.flatMap(_.lastSuccess).fold(Seq.empty[CinemaMovie])(_.films))
    })

  /** Every venue of `live` with the listing it is taken to publish, venues publishing nothing left out.
   *
   *  Both archives are read a page at a time, each page reduced to the live venues' listings before
   *  the next: the rows of venues no longer live, and the
   *  archive's copy of a venue that already has an accepted listing, are never fetched. An archive that
   *  could not be read whole counts as empty — a partial read is not a smaller archive. The
   *  listings share their repeated values ([[ListingValuePool]]). */
  def listings(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = read(live)((_, films) => films)

  /** [[listings]] as the projection holds them: each listing without its row or showtimes, only their digests
   *  ([[ProjectedListing]]), each venue's rows reduced as its page is read.
   *
   *  Held between calls, and read again only for the venues whose listing moved since the last: their row's stamp
   *  in either archive (`contentStamps`, `_id` and `scrapedAt` alone), a scrape this intake took since (a listing is
   *  stamped by the clock that filed it, which can file two at one instant), or a venue whose roster entry is
   *  another object now. Read whole, the two archives were the most of Mongo's outbound traffic — the US's 308 MB
   *  every five minutes — for the one venue in twelve that re-scrapes between projections. Read whole anyway every
   *  [[IdentityListingIntake.WholeReadEvery]] calls, and whenever a stamp or keyed read could not be completed: a
   *  partial read is not a smaller archive. */
  def projected(live: Seq[Cinema]): Seq[ProjectedListing] = heldLock.synchronized {
    val wanted  = live.distinct.map(c => c.displayName -> c).toMap
    val written = dirty.synchronized { val names = dirty.toSet; dirty.clear(); names }
    val whole   = calls % IdentityListingIntake.WholeReadEvery == 0
    calls += 1
    def project(cinema: Cinema, films: Seq[CinemaMovie]) = films.map(cm => ProjectedListing.of(Listing.of(cinema, cm, normalizer), cm))
    val acceptedNow = heldAccepted.refresh(accepted, wanted, written, whole, project)
    val archivedNow = heldArchived.refresh(archive, wanted.filter { case (name, _) => !acceptedNow.contains(name) }, written, whole, project)
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedNow.get(c.displayName).orElse(archivedNow.get(c.displayName)).flatten.getOrElse(Nil))
  }

  // The projection's held listings, by venue name, one set per archive; venues written since the last read.
  private val heldLock     = new Object
  private val heldAccepted = new IdentityListingIntake.Held
  private val heldArchived = new IdentityListingIntake.Held
  private val dirty        = scala.collection.mutable.Set.empty[String]
  private var calls        = 0L

  /** The rows each of `venues` publishes now, showtimes and all — what the projection builds a venue's slots from. */
  def rowsOf(venues: Set[Cinema]): Map[Cinema, Seq[CinemaMovie]] = listings(venues.toSeq).toMap

  /** Each venue of `live` publishing something, with `view` of its listing: its accepted one, else the archive's. */
  private def read[A](live: Seq[Cinema])(view: (Cinema, Seq[CinemaMovie]) => A): Seq[(Cinema, A)] = {
    val wanted          = live.toSet
    val values          = new ListingValuePool
    val acceptedByVenue = IdentityListingIntake.lastListings(accepted, wanted, values, view)
    val archivedByVenue = IdentityListingIntake.lastListings(archive, c => wanted(c) && !acceptedByVenue.contains(c), values, view)
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedByVenue.get(c).orElse(archivedByVenue.get(c)).flatten.map(c -> _))
  }

  // A venue's guard state and accepted listing are its own, so one venue's scrape waits only for
  // another of the SAME venue — never for the whole country's: a US scrape walk lands 4,462 venues.
  private val venueLocks = new StripedLocks()

  override def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String],
                                  viaFallback: Boolean): Unit = venueLocks.locking(Seq(cinema.displayName)) {
    knownListing(cinema) match {
      case Failure(exception) =>
        // Nothing decided, written or published: the venue's next scrape judges against what it holds.
        logger.warn(s"Identity intake: ${cinema.displayName}'s listing could not be read (${exception.getMessage}) — " +
          "leaving its scrape undecided")
      case Success(known) => land(cinema, known, ListingIntake.Offer(movies, listingIsComplete, sourceKey, viaFallback))
    }
  }

  private def land(cinema: Cinema, known: Seq[CinemaMovie], offer: ListingIntake.Offer): Unit = {
    val stored = guards.get(cinema)
    val guard  = stored.getOrElse(ScrapeGuardState.Fresh)
    val verdict = ListingIntake.decide(cinema, known, offer, guard, new VenueClock(clock).nowAt(cinema, clock.getZone), maxRejections, normalizer)
    verdict.guarded.foreach(g => metrics.recordGuardVerdict(g.guard, g.verdict))
    // An unreadable ledger is judged as fresh, and its state is never written back over it.
    if (stored.isDefined && verdict.guard != guard) guards.put(cinema, verdict.guard)
    val recorded = verdict.outcome != ListingIntake.Outcome.Kept && verdict.accepted != known
    if (recorded)
      accepted.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), clock.instant(), listingComplete = true, verdict.accepted, error = None))
    // Read the venue again at the next projection, whatever was recorded: a scrape archived at the instant of the
    // one before it — a pinned clock — moves no stamp, and the archive's copy is what a venue with no accepted
    // listing publishes.
    dirty.synchronized { dirty += cinema.displayName; () }
    // What the venue is taken to publish now — its accepted listing, or the archive's when the intake
    // kept none of its own — to whoever keeps a model of it (the identity model). Unrecorded, that is
    // still the listing just read: read again only when the archive's own rules decided what landed.
    published(cinema, if (recorded) listingOf(cinema) else known)
  }
}

object IdentityListingIntake {
  /** Every how many projection reads the listings are read whole, whatever their stamps say: at the projection's
   *  five minutes, hourly — what bounds a staleness no stamp or write could tell. */
  val WholeReadEvery = 12

  /** One archive's listings as the projection last read them, by venue name: the venue as it was then, the row's
   *  stamp, and its listings (`None`: it listed nothing). */
  private[identity] final class Held {
    private final case class Entry(cinema: Cinema, stamp: Option[java.time.Instant], listings: Option[Seq[ProjectedListing]])
    private var entries = Map.empty[String, Entry]

    /** The listings of `wanted`'s venues in `repository` now, re-read for those whose stamp moved, whose venue is
     *  another object or that were `written`, or all of them when `whole`; empty when a read could not complete. */
    def refresh(repository: ScrapeArchiveRepository, wanted: Map[String, Cinema], written: Set[String], whole: Boolean,
                project: (Cinema, Seq[CinemaMovie]) => Seq[ProjectedListing]): Map[String, Option[Seq[ProjectedListing]]] = {
      val stamps = if (whole) Map.empty[String, services.scrapes.ContentStamp] else repository.contentStamps()
      // No stamps is a read that failed or an empty archive: either way read it whole, which answers both.
      val present = stamps.collect { case (name, services.scrapes.ContentStamp(Some(at), _)) if wanted.contains(name) => name -> at }
      val keep = if (whole || stamps.isEmpty) Map.empty[String, Entry] else entries.filter { case (name, entry) =>
        present.get(name).exists(at => entry.stamp.contains(at)) && (entry.cinema eq wanted(name)) && !written(name)
      }
      val reread: Cinema => Boolean =
        if (whole || stamps.isEmpty) c => wanted.get(c.displayName).exists(_ eq c)
        else c => present.contains(c.displayName) && !keep.contains(c.displayName) && wanted.get(c.displayName).exists(_ eq c)
      val fresh    = Map.newBuilder[String, Entry]
      val complete = repository.scanVenues(reread)(_.foreach(row => row.lastSuccess.foreach(s =>
        fresh += row.cinema.displayName -> Entry(row.cinema, Some(s.at), Option.when(s.films.nonEmpty)(project(row.cinema, s.films))))))
      entries = if (complete.isComplete) keep ++ fresh.result() else Map.empty
      entries.map { case (name, entry) => name -> entry.listings }
    }
  }

  /** Where each venue's accepted listing is kept. */
  val Collection = "identity_listings"

  /** Each `keep` venue's last successful listing in `repository`, scanned a page at a time, its values held
   *  in `values`, and taken as `view` of it as its page is read (`None`: it listed nothing); empty when the
   *  scan could not complete. */
  private def lastListings[A](repository: ScrapeArchiveRepository, keep: Cinema => Boolean, values: ListingValuePool,
                              view: (Cinema, Seq[CinemaMovie]) => A): Map[Cinema, Option[A]] = {
    val byVenue  = Map.newBuilder[Cinema, Option[A]]
    val complete = repository.scanVenues(keep)(_.foreach(row => row.lastSuccess.foreach(s =>
      byVenue += row.cinema -> Option.when(s.films.nonEmpty)(view(row.cinema, s.films.map(values.film))))))
    if (complete.isComplete) byVenue.result() else Map.empty
  }
}
