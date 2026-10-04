package services.identity

import models.{Cinema, CinemaMovie, VenueClock}
import services.movies.{ListingKey, ScrapeGuardLedger, ScrapeGuardState, ScrapeLandingMetrics, ScrapeSink, TitleNormalizer}
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
  def listings(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = read(live, lean = false)

  /** [[listings]] without a showtime (each film's `Nil`): what the identity model takes up, which reads none. Left on
   *  the server, they were ~11 CPU-s of a US boot's take-up decoding them (`ShowtimeCodec.read`, JFR). */
  def identities(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = read(live, lean = true)

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
  def projected(live: Seq[Cinema]): Seq[ProjectedListing] = projectedByVenue(live).flatMap(_._2)

  /** [[projected]] by venue, each venue's listing the SAME object as last time while the venue was not read again — how
   *  the projection tells a venue that did not move from one that did without comparing their listings
   *  (`LiveProjectionIndex`). */
  def projectedByVenue(live: Seq[Cinema]): Seq[(Cinema, Seq[ProjectedListing])] = heldLock.synchronized {
    val read = if (calls % IdentityListingIntake.WholeReadEvery == 0) IdentityListingIntake.Read.Whole else IdentityListingIntake.Read.Stamped
    calls += 1
    projectedBy(live, read)
  }

  /** [[projectedByVenue]] reading again only the venues this intake took a scrape of since the last read, a venue now
   *  another roster object, and a venue it holds nothing of: no archive's stamps are read. What another process filed
   *  meanwhile is read by the next [[projectedByVenue]] — what a projection run on this intake's own scrapes, between
   *  two of those, reads its listings by. */
  def projectedChanged(live: Seq[Cinema]): Seq[(Cinema, Seq[ProjectedListing])] = heldLock.synchronized {
    projectedBy(live, IdentityListingIntake.Read.Changed)
  }

  private def projectedBy(live: Seq[Cinema], read: IdentityListingIntake.Read): Seq[(Cinema, Seq[ProjectedListing])] = {
    val wanted  = live.distinct.map(c => c.displayName -> c).toMap
    val written = dirty.synchronized { val names = dirty.toSet; dirty.clear(); names }
    // A listing the identity model holds the same is its object, not a second copy of it ([[adopt]]).
    def project(cinema: Cinema, films: Seq[(CinemaMovie, Int)]) =
      films.map { case (cm, showtimes) =>
        val listing = Listing.of(cinema, cm, normalizer)
        ProjectedListing.of(IdentityListingIntake.modelledAs(adopted, listing), cm, showtimes)
      }
    val acceptedNow = heldAccepted.refresh(accepted, wanted, written, read, project)
    val archivedNow = heldArchived.refresh(archive, wanted.filter { case (name, _) => !acceptedNow.contains(name) }, written, read, project)
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedNow.get(c.displayName).orElse(archivedNow.get(c.displayName)).flatten.map(c -> _))
  }

  /** The listings the identity model holds, as the projection last read them: each venue re-read from now on is projected
   *  with the model's object for a listing it holds the same, and each venue already held takes it now. Projected anew
   *  from every scrape, the projection held a second copy of every listing beside the model's (worker-us: 100k listings,
   *  their keys and catalogue ids) — and from a worker's first read, taken before the model reached the intake, until the
   *  whole read an hour later. A venue that takes one is another object, which the projection reads as unmoved by value. */
  def adopt(held: Seq[Listing]): Unit = heldLock.synchronized {
    adopted = held.iterator.map(l => l.key -> l).toMap
    heldAccepted.adopt(adopted)
    heldArchived.adopt(adopted)
  }
  private var adopted = Map.empty[ListingKey, Listing]

  // The projection's held listings, by venue name, one set per archive; venues written since the last read.
  private val heldLock     = new Object
  private val heldAccepted = new IdentityListingIntake.Held
  private val heldArchived = new IdentityListingIntake.Held
  private val dirty        = scala.collection.mutable.Set.empty[String]
  private var calls        = 0L

  /** The rows each of `venues` publishes now, showtimes and all — what the projection builds a venue's slots from. */
  def rowsOf(venues: Set[Cinema]): Map[Cinema, Seq[CinemaMovie]] = listings(venues.toSeq).toMap

  /** Each venue of `live` publishing something, with its listing: its accepted one, else the archive's; `lean`: without showtimes. */
  private def read(live: Seq[Cinema], lean: Boolean): Seq[(Cinema, Seq[CinemaMovie])] = {
    val wanted          = live.toSet
    val values          = new ListingValuePool
    val acceptedByVenue = IdentityListingIntake.lastListings(accepted, wanted, values, lean)
    val archivedByVenue = IdentityListingIntake.lastListings(archive, c => wanted(c) && !acceptedByVenue.contains(c), values, lean)
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
    def refresh(repository: ScrapeArchiveRepository, wanted: Map[String, Cinema], written: Set[String], read: Read,
                project: (Cinema, Seq[(CinemaMovie, Int)]) => Seq[ProjectedListing]): Map[String, Option[Seq[ProjectedListing]]] = {
      val current: Cinema => Boolean = c => wanted.get(c.displayName).exists(_ eq c)
      val (keep, reread) = read match {
        case Read.Whole => (Map.empty[String, Entry], current)
        case Read.Changed =>
          // Only what this intake knows moved: what it took a scrape of, a venue now another roster object, one it
          // holds nothing of (a venue the archive has no row of is never fetched, `scanLean` reading by id).
          val keep = entries.filter { case (name, entry) => wanted.get(name).exists(_ eq entry.cinema) && !written(name) }
          (keep, (c: Cinema) => current(c) && !keep.contains(c.displayName))
        case Read.Stamped =>
          val stamps = repository.contentStamps()
          // No stamps is a read that failed or an empty archive: either way read it whole, which answers both.
          if (stamps.isEmpty) (Map.empty[String, Entry], current)
          else {
            val present = stamps.collect { case (name, services.scrapes.ContentStamp(Some(at), _)) if wanted.contains(name) => name -> at }
            val keep = entries.filter { case (name, entry) =>
              present.get(name).exists(at => entry.stamp.contains(at)) && (entry.cinema eq wanted(name)) && !written(name)
            }
            (keep, (c: Cinema) => present.contains(c.displayName) && !keep.contains(c.displayName) && current(c))
          }
      }
      val fresh    = Map.newBuilder[String, Entry]
      // Nothing to read again — the usual tick — is no scan at all: even one that fetches no row first reads every
      // venue's id to pick the rows (`scanLean`), half of a quiet read's documents.
      val complete =
        if (!wanted.valuesIterator.exists(reread)) tools.ScanOutcome.complete
        else repository.scanLean(reread)(_.foreach(row =>
          fresh += row.cinema.displayName -> Entry(row.cinema, Some(row.at), Option.when(row.films.nonEmpty)(project(row.cinema, row.films)))))
      entries = if (complete.isComplete) keep ++ fresh.result() else Map.empty
      entries.map { case (name, entry) => name -> entry.listings }
    }

    /** Each held venue with the model's object for a listing it holds the same; a venue with none to take is kept as is. */
    def adopt(modelled: Map[ListingKey, Listing]): Unit =
      entries = entries.map { case (name, entry) =>
        val taken = entry.listings.filter(_.exists(p => modelledAs(modelled, p.listing) ne p.listing))
        name -> taken.fold(entry)(ls => entry.copy(listings = Some(ls.map { p =>
          val listing = modelledAs(modelled, p.listing)
          if (listing eq p.listing) p else p.copy(listing = listing)
        })))
      }
  }

  /** The model's object for `listing` when it holds one the same, else `listing`. */
  private def modelledAs(modelled: Map[ListingKey, Listing], listing: Listing): Listing =
    modelled.get(listing.key).filter(_ == listing).getOrElse(listing)

  /** How a projection read takes the archives: whole, by their rows' stamps, or only what this intake took since. */
  private[identity] enum Read { case Whole, Stamped, Changed }

  /** Where each venue's accepted listing is kept. */
  val Collection = "identity_listings"

  /** Each `keep` venue's last successful listing in `repository`, scanned a page at a time, its values held
   *  in `values` (`None`: it listed nothing) — `lean`, without showtimes; empty when the scan could not complete. */
  private def lastListings(repository: ScrapeArchiveRepository, keep: Cinema => Boolean, values: ListingValuePool,
                           lean: Boolean): Map[Cinema, Option[Seq[CinemaMovie]]] = {
    val byVenue = Map.newBuilder[Cinema, Option[Seq[CinemaMovie]]]
    def add(cinema: Cinema, films: Seq[CinemaMovie]): Unit = byVenue += cinema -> Option.when(films.nonEmpty)(films.map(values.film))
    val complete =
      if (lean) repository.scanLean(keep)(_.foreach(row => add(row.cinema, row.films.map(_._1))))
      else repository.scanVenues(keep)(_.foreach(row => row.lastSuccess.foreach(s => add(row.cinema, s.films))))
    if (complete.isComplete) byVenue.result() else Map.empty
  }
}
