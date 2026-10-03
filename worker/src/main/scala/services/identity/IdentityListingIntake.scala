package services.identity

import models.{Cinema, CinemaMovie, City}
import services.movies.{ScrapeGuardLedger, ScrapeGuardState, ScrapeLandingMetrics, ScrapeSink, TitleNormalizer}
import services.scrapes.{ScrapeArchiveRepository, ScrapeAttempt}

import java.time.Clock

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
) extends ScrapeSink {

  /** The listing `cinema` is taken to publish now. */
  def listingOf(cinema: Cinema): Seq[CinemaMovie] =
    accepted.find(cinema).flatMap(_.lastSuccess).orElse(archive.find(cinema).flatMap(_.lastSuccess)).map(_.films).getOrElse(Nil)

  /** Every venue of `live` with the listing it is taken to publish, venues publishing nothing left out.
   *
   *  Both archives are read a page at a time, each page reduced to the live venues' listings before
   *  the next: the rows of venues no longer live, and the
   *  archive's copy of a venue that already has an accepted listing, are never fetched. An archive that
   *  could not be read whole counts as empty — a partial read is not a smaller archive. */
  def listings(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = read(live)((_, films) => films)

  /** [[listings]] as the projection holds them: each listing without its showtimes, only their digest
   *  ([[ProjectedListing]]) — each venue's rows reduced as its page is read, so the showtimes of the whole
   *  corpus are never held at once. */
  def projected(live: Seq[Cinema]): Seq[ProjectedListing] =
    read(live)((cinema, films) => films.map(cm => ProjectedListing.of(Listing.of(cinema, cm, normalizer), cm))).flatMap(_._2)

  /** The rows each of `venues` publishes now, showtimes and all — what the projection builds a venue's slots from. */
  def rowsOf(venues: Set[Cinema]): Map[Cinema, Seq[CinemaMovie]] = listings(venues.toSeq).toMap

  /** Each venue of `live` publishing something, with `view` of its listing: its accepted one, else the archive's. */
  private def read[A](live: Seq[Cinema])(view: (Cinema, Seq[CinemaMovie]) => A): Seq[(Cinema, A)] = {
    val wanted          = live.toSet
    val acceptedByVenue = IdentityListingIntake.lastListings(accepted, wanted, view)
    val archivedByVenue = IdentityListingIntake.lastListings(archive, c => wanted(c) && !acceptedByVenue.contains(c), view)
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedByVenue.get(c).orElse(archivedByVenue.get(c)).flatten.map(c -> _))
  }

  // A venue's guard state and accepted listing are its own, so one venue's scrape waits only for
  // another of the SAME venue — never for the whole country's: a US scrape walk lands 4,462 venues.
  private val venueLocks = new StripedLocks()

  override def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String],
                                  viaFallback: Boolean): Unit = venueLocks.locking(Seq(cinema.displayName)) {
    val stored = guards.get(cinema)
    val guard  = stored.getOrElse(ScrapeGuardState.Fresh)
    val known  = listingOf(cinema)
    val verdict = ListingIntake.decide(cinema, known, ListingIntake.Offer(movies, listingIsComplete, sourceKey, viaFallback), guard,
      City.localNow(cinema, clock), maxRejections, normalizer)
    verdict.guarded.foreach(g => metrics.recordGuardVerdict(g.guard, g.verdict))
    // An unreadable ledger is judged as fresh, and its state is never written back over it.
    if (stored.isDefined && verdict.guard != guard) guards.put(cinema, verdict.guard)
    val recorded = verdict.outcome != ListingIntake.Outcome.Kept && verdict.accepted != known
    if (recorded)
      accepted.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), clock.instant(), listingComplete = true, verdict.accepted, error = None))
    // What the venue is taken to publish now — its accepted listing, or the archive's when the intake
    // kept none of its own — to whoever keeps a model of it (the identity model). Unrecorded, that is
    // still the listing just read: read again only when the archive's own rules decided what landed.
    published(cinema, if (recorded) listingOf(cinema) else known)
  }
}

object IdentityListingIntake {
  /** Where each venue's accepted listing is kept. */
  val Collection = "identity_listings"

  /** Each `keep` venue's last successful listing in `repository`, scanned a page at a time and taken
   *  as `view` of it as its page is read (`None`: it listed nothing); empty when the scan could not complete. */
  private def lastListings[A](repository: ScrapeArchiveRepository, keep: Cinema => Boolean,
                              view: (Cinema, Seq[CinemaMovie]) => A): Map[Cinema, Option[A]] = {
    val byVenue  = Map.newBuilder[Cinema, Option[A]]
    val complete = repository.scanVenues(keep)(_.foreach(row => row.lastSuccess.foreach(s =>
      byVenue += row.cinema -> Option.when(s.films.nonEmpty)(view(row.cinema, s.films)))))
    if (complete) byVenue.result() else Map.empty
  }
}
