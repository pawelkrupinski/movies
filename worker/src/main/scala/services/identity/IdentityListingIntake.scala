package services.identity

import models.{Cinema, CinemaMovie, City}
import services.movies.{CacheKey, ScrapeGuardLedger, ScrapeGuardState, ScrapeLandingMetrics, ScrapeSink, TitleNormalizer}
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

  // Each archive's listings, held between projections: a projection reads only what changed.
  private val acceptedListings = new HeldListings(accepted)
  private val archivedListings = new HeldListings(archive)

  /** Every venue of `live` with the listing it is taken to publish now — its accepted listing, else the
   *  archive's — venues publishing nothing left out. Each archive is read by [[HeldListings]]: only the
   *  venues whose listing changed since the last read, and only the venues asked for (the archive's copy
   *  of a venue that has an accepted listing is never held). An archive that could not be read whole
   *  counts as empty — a partial read is not a smaller archive. */
  def listings(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = {
    val wanted          = live.map(_.displayName).toSet
    val acceptedByVenue = acceptedListings(wanted)
    val acceptedNames   = acceptedByVenue.keySet.map(_.displayName)
    val archivedByVenue = archivedListings(name => wanted(name) && !acceptedNames(name))
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedByVenue.get(c).orElse(archivedByVenue.get(c)).map(c -> _)).filter(_._2.nonEmpty)
  }

  // A venue's guard state and accepted listing are its own, so one venue's scrape waits only for
  // another of the SAME venue — never for the whole country's: a US scrape walk lands 4,462 venues.
  private val venueLocks = new StripedLocks()

  override def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String],
                                  viaFallback: Boolean): Seq[(CinemaMovie, CacheKey, Boolean)] = venueLocks.locking(Seq(cinema.displayName)) {
    // The archive has just filed this scrape, and the intake may file it below: both are read again.
    acceptedListings.forget(cinema.displayName); archivedListings.forget(cinema.displayName)
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
    Seq.empty
  }
}

object IdentityListingIntake {
  /** Where a cut-over country keeps its venues' accepted listings. */
  val Collection = "identity_listings"
}
