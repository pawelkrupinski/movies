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

  /** Every venue of `live` with the listing it is taken to publish, venues publishing nothing left out.
   *
   *  Both archives are read a page at a time, each page reduced to the live venues' listings before
   *  the next: the rows of venues no longer live, and the
   *  archive's copy of a venue that already has an accepted listing, are never held. An archive that
   *  could not be read whole counts as empty — a partial read is not a smaller archive. */
  def listings(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = {
    val wanted          = live.toSet
    val acceptedByVenue = IdentityListingIntake.lastListings(accepted, wanted)
    val archivedByVenue = IdentityListingIntake.lastListings(archive, c => wanted(c) && !acceptedByVenue.contains(c))
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedByVenue.get(c).orElse(archivedByVenue.get(c)).map(c -> _)).filter(_._2.nonEmpty)
  }

  // A venue's guard state and accepted listing are its own, so one venue's scrape waits only for
  // another of the SAME venue — never for the whole country's: a US scrape walk lands 4,462 venues.
  private val venueLocks = new StripedLocks()

  override def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String],
                                  viaFallback: Boolean): Seq[(CinemaMovie, CacheKey, Boolean)] = venueLocks.locking(Seq(cinema.displayName)) {
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

  /** Each `keep` venue's last successful listing in `repository`, scanned a page at a time; empty
   *  when the scan could not complete. */
  private def lastListings(repository: ScrapeArchiveRepository, keep: Cinema => Boolean): Map[Cinema, Seq[CinemaMovie]] = {
    val byVenue  = Map.newBuilder[Cinema, Seq[CinemaMovie]]
    val complete = repository.scan(_.foreach(row => if (keep(row.cinema)) row.lastSuccess.foreach(s => byVenue += row.cinema -> s.films)))
    if (complete) byVenue.result() else Map.empty
  }
}
