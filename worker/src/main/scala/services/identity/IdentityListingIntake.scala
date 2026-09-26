package services.identity

import models.{Cinema, CinemaMovie, City}
import services.movies.{CacheKey, ScrapeGuardLedger, ScrapeGuardState, ScrapeSink, TitleNormalizer}
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
  clock:         Clock
) extends ScrapeSink {

  /** The listing `cinema` is taken to publish now. */
  def listingOf(cinema: Cinema): Seq[CinemaMovie] =
    accepted.find(cinema).flatMap(_.lastSuccess).orElse(archive.find(cinema).flatMap(_.lastSuccess)).map(_.films).getOrElse(Nil)

  /** Every venue of `live` with the listing it is taken to publish, venues publishing nothing left out. */
  def listings(live: Seq[Cinema]): Seq[(Cinema, Seq[CinemaMovie])] = {
    val acceptedByVenue = accepted.findAll().flatMap(a => a.lastSuccess.map(a.cinema -> _.films)).toMap
    val archivedByVenue = archive.findAll().flatMap(a => a.lastSuccess.map(a.cinema -> _.films)).toMap
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedByVenue.get(c).orElse(archivedByVenue.get(c)).map(c -> _)).filter(_._2.nonEmpty)
  }

  override def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String],
                                  viaFallback: Boolean): Seq[(CinemaMovie, CacheKey, Boolean)] = synchronized {
    val stored = guards.get(cinema)
    val guard  = stored.getOrElse(ScrapeGuardState.Fresh)
    val known  = listingOf(cinema)
    val verdict = ListingIntake.decide(cinema, known, ListingIntake.Offer(movies, listingIsComplete, sourceKey, viaFallback), guard,
      City.localNow(cinema, clock), maxRejections, normalizer)
    // An unreadable ledger is judged as fresh, and its state is never written back over it.
    if (stored.isDefined && verdict.guard != guard) guards.put(cinema, verdict.guard)
    if (verdict.outcome != ListingIntake.Outcome.Kept && verdict.accepted != known)
      accepted.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), clock.instant(), listingComplete = true, verdict.accepted, error = None))
    Seq.empty
  }
}

object IdentityListingIntake {
  /** Where a cut-over country keeps its venues' accepted listings. */
  val Collection = "identity_listings"
}
