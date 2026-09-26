package services.identity

import models.{Cinema, CinemaMovie}
import services.movies.{ListingKey, ScrapeGuardState, ScrapeHealth, ScrapeListing, TitleNormalizer}

import java.time.LocalDateTime

/**
 * Which listing a cut-over country's venue is taken to publish after a scrape
 * (docs/design/identity-resolver.md §8, phase 5): the evidence the identity projection resolves.
 *
 * The same scrape-health rules the old landing applies (`ScrapeLanding.recordCinemaScrape`), as a
 * pure function of the venue's accepted listing, the fresh scrape and the guards' state — only the
 * outcome differs in kind: the landing wrote slots and skipped a prune, this decides the listing
 * set the projection reads.
 *
 *  - an EMPTY scrape is a silent failure, never "nothing on": the accepted listing stays;
 *  - a FALLBACK-served scrape (the primary is down) is ADDED to the accepted listing — the fresh
 *    row wins per [[ListingKey]] — and leaves the guards' state alone;
 *  - a scrape from a different upstream (`ScrapeHealth.isRewire`) replaces the listing, guards
 *    standing aside;
 *  - the DEPTH guard (`ScrapeHealth.depth`) discards a scrape whose upcoming showtimes collapsed,
 *    until its grace runs out;
 *  - the BREADTH guard (`ScrapeHealth.breadth`) lands a short or known-incomplete scrape
 *    ADDITIVELY — what it failed to mention keeps its rows — until its grace runs out; an
 *    incomplete scrape never replaces.
 *
 * A healthy scrape replaces the venue's listing: a film the venue stopped listing leaves it.
 */
object ListingIntake {

  /** One scrape of one venue, as the runner hands it over. */
  final case class Offer(films: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String], viaFallback: Boolean)

  enum Outcome {
    /** The scrape is the venue's listing now. */
    case Replaced
    /** The scrape was added to the listing: nothing it failed to mention is withdrawn. */
    case Added
    /** The scrape was discarded; the listing is unchanged. */
    case Kept
  }

  /** What the venue's listing becomes, the guards' state after this scrape, and how. */
  final case class Verdict(accepted: Seq[CinemaMovie], guard: ScrapeGuardState, outcome: Outcome)

  def decide(cinema: Cinema, known: Seq[CinemaMovie], offer: Offer, guard: ScrapeGuardState, now: LocalDateTime,
             maxRejections: Int, normalizer: TitleNormalizer): Verdict = {
    def added = union(cinema, known, offer.films)
    if (offer.films.isEmpty) Verdict(known, guard, Outcome.Kept)
    else if (offer.viaFallback) Verdict(added, guard, Outcome.Added)
    else {
      val landed = ScrapeGuardState(sourceKey = offer.sourceKey.orElse(guard.sourceKey))
      if (known.nonEmpty && ScrapeHealth.isRewire(guard.sourceKey, offer.sourceKey)) Verdict(offer.films, landed, Outcome.Replaced)
      else {
        def upcoming(films: Seq[CinemaMovie]) = films.iterator.map(_.showtimes.count(_.dateTime.isAfter(now))).sum
        def slots(films: Seq[CinemaMovie])    = films.map(cm => ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)).distinct.size
        ScrapeHealth.depth(upcoming(known), upcoming(offer.films), guard.depthRejections, maxRejections) match {
          case ScrapeHealth.Depth.Reject(consecutive) => Verdict(known, guard.copy(depthRejections = consecutive), Outcome.Kept)
          case depth =>
            ScrapeHealth.breadth(slots(known), slots(offer.films), offer.listingIsComplete, guard.breadthRejections, maxRejections, depth) match {
              case ScrapeHealth.Breadth.Reject(consecutive) => Verdict(added, landed.copy(breadthRejections = consecutive), Outcome.Added)
              case _                                        => Verdict(offer.films, landed, Outcome.Replaced)
            }
        }
      }
    }
  }

  /** `known` with `fresh` added, a fresh row replacing the known one of its [[ListingKey]]. */
  private def union(cinema: Cinema, known: Seq[CinemaMovie], fresh: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val freshKeys = fresh.map(ListingKey.of(cinema, _)).toSet
    known.filterNot(cm => freshKeys(ListingKey.of(cinema, cm))) ++ fresh
  }
}
