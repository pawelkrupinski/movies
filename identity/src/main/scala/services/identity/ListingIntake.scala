package services.identity

import models.{Cinema, CinemaMovie, ListingKey}
import services.movies.{ScrapeGuardState, ScrapeHealth, ListingIntakeMetrics, ScrapeListing}
import services.titles.TitleNormalizer

import java.time.LocalDateTime

/**
 * Which listing a cut-over country's venue is taken to publish after a scrape
 * (docs/design/identity-resolver.md §8, phase 5): the evidence the identity projection resolves.
 *
 * The scrape-health rules the since-deleted scrape landing applied, as a
 * pure function of the venue's accepted listing, the fresh scrape and the guards' state — the
 * outcome is a listing set for the projection to read, not slot writes and a skipped prune.
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

  /** What the venue's listing becomes, the guards' state after this scrape, how, and which guard
   *  stepped in (none on a healthy scrape) — counted, so a guard stuck rejecting can be alerted on. */
  final case class Verdict(accepted: Seq[CinemaMovie], guard: ScrapeGuardState, outcome: Outcome, guarded: Seq[Guarded] = Nil)

  /** One guard's decision about a scrape: it rejected it as a bad fetch, or accepted a degraded one
   *  after its grace ran out. */
  enum Guarded(val guard: String, val verdict: String) {
    case DepthReject   extends Guarded(ListingIntakeMetrics.Guard.Depth, ListingIntakeMetrics.Verdict.Reject)
    case DepthAccept   extends Guarded(ListingIntakeMetrics.Guard.Depth, ListingIntakeMetrics.Verdict.Accept)
    case BreadthReject extends Guarded(ListingIntakeMetrics.Guard.Breadth, ListingIntakeMetrics.Verdict.Reject)
    case BreadthAccept extends Guarded(ListingIntakeMetrics.Guard.Breadth, ListingIntakeMetrics.Verdict.Accept)
  }

  def decide(cinema: Cinema, known: Seq[CinemaMovie], scraped: Offer, guard: ScrapeGuardState, now: LocalDateTime,
             maxRejections: Int, normalizer: TitleNormalizer): Verdict = {
    // A film the scrape lists with no showtime has nothing to screen: landed, it is a film served as an empty card
    // (Kino Parczew's closed-day notice "Kino 01.11.2026/ nieczynne", a page with no run). The scrape is judged
    // without it.
    val offer = scraped.copy(films = scraped.films.filter(_.showtimes.nonEmpty))
    if (offer.films.isEmpty) Verdict(known, guard, Outcome.Kept)
    else if (offer.viaFallback) Verdict(union(cinema, known, offer.films), guard, Outcome.Added)
    else {
      val landed = ScrapeGuardState(sourceKey = offer.sourceKey.orElse(guard.sourceKey))
      if (known.nonEmpty && ScrapeHealth.isRewire(guard.sourceKey, offer.sourceKey)) Verdict(offer.films, landed, Outcome.Replaced)
      else guarded(cinema, known, offer, guard, landed, now, maxRejections, normalizer)
    }
  }

  /** A scrape of the venue's own source, as the depth guard and then the breadth guard judge it. */
  private def guarded(cinema: Cinema, known: Seq[CinemaMovie], offer: Offer, guard: ScrapeGuardState, landed: ScrapeGuardState,
                      now: LocalDateTime, maxRejections: Int, normalizer: TitleNormalizer): Verdict =
    ScrapeHealth.depth(upcomingShowtimes(known, now), upcomingShowtimes(offer.films, now), guard.depthRejections, maxRejections) match {
      case ScrapeHealth.Depth.Reject(consecutive) =>
        Verdict(known, guard.copy(depthRejections = consecutive), Outcome.Kept, Seq(Guarded.DepthReject))
      case depth =>
        val deep = depth match { case ScrapeHealth.Depth.AcceptDegraded(_) => Seq(Guarded.DepthAccept); case _ => Nil }
        ScrapeHealth.breadth(slotCount(cinema, known, normalizer), slotCount(cinema, offer.films, normalizer), offer.listingIsComplete,
            guard.breadthRejections, maxRejections, depth) match {
          case ScrapeHealth.Breadth.Reject(consecutive) =>
            Verdict(union(cinema, known, offer.films), landed.copy(breadthRejections = consecutive), Outcome.Added, deep :+ Guarded.BreadthReject)
          case ScrapeHealth.Breadth.AcceptDegraded(_) => Verdict(offer.films, landed, Outcome.Replaced, deep :+ Guarded.BreadthAccept)
          case ScrapeHealth.Breadth.Healthy           => Verdict(offer.films, landed, Outcome.Replaced, deep)
        }
    }

  /** The showtimes `films` hold after `now`. */
  private def upcomingShowtimes(films: Seq[CinemaMovie], now: LocalDateTime): Int = films.iterator.map(_.showtimes.count(_.dateTime.isAfter(now))).sum
  /** The venue slots `films` land in. */
  private def slotCount(cinema: Cinema, films: Seq[CinemaMovie], normalizer: TitleNormalizer): Int =
    films.map(cm => ScrapeListing.slotKey(cinema, cm.movie.title, normalizer)).distinct.size

  /** `known` with `fresh` added, a fresh row replacing the known one of its [[ListingKey]]. */
  private def union(cinema: Cinema, known: Seq[CinemaMovie], fresh: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val freshKeys = fresh.map(ListingKey.of(cinema, _)).toSet
    known.filterNot(cm => freshKeys(ListingKey.of(cinema, cm))) ++ fresh
  }
}
