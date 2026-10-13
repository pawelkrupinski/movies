package services.identity

import java.time.{Clock, LocalDate, ZoneId}

/**
 * The live resolve's SUSPECT TAKES ([[SuspectTakes]]), counted per signal on every projection of the whole corpus: the
 * billings whose take a signal suspects, as `report` exports them (`kinowo_worker_identity_suspect_takes{signal}`). The
 * convergence legs fail on a new one (`tools.SuspectTakesAudit`); this says how many prod serves between recordings.
 *
 * Read as [[SuspectTakes.corpus]] reads a corpus: `venues` the venue pages the agreement stage reads, `recordOf` an agreed
 * film's record. Nothing is held between counts: what the corpus bills and the takes are built for one count and dropped with it.
 */
final class SuspectTakeCount(venues: IdentityLookups, recordOf: Int => Option[IdentityMeasures.Film], clock: Clock, zone: ZoneId,
                             report: Map[SuspectTakes.Signal, Int] => Unit) {

  /** Count `resolution`'s suspect takes among `listings` (every listing it decided) and report them. */
  def apply(resolution: Resolution, listings: Seq[Listing]): Unit = {
    val corpus = SuspectTakes.corpus(resolution, listings, venues, recordOf)
    val year   = LocalDate.ofInstant(clock.instant(), zone).getYear
    report(SuspectTakeCount.bySignal(SuspectTakes.suspected(corpus.takes, year, corpus.billings).map { case (take, found) =>
      (take.listing.rawTitle, take.film) -> found }))
  }
}

object SuspectTakeCount {
  /** How many billings (raw title, film) each signal suspects — every signal, 0 where it suspects none. */
  def bySignal(suspected: Seq[((String, String), Seq[SuspectTakes.Suspicion])]): Map[SuspectTakes.Signal, Int] = {
    val billings = suspected.flatMap { case (billing, found) => found.map(_.signal -> billing) }.distinct
    SuspectTakes.Signal.values.map(signal => signal -> billings.count(_._1 == signal)).toMap
  }
}
