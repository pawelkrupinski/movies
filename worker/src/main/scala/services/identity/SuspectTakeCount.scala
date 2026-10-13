package services.identity

import play.api.Logging

import java.time.{Clock, LocalDate, ZoneId}
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal

/**
 * The live resolve's SUSPECT TAKES ([[SuspectTakes]]), counted per signal over each WRITTEN projection of the whole corpus:
 * the billings whose take a signal suspects, as `report` exports them (`kinowo_worker_identity_suspect_takes{signal}`). The
 * convergence legs fail on a new one (`tools.SuspectTakesAudit`); this says how many prod serves between recordings.
 *
 * Read as [[SuspectTakes.corpus]] reads a corpus: `venues` the venue pages the agreement stage reads, `records` the records
 * of the agreed films (which the model never decided), read in one batch. Nothing is held between counts: what the corpus
 * bills and the takes are built for one count and dropped with it.
 *
 * A projection [[submit]]s and goes on: the count runs on `runner` (a thread of its own in the worker), never on the
 * projection's path — on the US corpus it allocates ~360 MB and takes ~1.4 s. Only the latest submission waits: one
 * submitted while another waits replaces it. A count over the decisions and listings last counted is not made again, and
 * none is made while `pressed` (the heap held near its limit after its last collection): the next submission counts.
 */
final class SuspectTakeCount(venues: IdentityLookups, records: Seq[Int] => Map[Int, IdentityMeasures.Film], clock: Clock, zone: ZoneId,
                             report: Map[SuspectTakes.Signal, Int] => Unit, runner: Executor, pressed: () => Boolean) extends Logging {

  private val waiting = new AtomicReference[Option[(Resolution, Seq[Listing])]](None)
  /** The decisions and listings (their hashes) last counted over: a count of the same is the same. */
  private var countedOver = Option.empty[(Int, Int)]

  /** Count `resolution`'s suspect takes among `listings` on `runner`, once it is free. */
  def submit(resolution: Resolution, listings: Seq[Listing]): Unit =
    if (waiting.getAndSet(Some(resolution -> listings)).isEmpty) runner.execute(() => countWaiting())

  private def countWaiting(): Unit = synchronized {
    waiting.getAndSet(None).foreach { case (resolution, listings) =>
      if (pressed()) logger.info("identity suspect takes: not counted, the heap is near its limit; the next projection of the corpus counts")
      else {
        val over = (resolution.decisions.##, listings.##)
        if (!countedOver.contains(over))
          try { apply(resolution, listings); countedOver = Some(over) }
          catch { case NonFatal(e) => logger.warn("identity suspect takes: counting failed", e) }
      }
    }
  }

  /** Count `resolution`'s suspect takes among `listings` (every listing it decided) and report them, on this thread. */
  def apply(resolution: Resolution, listings: Seq[Listing]): Unit = {
    val agreed = resolution.decisions.iterator.flatMap(_.film).filterNot(resolution.films.contains).distinct.toSeq
    val held   = if (agreed.isEmpty) Map.empty[Int, IdentityMeasures.Film] else records(agreed)
    val corpus = SuspectTakes.corpus(resolution, listings, venues, held.get)
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

  /** The records `lookups` holds of the films asked for, read in ONE prefetch rather than a read per film, and let go
   *  once read: `lookups` must be the caller's own, as a prefetch drops whatever the last one held. */
  def recordsOf(lookups: IdentityLookups): Seq[Int] => Map[Int, IdentityMeasures.Film] = films => {
    lookups.prefetch(Nil, films, Nil)
    try films.flatMap(id => lookups.film(id).toOption.flatten.map(id -> _)).toMap
    finally lookups.prefetchAnswered()
  }
}
