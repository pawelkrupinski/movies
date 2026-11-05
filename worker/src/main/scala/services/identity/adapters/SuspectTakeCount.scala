package services.identity
package adapters

import play.api.Logging

import java.time.{Clock, LocalDate, ZoneId}
import java.util.concurrent.Executor

/**
 * The live resolve's SUSPECT TAKES ([[SuspectTakes]]), counted per signal over each WRITTEN projection of the whole corpus:
 * the billings whose take a signal suspects and no hand judgement judges (`judgements`, [[TakeJudgements]]) — one judged
 * right is answered, one judged wrong is counted apart — as `report` exports them
 * (`kinowo_worker_identity_suspect_takes{signal}`), and the billings taken as a film a judgement calls wrong, suspect or
 * not, as `reportWrong` does (`kinowo_worker_identity_suspect_takes_wrong`: 0 once each is corrected). One log line
 * states the counts (`identity suspect takes: counted …`, a `suspect_<signal>` field each) and another names the billings
 * (`identity suspect takes: billings …`, the first [[SuspectTakeCount.Named]] of each kind, the most venues first), both
 * under the submitting projection's `projection_id` though another thread counts: what prod serves, read without its
 * database.
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
                             report: Map[SuspectTakes.Signal, Int] => Unit, runner: Executor, pressed: () => Boolean,
                             judgements: TakeJudgements.Index = TakeJudgements.Index.Empty, reportWrong: Int => Unit = _ => ()) extends Logging {

  /** The decisions and listings (their hashes) last counted over: a count of the same is the same. */
  private var countedOver = Option.empty[(Int, Int)]
  private val counting = new OffThreadCount[(Resolution, Seq[Listing])]("identity suspect takes", logger, runner, pressed)(
    { case (resolution, listings) =>
      val over = (resolution.decisions.##, listings.##)
      if (!countedOver.contains(over)) { apply(resolution, listings); countedOver = Some(over) }
    })

  /** Count `resolution`'s suspect takes among `listings` on `runner`, once it is free. */
  def submit(resolution: Resolution, listings: Seq[Listing]): Unit = counting.submit((resolution, listings))

  /** Count `resolution`'s suspect takes among `listings` (every listing it decided) and report them, on this thread. */
  def apply(resolution: Resolution, listings: Seq[Listing]): Unit = {
    val agreed = resolution.decisions.iterator.flatMap(_.film).filterNot(resolution.films.contains).distinct.toSeq
    val held   = if (agreed.isEmpty) Map.empty[Int, IdentityMeasures.Film] else records(agreed)
    val corpus = SuspectTakes.corpus(resolution, listings, venues, held.get)
    val year   = LocalDate.ofInstant(clock.instant(), zone).getYear
    val judged = corpus.takes.map(take => take -> judgements.verdict(take.listing.venue, take.listing.rawTitle, SuspectTakeCount.idsOf(take)))
    val verdictOf = judged.iterator.map { case (take, v) => (take.listing.venue, take.listing.rawTitle, take.film) -> v }.toMap
    val unjudged = SuspectTakes.suspected(corpus.takes, year, corpus.billings).filter { case (take, _) =>
      verdictOf.get((take.listing.venue, take.listing.rawTitle, take.film)).flatten.isEmpty }
    val counts   = SuspectTakeCount.bySignal(unjudged.map { case (take, found) => (take.listing.rawTitle, take.film) -> found })
    val wrong    = judged.collect { case (take, Some(false)) => take }
    val wrongBillings = wrong.map(take => (take.listing.rawTitle, take.film)).distinct.size
    report(counts)
    reportWrong(wrongBillings)
    val bySignal = SuspectTakes.Signal.values.toSeq.map(signal => signal -> counts.getOrElse(signal, 0))
    val total    = bySignal.map(_._2).sum
    tools.LogFields.info(logger.underlyingLogger, s"identity suspect takes: counted $total suspect billing(s) " +
      s"(${bySignal.filter(_._2 > 0).map { case (signal, n) => s"${signal.label} $n" }.mkString(", ")}), $wrongBillings judged wrong",
      ("suspected" -> total) +: ("judged_wrong" -> wrongBillings) +: bySignal.map { case (signal, n) => s"suspect_${signal.label}" -> n })
    val suspects = SuspectTakeCount.named(unjudged.flatMap { case (take, found) => found.map(take -> _.render) })
    val wrongs   = SuspectTakeCount.named(wrong.map(_ -> "judged wrong"))
    if (suspects.nonEmpty || wrongs.nonEmpty)
      tools.LogFields.info(logger.underlyingLogger, s"identity suspect takes: billings (${suspects.size} unjudged suspect, ${wrongs.size} judged wrong named)",
        Seq("suspect_billings" -> suspects.mkString("\n"), "wrong_billings" -> wrongs.mkString("\n")))
  }
}

object SuspectTakeCount {
  /** How many billings of each kind (unjudged suspects, judged wrong) the billings log line names. */
  val Named = 50

  /** Every id `take` is known by, as a judgement names one: TMDB's, IMDb's, a fallback film's, each agreeing family's. */
  def idsOf(take: SuspectTakes.Take): Set[String] =
    Set(take.film) ++ take.tmdb.map(id => s"tmdb:$id") ++ Option.when(take.record.imdbNumber > 0)(f"imdb:tt${take.record.imdbNumber}%07d") ++
      take.decision.agreed.map { case (family, id) => s"${if (family == "wiki") "wikidata" else family}:$id" }

  /** The first [[Named]] billings of `found`, the most venues first, one line each: "<billing> → <film> <title> (<year>)
   *  [<n> venue(s)]: <reasons>". */
  def named(found: Seq[(SuspectTakes.Take, String)]): Seq[String] =
    found.groupBy { case (take, _) => (take.listing.rawTitle, take.film) }.toSeq.map { case ((raw, film), all) =>
      val take   = all.head._1
      val venues = all.map(_._1.listing.venue).distinct.size
      (venues, s"$raw → $film ${take.record.title} (${take.record.year.fold("?")(_.toString)}) [$venues venue(s)]: ${all.map(_._2).distinct.mkString("; ")}")
    }.sortBy { case (venues, line) => (-venues, line) }.take(Named).map(_._2)

  /** How many billings (raw title, film) each signal suspects — every signal, 0 where it suspects none. */
  def bySignal(suspected: Seq[((String, String), Seq[SuspectTakes.Suspicion])]): Map[SuspectTakes.Signal, Int] = {
    val billings = suspected.flatMap { case (billing, found) => found.map(_.signal -> billing) }.distinct
    SuspectTakes.Signal.values.map(signal => signal -> billings.count(_._1 == signal)).toMap
  }

  /** The records `lookups` holds of the films asked for, read in ONE prefetch rather than a read per film, and let go
   *  once read: `lookups` must be the caller's own, as a prefetch drops whatever the last one held. */
  def recordsOf(lookups: IdentityLookups): Seq[Int] => Map[Int, IdentityMeasures.Film] = films => {
    lookups.prefetch(Nil, films, Nil)
    try films.flatMap(id => lookups.film(id).answered.flatten.map(id -> _)).toMap
    finally lookups.prefetchAnswered()
  }
}
