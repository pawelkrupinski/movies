package services.closure

import play.api.Logging
import services.scrapes.VenueClosure
import services.scrapes.VenueClosure.{SourceDrop, Verdict}

import java.time.{Clock, Instant}

/**
 * Runs every rostered venue through [[VenueClosure]] once a day.
 *
 *  - A venue newly CLOSED — its sources dropped it and the venue itself shows it has
 *    closed — is paged ONCE with its evidence and, when it sits in a data-driven roster,
 *    handed to [[RetirementDispatch]], which opens the PR that removes it ([[ClosureRetirement]]). One still on the
 *    roster [[RaisedClosures.RetirementOverdue]] after that is paged and handed over again.
 *  - A venue newly SOURCE-DROPPED — its sources dropped it but nothing shows the venue
 *    closed — is paged ONCE as a coverage gap that needs a new source, and kept on the
 *    ledger, which /uptime lists. It is never retired. When the gone-venue alert has already
 *    paged that same page-gone spell ([[ClosureCandidate.pagedWhenGone]],
 *    `GoneUpstream.pagedAsGone`), the gap is recorded but not paged: one cause, one page.
 *  - A raised venue that shows life again is withdrawn and paged; one that has left the
 *    roster (the PR merged) is forgotten quietly.
 *
 * Reads the archive through [[ClosureDrops]]. An incomplete read changes nothing — a venue the read never
 * reached is not a venue with no evidence. The venue checks ([[VenueChecks]]) run only
 * for the few venues whose sources dropped them.
 */
class ClosureSweep(candidates: () => Seq[ClosureCandidate], drops: ClosureDrops, checks: VenueChecks, ledger: ClosureLedger,
                   sendAlert: String => Unit, retirement: ClosureRetirement, clock: Clock)
  extends Logging {

  /** Judge every candidate from the scrape archive: [[tools.TickRun.Incomplete]] when the archive could not be read
   *  in full (nothing judged). */
  def sweep(): tools.TickRun = {
    val now    = clock.instant()
    val byName = candidates().map(c => c.name -> c).toMap
    drops.read(byName, now) match {
      case None =>
        logger.warn("Closure sweep skipped: the scrape archive could not be read in full.")
        tools.TickRun.Incomplete
      case Some((dropped, pagedGone)) =>
        settle(byName, dropped, pagedGone, now)
        tools.TickRun.Completed
    }
  }

  /** `pagedGone`: the venues whose current page-gone spell the gone-venue alert has already paged. */
  private def settle(byName: Map[String, ClosureCandidate], drops: Map[String, Either[String, SourceDrop]], pagedGone: Set[String],
                     now: Instant): Unit = {
    val raised = RaisedClosures.of(ledger, now)
    (raised.names -- byName.keySet).foreach(ledger.withdraw)
    val verdicts = byName.map { case (name, candidate) =>
      name -> VenueClosure.judge(drops.getOrElse(name, Left("no scrape on record")), checks.signals(candidate, _))
    }
    retirement.retire(verdicts.toSeq.flatMap { case (name, verdict) =>
      ruled(name, verdict, raised, pagedGone).map { case (drop, signals) => (byName(name), drop, signals) }
    }, raised)
  }

  /** Act on one venue's verdict against what the ledger already holds for it: withdraw a closure or a coverage gap
   *  it is listed again past, record and page a new coverage gap -- or hand back the evidence of a closure to retire
   *  (a new one, or one whose retirement is overdue). */
  private def ruled(name: String, verdict: Verdict, raised: RaisedClosures,
                    pagedGone: Set[String]): Option[(SourceDrop, Seq[VenueClosure.VenueSignal])] =
    verdict match {
      case Verdict.Listed(why) if raised.closed(name) =>
        ledger.withdraw(name)
        sendAlert(s"↩️ $name no longer looks closed ($why): its retirement is withdrawn. Close its retirement PR if one is open.")
        None
      case Verdict.Listed(why) if raised.sourceDropped(name) =>
        ledger.withdraw(name)
        sendAlert(s"↩️ $name is listed again ($why): its coverage gap has closed.")
        None
      case Verdict.SourceDropped(drop, signals) if !raised.sourceDropped(name) =>
        sourceDropped(name, ClosureEvidence.line(drop, signals), wasClosed = raised.closed(name), pagedGone, raised.now)
        None
      case Verdict.Closed(drop, signals) if !raised.closed(name) || raised.overdue(name) =>
        Some((drop, signals))
      case _ => None
    }

  /** Record a venue its sources dropped but nothing shows closed, and page it -- unless the gone-venue alert already
   *  paged this 404 and there is no withdrawn closure to tell, which is news it never said. */
  private def sourceDropped(name: String, line: String, wasClosed: Boolean, pagedGone: Set[String], now: Instant): Unit = {
    ledger.sourceDropped(name, now, line)
    if (wasClosed || !pagedGone(name)) {
      val opening =
        if (wasClosed) s"↩️ $name no longer looks closed: its retirement is withdrawn (close its PR if one is open). It"
        else s"📡 $name"
      sendAlert(s"$opening is dropped by its sources but not shown closed — a coverage gap that needs a new source. $line")
    }
  }
}
