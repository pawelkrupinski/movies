package services.closure

import models.{ArchivedScrape, Cinema}
import play.api.Logging
import services.fallback.FallbackStore
import services.scrapes.VenueClosure.{FallbackEvidence, SourceDrop, VenueSignal, Verdict}
import services.scrapes.{GoneUpstream, ScrapeArchiveReader, VenueClosure}

import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import scala.util.control.NonFatal

/**
 * Runs every rostered venue through [[VenueClosure]] once a day.
 *
 *  - A venue newly CLOSED — its sources dropped it and the venue itself shows it has
 *    closed — is paged ONCE with its evidence and, when it sits in a data-driven roster,
 *    handed to [[RetirementDispatch]], which opens the PR that removes it. One still on the
 *    roster [[ClosureSweep.RetirementOverdue]] after that is paged and handed over again.
 *  - A venue newly SOURCE-DROPPED — its sources dropped it but nothing shows the venue
 *    closed — is paged ONCE as a coverage gap that needs a new source, and kept on the
 *    ledger, which /uptime lists. It is never retired. When the gone-venue alert has already
 *    paged that same page-gone spell ([[ClosureCandidate.pagedWhenGone]],
 *    [[GoneUpstream.pagedAsGone]]), the gap is recorded but not paged: one cause, one page.
 *  - A raised venue that shows life again is withdrawn and paged; one that has left the
 *    roster (the PR merged) is forgotten quietly.
 *
 * Reads the archive with `scan`, never `findAll`: only the verdicts are kept, not every
 * venue's parsed listing. An incomplete read changes nothing — a venue the read never
 * reached is not a venue with no evidence. The venue checks ([[VenueChecks]]) run only
 * for the few venues whose sources dropped them.
 */
class ClosureSweep(candidates: () => Seq[ClosureCandidate], archive: ScrapeArchiveReader, fallbacks: FallbackStore,
                   checks: VenueChecks, ledger: ClosureLedger, sendAlert: String => Unit, dispatch: Option[RetirementDispatch],
                   clock: Clock)
  extends Logging {

  /** Judge every candidate from the scrape archive: [[tools.TickRun.Incomplete]] when the archive could not be read
   *  in full (nothing judged). */
  def sweep(): tools.TickRun = {
    val now     = clock.instant()
    val byName  = candidates().map(c => c.cinema.displayName -> c).toMap
    val drops   = collection.mutable.Map.empty[String, Either[String, SourceDrop]]
    val pagedGone = collection.mutable.Set.empty[String]
    val complete = archive.scan(_.foreach { (row: ArchivedScrape) =>
      byName.get(row.cinema.displayName).foreach { c =>
        drops(c.cinema.displayName) = VenueClosure.sourceDropped(row, fallbacks.get(c.cinema.displayName), c.hasFallback, c.cinema.opening, now)
        if (c.pagedWhenGone && row.lastBarren.exists(GoneUpstream.pagedAsGone)) pagedGone += c.cinema.displayName
      }
    })
    if (!complete.isComplete) { logger.warn("Closure sweep skipped: the scrape archive could not be read in full."); tools.TickRun.Incomplete }
    else { settle(byName, drops.toMap, pagedGone.toSet, now); tools.TickRun.Completed }
  }

  /** `pagedGone`: the venues whose current page-gone spell the gone-venue alert has already paged. */
  private def settle(byName: Map[String, ClosureCandidate], drops: Map[String, Either[String, SourceDrop]], pagedGone: Set[String],
                     now: Instant): Unit = {
    val raised = ClosureSweep.Raised(ledger.entries(), now)
    (raised.entries.keySet -- byName.keySet).foreach(ledger.withdraw)
    val verdicts = byName.map { case (name, candidate) =>
      name -> VenueClosure.judge(drops.getOrElse(name, Left("no scrape on record")), checks.signals(candidate.cinema, _))
    }
    val newlyClosed = verdicts.toSeq.flatMap { case (name, verdict) =>
      ruled(name, verdict, raised, pagedGone).map { case (drop, signals) => (byName(name), drop, signals) }
    }
    val (inRoster, byHand) =
      newlyClosed.partition { case (candidate, _, _) => RosterEntry.of(candidate.cinema).isDefined && dispatch.isDefined }
    byHand.foreach { case (candidate, drop, signals) => retireByHand(candidate.cinema, drop, signals, raised) }
    inRoster.groupBy { case (candidate, _, _) => RosterEntry.of(candidate.cinema).get.directory }.foreach {
      case (directory, venues) => requestRetirement(directory, venues, raised)
    }
  }

  /** Act on one venue's verdict against what the ledger already holds for it: withdraw a closure or a coverage gap
   *  it is listed again past, record and page a new coverage gap -- or hand back the evidence of a closure to retire
   *  (a new one, or one whose retirement is overdue). */
  private def ruled(name: String, verdict: Verdict, raised: ClosureSweep.Raised,
                    pagedGone: Set[String]): Option[(SourceDrop, Seq[VenueSignal])] =
    (verdict, raised.kind(name)) match {
      case (Verdict.Listed(why), Some(LedgerKind.Closed)) =>
        ledger.withdraw(name)
        sendAlert(s"↩️ $name no longer looks closed ($why): its retirement is withdrawn. Close its retirement PR if one is open.")
        None
      case (Verdict.Listed(why), Some(LedgerKind.SourceDropped)) =>
        ledger.withdraw(name)
        sendAlert(s"↩️ $name is listed again ($why): its coverage gap has closed.")
        None
      case (Verdict.SourceDropped(drop, signals), previous) if !previous.contains(LedgerKind.SourceDropped) =>
        sourceDropped(name, ClosureSweep.evidenceLine(drop, signals), wasClosed = previous.contains(LedgerKind.Closed),
          pagedGone, raised.now)
        None
      case (Verdict.Closed(drop, signals), previous) if !previous.contains(LedgerKind.Closed) || raised.overdue(name) =>
        Some((drop, signals))
      case _ => None
    }

  /** Record a venue its sources dropped but nothing shows closed, and page it -- unless the gone-venue alert already
   *  paged this 404 and there is no withdrawn closure to tell, which is news it never said. */
  private def sourceDropped(name: String, line: String, wasClosed: Boolean, pagedGone: Set[String], now: Instant): Unit = {
    ledger.record(name, LedgerEntry(LedgerKind.SourceDropped, now, line))
    if (wasClosed || !pagedGone(name)) {
      val opening =
        if (wasClosed) s"↩️ $name no longer looks closed: its retirement is withdrawn (close its PR if one is open). It"
        else s"📡 $name"
      sendAlert(s"$opening is dropped by its sources but not shown closed — a coverage gap that needs a new source. $line")
    }
  }

  /** A closed venue no retirement can be requested for (off the roster, or no dispatch wired): record it and page it. */
  private def retireByHand(cinema: Cinema, drop: SourceDrop, signals: Seq[VenueSignal], raised: ClosureSweep.Raised): Unit = {
    val line = ClosureSweep.evidenceLine(drop, signals)
    ledger.record(cinema.displayName, LedgerEntry(LedgerKind.Closed, raised.now, line))
    sendAlert(s"${raised.closedLead(cinema.displayName)} — retire by hand. $line")
  }

  /** Ask for one roster directory's closed venues to be retired, in one request: recorded and paged once asked, or
   *  paged as failed, to be asked again on the next sweep. */
  private def requestRetirement(directory: RosterDirectory, venues: Seq[(ClosureCandidate, SourceDrop, Seq[VenueSignal])],
                                raised: ClosureSweep.Raised): Unit = {
    val requests = venues.map { case (candidate, drop, signals) =>
      RetirementRequest(RosterEntry.of(candidate.cinema).get, candidate.cinema.displayName, ClosureSweep.evidenceLine(drop, signals),
        checks.live(candidate.cinema, drop), candidate.page)
    }
    try {
      dispatch.get.request(directory, requests)
      requests.foreach { request =>
        ledger.record(request.name, LedgerEntry(LedgerKind.Closed, raised.now, request.evidence))
        val requested =
          if (raised.overdue(request.name)) "retirement PR requested again (close any older one still open)" else "retirement PR requested"
        sendAlert(s"${raised.closedLead(request.name)} — $requested. ${request.evidence}")
      }
    } catch {
      case NonFatal(e) =>
        sendAlert(s"🪦 ${venues.map(_._1.cinema.displayName).mkString(", ")} look closed, but the retirement request failed (${e.getMessage}); retrying tomorrow.")
    }
  }
}

object ClosureSweep {
  /** How long a CLOSED venue may stay on the roster before the sweep asks again. The
   *  dispatch returning 2xx only starts the workflow: its live re-check may keep the venue
   *  (and open no PR), or the PR may sit unmerged. A venue retired goes off the roster within
   *  days of the request; one still on it two weeks later needs a fresh re-check and a page,
   *  and a fortnight keeps a blip-kept venue from paging daily. While a PR is open, asking
   *  again opens a second PR — the page says so, and the older one can be closed. */
  val RetirementOverdue: java.time.Duration = java.time.Duration.ofDays(14)

  private def day(instant: Instant): LocalDate = LocalDate.ofInstant(instant, ZoneOffset.UTC)

  /** What the ledger held for each venue as the sweep began at `now`. */
  private final case class Raised(entries: Map[String, LedgerEntry], now: Instant) {
    def kind(name: String): Option[LedgerKind] = entries.get(name).map(_.kind)

    /** Still a candidate, so still on the roster, this long after its retirement was asked for. */
    def overdue(name: String): Boolean =
      entries.get(name).exists(entry => entry.kind == LedgerKind.Closed && !now.isBefore(entry.at.plus(RetirementOverdue)))

    def closedLead(name: String): String =
      if (overdue(name)) s"🪦 $name still looks closed and is still on the roster, its retirement asked for on ${day(entries(name).at)}"
      else s"🪦 $name looks closed"
  }

  /** The evidence in one line, for the page, the ledger and the PR body: what the
   *  aggregators answered, then what was seen of the venue itself, each as observed. */
  def evidenceLine(drop: SourceDrop, signals: Seq[VenueSignal]): String = {
    val fallback = drop.fallback match {
      case FallbackEvidence.ListedEmpty(since, lastSeen)    => s"its fallback has listed no screenings since ${day(since)} (last checked ${day(lastSeen)})"
      case FallbackEvidence.Dropped(source, since, _, _)    => s"its fallback $source has answered gone since ${day(since)}"
      case FallbackEvidence.NoFallback                      => "it has no fallback feed"
    }
    val lastListing = drop.lastContentAt.fold("no listing on record")(at => s"last listing ${day(at)}")
    val venue =
      if (signals.isEmpty) "Venue itself: no own website or second listing on record to check."
      else signals.map {
        case VenueSignal.Closure(observed) => s"CLOSED: $observed"
        case VenueSignal.Life(observed)    => s"OPEN: $observed"
        case VenueSignal.Silent(observed)  => s"no answer: $observed"
      }.mkString("Venue itself: ", "; ", ".")
    s"Aggregator page gone since ${day(drop.goneSince)} (${drop.primaryError}, last probed ${day(drop.lastAttemptAt)}); $fallback; $lastListing. $venue"
  }
}
