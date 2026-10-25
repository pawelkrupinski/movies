package services.closure

import models.Cinema
import play.api.Logging
import services.fallback.FallbackStore
import services.scrapes.VenueClosure.{FallbackEvidence, SourceDrop, VenueSignal, Verdict}
import services.scrapes.{ArchivedScrape, ScrapeArchiveRepository, VenueClosure}

import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import scala.util.control.NonFatal

/** A rostered venue the sweep judges, whether it has a fallback feed to corroborate it, and
 *  the page its primary scrapes (`CinemaScraper.sourceUrl`) — the page whose 404 is the
 *  source-drop evidence, so the one the retirement workflow re-checks. */
final case class ClosureCandidate(cinema: Cinema, hasFallback: Boolean, page: Option[String] = None)

/**
 * Runs every rostered venue through [[VenueClosure]] once a day.
 *
 *  - A venue newly CLOSED — its sources dropped it and the venue itself shows it has
 *    closed — is paged ONCE with its evidence and, when it sits in a data-driven roster,
 *    handed to [[RetirementDispatch]], which opens the PR that removes it. One still on the
 *    roster [[ClosureSweep.RetirementOverdue]] after that is paged and handed over again.
 *  - A venue newly SOURCE-DROPPED — its sources dropped it but nothing shows the venue
 *    closed — is paged ONCE as a coverage gap that needs a new source, and kept on the
 *    ledger, which /uptime lists. It is never retired.
 *  - A raised venue that shows life again is withdrawn and paged; one that has left the
 *    roster (the PR merged) is forgotten quietly.
 *
 * Reads the archive with `scan`, never `findAll`: only the verdicts are kept, not every
 * venue's parsed listing. An incomplete read changes nothing — a venue the read never
 * reached is not a venue with no evidence. The venue checks ([[VenueChecks]]) run only
 * for the few venues whose sources dropped them.
 */
class ClosureSweep(candidates: () => Seq[ClosureCandidate], archive: ScrapeArchiveRepository, fallbacks: FallbackStore,
                   checks: VenueChecks, ledger: ClosureLedger, sendAlert: String => Unit, dispatch: Option[RetirementDispatch],
                   clock: Clock)
  extends Logging {

  /** Judge every candidate from the scrape archive: [[tools.TickRun.Incomplete]] when the archive could not be read
   *  in full (nothing judged). */
  def sweep(): tools.TickRun = {
    val now     = clock.instant()
    val byName  = candidates().map(c => c.cinema.displayName -> c).toMap
    val drops   = collection.mutable.Map.empty[String, Either[String, SourceDrop]]
    val complete = archive.scan(_.foreach { (row: ArchivedScrape) =>
      byName.get(row.cinema.displayName).foreach(c =>
        drops(c.cinema.displayName) = VenueClosure.sourceDropped(row, fallbacks.get(c.cinema.displayName), c.hasFallback, c.cinema.opening, now))
    })
    if (!complete.isComplete) { logger.warn("Closure sweep skipped: the scrape archive could not be read in full."); tools.TickRun.Incomplete }
    else { settle(byName, drops.toMap, now); tools.TickRun.Completed }
  }

  private def settle(byName: Map[String, ClosureCandidate], drops: Map[String, Either[String, SourceDrop]], now: Instant): Unit = {
    val raised = ledger.entries()
    (raised.keySet -- byName.keySet).foreach(ledger.withdraw)
    // Still a candidate, so still on the roster, this long after its retirement was asked for.
    def overdue(name: String): Boolean =
      raised.get(name).exists(entry => entry.kind == LedgerKind.Closed && !now.isBefore(entry.at.plus(ClosureSweep.RetirementOverdue)))
    def closedLead(name: String): String =
      if (overdue(name)) s"🪦 $name still looks closed and is still on the roster, its retirement asked for on ${ClosureSweep.day(raised(name).at)}"
      else s"🪦 $name looks closed"

    val verdicts = byName.map { case (name, candidate) =>
      name -> VenueClosure.judge(drops.getOrElse(name, Left("no scrape on record")), checks.signals(candidate.cinema, _))
    }
    val newlyClosed = collection.mutable.ListBuffer.empty[(Cinema, SourceDrop, Seq[VenueSignal])]

    verdicts.foreach { case (name, verdict) =>
      (verdict, raised.get(name).map(_.kind)) match {
        case (Verdict.Listed(why), Some(LedgerKind.Closed)) =>
          ledger.withdraw(name)
          sendAlert(s"↩️ $name no longer looks closed ($why): its retirement is withdrawn. Close its retirement PR if one is open.")
        case (Verdict.Listed(why), Some(LedgerKind.SourceDropped)) =>
          ledger.withdraw(name)
          sendAlert(s"↩️ $name is listed again ($why): its coverage gap has closed.")
        case (Verdict.SourceDropped(drop, signals), previous) if !previous.contains(LedgerKind.SourceDropped) =>
          val line = ClosureSweep.evidenceLine(drop, signals)
          ledger.record(name, LedgerEntry(LedgerKind.SourceDropped, now, line))
          val opening =
            if (previous.contains(LedgerKind.Closed)) s"↩️ $name no longer looks closed: its retirement is withdrawn (close its PR if one is open). It"
            else s"📡 $name"
          sendAlert(s"$opening is dropped by its sources but not shown closed — a coverage gap that needs a new source. $line")
        case (Verdict.Closed(drop, signals), previous) if !previous.contains(LedgerKind.Closed) || overdue(name) =>
          newlyClosed += ((byName(name).cinema, drop, signals))
        case _ => ()
      }
    }

    val (inRoster, byHand) = newlyClosed.toSeq.partition { case (cinema, _, _) => RosterEntry.of(cinema).isDefined && dispatch.isDefined }

    byHand.foreach { case (cinema, drop, signals) =>
      val line = ClosureSweep.evidenceLine(drop, signals)
      ledger.record(cinema.displayName, LedgerEntry(LedgerKind.Closed, now, line))
      sendAlert(s"${closedLead(cinema.displayName)} — retire by hand. $line")
    }

    inRoster.groupBy { case (cinema, _, _) => RosterEntry.of(cinema).get.directory }.foreach { case (directory, venues) =>
      val requests = venues.map { case (cinema, drop, signals) =>
        RetirementRequest(RosterEntry.of(cinema).get, cinema.displayName, ClosureSweep.evidenceLine(drop, signals), checks.live(cinema, drop),
          byName(cinema.displayName).page)
      }
      try {
        dispatch.get.request(directory, requests)
        requests.foreach { request =>
          ledger.record(request.name, LedgerEntry(LedgerKind.Closed, now, request.evidence))
          val requested = if (overdue(request.name)) "retirement PR requested again (close any older one still open)" else "retirement PR requested"
          sendAlert(s"${closedLead(request.name)} — $requested. ${request.evidence}")
        }
      } catch {
        case NonFatal(e) =>
          sendAlert(s"🪦 ${venues.map(_._1.displayName).mkString(", ")} look closed, but the retirement request failed (${e.getMessage}); retrying tomorrow.")
      }
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
