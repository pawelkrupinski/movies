package services.closure

import models.Cinema
import play.api.Logging
import services.fallback.FallbackStore
import services.scrapes.VenueClosure.{ClosureEvidence, FallbackEvidence}
import services.scrapes.{ArchivedScrape, ScrapeArchiveRepository, VenueClosure}

import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import scala.util.control.NonFatal

/** A rostered venue the sweep judges, and whether it has a fallback feed to corroborate it. */
final case class ClosureCandidate(cinema: Cinema, hasFallback: Boolean)

/**
 * Runs every rostered venue through [[VenueClosure]] once a day. A venue newly confirmed
 * closed is paged ONCE with its evidence and, when it sits in a data-driven roster,
 * handed to [[RetirementDispatch]], which opens the PR that removes it. A confirmed venue
 * that shows life again before that PR merges is withdrawn and paged; one that has left
 * the roster (the PR merged) is forgotten quietly.
 *
 * Reads the archive with `scan`, never `findAll`: only the verdicts are kept, not every
 * venue's parsed listing. An incomplete read changes nothing — a venue the read never
 * reached is not a venue with no evidence.
 */
class ClosureSweep(candidates: () => Seq[ClosureCandidate], archive: ScrapeArchiveRepository, fallbacks: FallbackStore,
                   ledger: ClosureLedger, notify: String => Unit, dispatch: Option[RetirementDispatch], clock: Clock)
  extends Logging {

  def sweep(): Unit = {
    val now     = clock.instant()
    val byName  = candidates().map(c => c.cinema.displayName -> c).toMap
    val verdicts = collection.mutable.Map.empty[String, Either[String, ClosureEvidence]]
    val complete = archive.scan(_.foreach { (row: ArchivedScrape) =>
      byName.get(row.cinema.displayName).foreach(c =>
        verdicts(c.cinema.displayName) = VenueClosure.judge(row, fallbacks.get(c.cinema.displayName), c.hasFallback, now))
    })
    if (!complete) logger.warn("Closure sweep skipped: the scrape archive could not be read in full.")
    else settle(byName, verdicts.toMap, now)
  }

  private def settle(byName: Map[String, ClosureCandidate], verdicts: Map[String, Either[String, ClosureEvidence]], now: Instant): Unit = {
    val confirmed = ledger.confirmed()
    (confirmed.keySet -- byName.keySet).foreach(ledger.withdraw)

    confirmed.keySet.intersect(byName.keySet).foreach { name =>
      verdicts.getOrElse(name, Left("no scrape on record")).left.foreach { why =>
        ledger.withdraw(name)
        notify(s"↩️ $name no longer looks closed ($why): its retirement is withdrawn. Close its retirement PR if one is open.")
      }
    }

    val newlyClosed = verdicts.collect { case (name, Right(evidence)) if !confirmed.contains(name) => byName(name).cinema -> evidence }.toSeq
    val (inRoster, byHand) = newlyClosed.partition { case (cinema, _) => RosterEntry.of(cinema).isDefined && dispatch.isDefined }

    byHand.foreach { case (cinema, evidence) =>
      ledger.confirm(cinema.displayName, now)
      notify(s"🪦 ${cinema.displayName} looks closed — retire by hand. ${ClosureSweep.evidenceLine(evidence)}")
    }

    inRoster.groupBy { case (cinema, _) => RosterEntry.of(cinema).get.directory }.foreach { case (directory, venues) =>
      val requests = venues.map { case (cinema, evidence) => RetirementRequest(RosterEntry.of(cinema).get, cinema.displayName, evidence) }
      try {
        dispatch.get.request(directory, requests)
        venues.foreach { case (cinema, evidence) =>
          ledger.confirm(cinema.displayName, now)
          notify(s"🪦 ${cinema.displayName} looks closed — retirement PR requested. ${ClosureSweep.evidenceLine(evidence)}")
        }
      } catch {
        case NonFatal(e) =>
          notify(s"🪦 ${venues.map(_._1.displayName).mkString(", ")} look closed, but the retirement request failed (${e.getMessage}); retrying tomorrow.")
      }
    }
  }
}

object ClosureSweep {
  private def day(instant: Instant): LocalDate = LocalDate.ofInstant(instant, ZoneOffset.UTC)

  /** The evidence in one line, for the page and the PR body. */
  def evidenceLine(evidence: ClosureEvidence): String = {
    val fallback = evidence.fallback match {
      case FallbackEvidence.ListedEmpty(since, lastSeen) => s"its fallback has listed no screenings since ${day(since)} (last checked ${day(lastSeen)})"
      case FallbackEvidence.NoFallback                   => "it has no fallback feed"
    }
    val lastListing = evidence.lastContentAt.fold("no listing on record")(at => s"last listing ${day(at)}")
    s"Primary page gone since ${day(evidence.goneSince)} (${evidence.primaryError}, last probed ${day(evidence.lastAttemptAt)}); $fallback; $lastListing."
  }
}
