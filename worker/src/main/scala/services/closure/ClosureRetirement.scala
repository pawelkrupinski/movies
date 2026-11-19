package services.closure

import services.scrapes.VenueClosure.{SourceDrop, VenueSignal}

import scala.util.control.NonFatal

/** The closed venues a sweep found, retired: a venue in a data-driven roster handed to [[RetirementDispatch]] (one
 *  request per roster directory), one off the roster -- or with no dispatch wired -- recorded and paged to be retired
 *  by hand. */
final class ClosureRetirement(ledger: ClosureLedger, sendAlert: String => Unit, dispatch: Option[RetirementDispatch],
                              checks: VenueChecks) {

  def retire(closed: Seq[(ClosureCandidate, SourceDrop, Seq[VenueSignal])], raised: RaisedClosures): Unit = {
    val asked = closed.map { case (candidate, drop, signals) =>
      val evidence = ClosureEvidence.line(drop, signals)
      (candidate, evidence, dispatch.flatMap(_ => checks.retirement(candidate, drop, evidence)))
    }
    asked.foreach { case (candidate, evidence, request) => if (request.isEmpty) retireByHand(candidate, evidence, raised) }
    asked.flatMap(_._3).groupBy(_.directory).foreach { case (directory, requests) => requestRetirement(directory, requests, raised) }
  }

  /** A closed venue no retirement can be requested for (off the roster, or no dispatch wired): record it and page it. */
  private def retireByHand(candidate: ClosureCandidate, line: String, raised: RaisedClosures): Unit = {
    ledger.closed(candidate.name, raised.now, line)
    sendAlert(s"${raised.closedLead(candidate.name)} — retire by hand. $line")
  }

  /** Ask for one roster directory's closed venues to be retired, in one request: recorded and paged once asked, or
   *  paged as failed, to be asked again on the next sweep. */
  private def requestRetirement(directory: RosterDirectory, requests: Seq[RetirementRequest], raised: RaisedClosures): Unit =
    try {
      dispatch.get.request(directory, requests)
      requests.foreach { request =>
        ledger.closed(request.name, raised.now, request.evidence)
        val requested =
          if (raised.overdue(request.name)) "retirement PR requested again (close any older one still open)" else "retirement PR requested"
        sendAlert(s"${raised.closedLead(request.name)} — $requested. ${request.evidence}")
      }
    } catch {
      case NonFatal(e) =>
        sendAlert(s"🪦 ${requests.map(_.name).mkString(", ")} look closed, but the retirement request failed (${e.getMessage}); retrying tomorrow.")
    }
}
