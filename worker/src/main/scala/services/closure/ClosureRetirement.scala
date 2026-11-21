package services.closure

import scala.util.{Failure, Success, Try}

import services.scrapes.VenueClosure.{SourceDrop, VenueSignal}

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

  /** A closed venue no retirement can be requested for (off the roster, or no dispatch wired): page it, then record it
   *  -- recorded first, a page that failed was never sent again. */
  private def retireByHand(candidate: ClosureCandidate, line: String, raised: RaisedClosures): Unit = {
    sendAlert(s"${raised.closedLead(candidate.name)} — retire by hand. $line")
    ledger.closed(candidate.name, raised.now, line)
  }

  /** Ask for one roster directory's closed venues to be retired, in one request: paged, then recorded, once asked (a
   *  page that fails leaves the rest unrecorded, asked and paged again on the next sweep), or paged as failed, to be
   *  asked again on the next sweep. */
  private def requestRetirement(directory: RosterDirectory, requests: Seq[RetirementRequest], raised: RaisedClosures): Unit = {
    Try(dispatch.get.request(directory, requests)) match {
      case Failure(e) =>
        sendAlert(s"🪦 ${requests.map(_.name).mkString(", ")} look closed, but the retirement request failed (${e.getMessage}); retrying tomorrow.")
      case Success(_) =>
        requests.foreach { request =>
          val requested =
            if (raised.overdue(request.name)) "retirement PR requested again (close any older one still open)" else "retirement PR requested"
          sendAlert(s"${raised.closedLead(request.name)} — $requested. ${request.evidence}")
          ledger.closed(request.name, raised.now, request.evidence)
        }
    }
  }
}
