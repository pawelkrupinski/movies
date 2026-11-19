package services.closure

import java.time.Instant

/** What the ledger held for each venue as a sweep began at `now`. */
final case class RaisedClosures(entries: Map[String, LedgerEntry], now: Instant) {
  /** The venues the ledger holds. */
  def names: Set[String] = entries.keySet

  def closed(name: String): Boolean        = kind(name).contains(LedgerKind.Closed)
  def sourceDropped(name: String): Boolean = kind(name).contains(LedgerKind.SourceDropped)

  private def kind(name: String): Option[LedgerKind] = entries.get(name).map(_.kind)

  /** Still a candidate, so still on the roster, this long after its retirement was asked for. */
  def overdue(name: String): Boolean =
    entries.get(name).exists(entry => entry.kind == LedgerKind.Closed && !now.isBefore(entry.at.plus(RaisedClosures.RetirementOverdue)))

  def closedLead(name: String): String =
    if (overdue(name)) s"🪦 $name still looks closed and is still on the roster, its retirement asked for on ${ClosureEvidence.day(entries(name).at)}"
    else s"🪦 $name looks closed"
}

object RaisedClosures {
  /** What `ledger` holds as a sweep begins at `now`. */
  def of(ledger: ClosureLedger, now: Instant): RaisedClosures = RaisedClosures(ledger.entries(), now)

  /** How long a CLOSED venue may stay on the roster before the sweep asks again. The
   *  dispatch returning 2xx only starts the workflow: its live re-check may keep the venue
   *  (and open no PR), or the PR may sit unmerged. A venue retired goes off the roster within
   *  days of the request; one still on it two weeks later needs a fresh re-check and a page,
   *  and a fortnight keeps a blip-kept venue from paging daily. While a PR is open, asking
   *  again opens a second PR — the page says so, and the older one can be closed. */
  val RetirementOverdue: java.time.Duration = java.time.Duration.ofDays(14)
}
