package services.sharecards

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** [[FacebookRescrapeStore]] in a map — what several simulated workers share in a spec, the way
 *  the fleet shares the Mongo collection. Same contract, pinned by `FacebookRescrapeStoreContractSpec`. */
class InMemoryFacebookRescrapeStore extends FacebookRescrapeStore {
  // key -> (entry, enqueued at)
  private val entries  = collection.mutable.LinkedHashMap.empty[String, (RescrapeEntry, Instant)]
  private var nextSlot = Option.empty[Instant]

  def add(added: Seq[RescrapeEntry]): Int = synchronized {
    added.count { e =>
      val fresh = !entries.contains(e.key)
      if (fresh) entries(e.key) = (e.copy(attempts = 0), e.notBefore)
      fresh
    }
  }

  private def due(country: String, kind: RescrapeKind, now: Instant) =
    entries.values.filter { case (e, _) => e.country == country && e.kind == kind && !e.notBefore.isAfter(now) }

  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean = synchronized(due(country, kind, now).nonEmpty)

  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry] = synchronized {
    due(country, kind, now).toSeq.sortBy(_._2).headOption.map { case (e, enqueued) =>
      val claimed = e.copy(notBefore = now.plusMillis(lease.toMillis), attempts = e.attempts + 1)
      entries(e.key) = (claimed, enqueued)
      claimed
    }
  }

  private def ifStillClaimed(claimed: RescrapeEntry)(f: Instant => Unit): Unit =
    entries.get(claimed.key).filter(_._1.attempts == claimed.attempts).foreach { case (_, enqueued) => f(enqueued) }

  def complete(claimed: RescrapeEntry): Unit = synchronized(ifStillClaimed(claimed)(_ => entries.remove(claimed.key)))

  def retry(claimed: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit = synchronized {
    ifStillClaimed(claimed)(enqueued =>
      entries(claimed.key) = (claimed.copy(notBefore = at, attempts = if (countAttempt) claimed.attempts else claimed.attempts - 1), enqueued))
  }

  def takeSlot(now: Instant, spacing: FiniteDuration): Boolean = synchronized {
    val due = nextSlot.forall(!_.isAfter(now))
    if (due) nextSlot = Some(now.plusMillis(spacing.toMillis))
    due
  }

  def holdSlots(until: Instant): Unit = synchronized { nextSlot = Some(nextSlot.fold(until)(s => if (s.isAfter(until)) s else until)) }

  def waitingPages(country: String): Long = synchronized(entries.values.count { case (e, _) => e.country == country && e.kind == RescrapeKind.Page }.toLong)

  /** Test seam: every waiting entry. */
  def waiting: Seq[RescrapeEntry] = synchronized(entries.values.map(_._1).toSeq)
}
