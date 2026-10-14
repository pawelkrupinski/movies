package services.sharecards

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** [[FacebookRescrapeStore]] in a map — what several simulated workers share in a spec, the way
 *  the fleet shares the Mongo collection. Same contract, pinned by `FacebookRescrapeStoreContractSpec`. */
class InMemoryFacebookRescrapeStore extends FacebookRescrapeStore {
  // key -> (entry, enqueued at)
  private val entries  = collection.mutable.LinkedHashMap.empty[String, (RescrapeEntry, Instant)]
  private var nextSlot = Option.empty[Instant]
  // key -> the latest due time a request asked for since the entry was last claimed
  private val askedAgain = collection.mutable.Map.empty[String, Instant]

  def add(added: Seq[RescrapeEntry]): Int = synchronized {
    added.count { e =>
      entries.get(e.key) match {
        case Some((waiting, enqueued)) =>
          if (e.notBefore.isAfter(waiting.notBefore)) entries(e.key) = (waiting.copy(notBefore = e.notBefore), enqueued)
          askedAgain(e.key) = askedAgain.get(e.key).filter(_.isAfter(e.notBefore)).getOrElse(e.notBefore)
          false
        case None =>
          entries(e.key) = (e.copy(attempts = 0), e.notBefore)
          true
      }
    }
  }

  private def due(country: String, kind: RescrapeKind, now: Instant) =
    entries.values.filter { case (e, _) => e.country == country && e.kind == kind && !e.notBefore.isAfter(now) }

  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean = synchronized(due(country, kind, now).nonEmpty)

  def peekDue(country: String, kind: RescrapeKind, now: Instant, limit: Int): Seq[RescrapeEntry] = synchronized {
    due(country, kind, now).toSeq.sortBy(_._2).take(limit).map(_._1)
  }

  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry] = synchronized {
    due(country, kind, now).toSeq.sortBy(_._2).headOption.map { case (e, enqueued) =>
      val claimed = e.copy(notBefore = now.plusMillis(lease.toMillis), attempts = e.attempts + 1)
      entries(e.key) = (claimed, enqueued)
      askedAgain.remove(e.key)
      claimed
    }
  }

  private def ifStillClaimed(claimed: RescrapeEntry)(f: Instant => Unit): Unit =
    entries.get(claimed.key).filter(_._1.attempts == claimed.attempts).foreach { case (_, enqueued) => f(enqueued) }

  def complete(claimed: RescrapeEntry): Unit = synchronized(ifStillClaimed(claimed) { enqueued =>
    askedAgain.remove(claimed.key) match {
      case Some(at) => entries(claimed.key) = (claimed.copy(notBefore = at, attempts = 0), enqueued)
      case None     => entries.remove(claimed.key)
    }
  })

  def discard(entry: RescrapeEntry): Unit = synchronized(ifStillClaimed(entry) { _ => entries.remove(entry.key); askedAgain.remove(entry.key) })

  def retry(claimed: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit = synchronized {
    ifStillClaimed(claimed)(enqueued =>
      entries(claimed.key) = (claimed.copy(notBefore = at, attempts = if (countAttempt) claimed.attempts else claimed.attempts - 1), enqueued))
  }

  def takeSlot(now: Instant, spacing: FiniteDuration, slack: FiniteDuration): Boolean = synchronized {
    val due = nextSlot.forall(!_.isAfter(now))
    if (due) {
      val floor = now.minusMillis(slack.toMillis)
      nextSlot = Some(nextSlot.filter(_.isAfter(floor)).getOrElse(floor).plusMillis(spacing.toMillis))
    }
    due
  }

  def holdSlots(until: Instant): Unit = synchronized { nextSlot = Some(nextSlot.fold(until)(s => if (s.isAfter(until)) s else until)) }

  def waitingPages(country: String): Long = synchronized(entries.values.count { case (e, _) => e.country == country && e.kind == RescrapeKind.Page }.toLong)

  /** Test seam: every waiting entry. */
  def waiting: Seq[RescrapeEntry] = synchronized(entries.values.map(_._1).toSeq)
}
