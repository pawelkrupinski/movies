package services.sharecards

import java.time.Instant

import scala.concurrent.duration.*

import services.facebook.FacebookSlots

/** Where the fleet's re-scrapes wait, and the quota they share. Storage only: what to do with an
 *  entry is [[FacebookRescrapeDrain]]'s. */
trait FacebookRescrapeStore extends FacebookSlots {
  /** Add each entry unless one with its key is waiting already — that one is then due no sooner
   *  than either asked (a second request names a newer card, which needs its own wait); how many
   *  were added. */
  def add(entries: Seq[RescrapeEntry]): Int
  /** True when the country has an entry of `kind` due at `now`. */
  def hasDue(country: String, kind: RescrapeKind, now: Instant): Boolean
  /** The country's oldest-due entry of `kind`, leased: held back until `now + lease` and its
   *  `attempts` raised — the entry as claimed. */
  def claim(country: String, kind: RescrapeKind, now: Instant, lease: FiniteDuration): Option[RescrapeEntry]
  /** Remove a claimed entry — unless it was claimed again since, or asked for again since it was
   *  claimed: a request that lands while its page is being sent names a newer card than the one
   *  Facebook fetched, so the entry waits again, due when that request asked, its attempts reset. */
  def complete(claimed: RescrapeEntry): Unit
  /** Put a claimed entry back, due at `at`; `countAttempt = false` gives the claim back. */
  def retry(claimed: RescrapeEntry, at: Instant, countAttempt: Boolean): Unit
  /** Take the quota's next slot, true when it was due. Slots are a SCHEDULE: the one after comes
   *  `spacing` after this one, or after `now - slack` if that is later — so a taker a few seconds
   *  late does not push every later slot back, and an idle quota banks at most `slack`. */
  def takeSlot(now: Instant, spacing: FiniteDuration, slack: FiniteDuration): Boolean
  /** The country's waiting pages. */
  def waitingPages(country: String): Long
}
