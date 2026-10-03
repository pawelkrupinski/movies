package tools

import java.time.{Clock, Instant, LocalDate, ZoneOffset}

/** A clock for specs whose code under test needs one but whose assertions don't depend on which
 *  instant it reads — only that it never moves with the wall clock, so no spec's verdict depends
 *  on the time of day it runs (the read-model projector's content check picks its slice by it). */
object SpecClock {
  val Pinned: Clock = Clock.fixed(Instant.parse("2026-09-07T00:00:00Z"), ZoneOffset.UTC)

  /** [[Pinned]]'s day — the `today` a scraper spec hands a client whose parse does not depend on it. */
  val PinnedDay: LocalDate = LocalDate.ofInstant(Pinned.instant(), ZoneOffset.UTC)
}
