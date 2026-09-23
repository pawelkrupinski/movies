package services.cinemas.roster

import models.{Cinema, KinoMikro, MikroBronowice}

/**
 * Pairs of roster venues that LOOK like one screen listed twice — by name (the offline roster
 * audit, `CinemaRosterAuditSpec`) or by programme (`services.metrics.DuplicateVenueCensus`) —
 * and are genuinely two. One list for both checks, so a pair cleared by one is cleared by the
 * other and the reason is written down once.
 */
object DistinctVenuePairs {
  val all: Set[Set[Cinema]] = Set(
    Set(KinoMikro, MikroBronowice),   // Kino Mikro (Juliusza Lea) and its second screen in Bronowice
  )

  def contains(a: Cinema, b: Cinema): Boolean = all(Set(a, b))
}
