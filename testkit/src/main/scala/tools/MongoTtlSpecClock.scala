package tools

import java.time.{Clock, Instant, ZoneOffset}

/** The pinned clock for a spec whose rows land in a Mongo collection with a TTL index.
 *
 *  Mongo's TTL monitor deletes by the SERVER's own clock, about once a minute, and no spec can pin
 *  that. A row stamped from [[SpecClock.Pinned]] (a date already past) is expired the moment it is
 *  written, so whether it is still there when the spec reads it back depends on whether a TTL pass
 *  happened to land in between — a flake that grows with a loaded run's longer windows (the auth
 *  exchange code spec's held redeem lost its code this way in a parallel `itAll`). Pinned a century
 *  out instead, every such row outlives the run. `NoTtlExpiredRowsInSpecsSpec` holds every `it/`
 *  spec naming a TTL-owning class to it. */
object MongoTtlSpecClock {
  val Pinned: Clock = Clock.fixed(Instant.parse("2126-09-07T00:00:00Z"), ZoneOffset.UTC)
}
