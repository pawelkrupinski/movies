package services.movies

import java.time.{Clock, LocalDate, ZoneOffset}

/** The latest release year a title may name: next year, in UTC, read from `clock` at each ask. A plausibility cap on a
 *  year read out of a title (`EmbeddedYear`, `SequelMarker`) — only New Year moves it, never a venue's day — so it
 *  reads no venue's zone. Its own file, not `models.VenueClock`: the identity rules version digests everything the
 *  title readers reach, and VenueClock changes for venue timezone fixes no identity decision reads. */
object LatestTitleYear {
  def of(clock: Clock): Int = LocalDate.ofInstant(clock.instant(), ZoneOffset.UTC).getYear + 1
}
