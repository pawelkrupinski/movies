package services.movies

import java.time.{Clock, LocalDate, ZoneOffset}

/** The latest release year a title may name: next year, in UTC, read from `clock` at each ask. A plausibility cap on a
 *  year read out of a title (`EmbeddedYear`, `SequelMarker`) — only New Year moves it, never a venue's day — so it
 *  reads no venue's zone. Its own file, not `models.VenueClock`: the identity rules version digests everything the
 *  title readers reach, and VenueClock changes for venue timezone fixes no identity decision reads.
 *
 *  The title readers are pure and take the year itself (`latestYear: Int`); a caller reads it off the clock it was
 *  given. The model accessors that reach them through a venue's representative slot (`MovieRecord.cinemaData`) take
 *  an instance as a context parameter, so their many readers name no clock. */
final class LatestTitleYear(clock: Clock) {
  def value: Int = LatestTitleYear.of(clock)
}

object LatestTitleYear {
  def of(clock: Clock): Int = LocalDate.ofInstant(clock.instant(), ZoneOffset.UTC).getYear + 1

  /** The year of the context's [[LatestTitleYear]], for a pure title reader's `latestYear`. */
  def current(using latestYear: LatestTitleYear): Int = latestYear.value
}
