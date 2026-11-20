package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Slubice: see [[PolishVenueRegistry]]. */
object SlubiceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // Two towns with one cinema each and nothing else within an hour's drive —
    // pages of their own rather than a stretch onto the nearest region.
    bilety24("https://www.bilety24.pl/kino/organizator/slubicki-miejski-osrodek-kultury-1336", KinoSmok),
  )
}
