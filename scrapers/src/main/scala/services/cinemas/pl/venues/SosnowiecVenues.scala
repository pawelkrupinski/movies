package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Sosnowiec: see [[PolishVenueRegistry]]. */
object SosnowiecVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Sosnowiec),
    cinemaCity("1083", CinemaCitySosnowiec),
  )
}
