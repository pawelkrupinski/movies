package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Sosnowiec: see [[PolishVenueRegistry]]. */
object SosnowiecVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Sosnowiec),
    cinemaCity("1083", CinemaCitySosnowiec),
  )
}
