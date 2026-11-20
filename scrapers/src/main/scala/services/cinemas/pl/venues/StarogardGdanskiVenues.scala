package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Starogard Gdanski: see [[PolishVenueRegistry]]. */
object StarogardGdanskiVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1095", CinemaCityStarogard),   // Starogard Gdański
    own(kit => new KinoRemusClient(kit.http, KinoRemus)),   // Kościerzyna
  )
}
