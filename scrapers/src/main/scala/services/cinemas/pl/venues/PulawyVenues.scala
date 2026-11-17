package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Pulawy: see [[PolishVenueRegistry]]. */
object PulawyVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    noveKino("sybilla", KinoSybilla),   // Puławy
    ekobilet("kino-renesans", KinoRenesans),   // Ryki
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    ekobilet("ock-opolelubelskie", KinoOpolanka),   // Opole Lubelskie
  )
}
