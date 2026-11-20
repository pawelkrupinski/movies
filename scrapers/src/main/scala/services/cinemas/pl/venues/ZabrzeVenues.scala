package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Zabrze: see [[PolishVenueRegistry]]. */
object ZabrzeVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0003", MultikinoZabrze),
    own(kit => new KinoRomaClient(kit.http, KinoRoma, kit.today)),
    msi(KinoPlanetCinemaZabrze, "https://zabrze.planetcinema.pl"),
  )
}
