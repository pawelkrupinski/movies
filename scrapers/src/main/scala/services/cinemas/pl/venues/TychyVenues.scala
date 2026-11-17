package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Tychy: see [[PolishVenueRegistry]]. */
object TychyVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0053", MultikinoTychy),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    // Oświęcim's culture centre sells its concerts on the same instance
    systemBiletowy(KinoNaszeKino, "https://ock.systembiletowy.pl", institution = Some(Institution("Nasze Kino"))),
    msi(KinoPlanetCinema, "https://oswiecim.planetcinema.pl"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    msi(KinoAleKinoLibiaz, "https://biletylck.libiaz.pl"),   // Libiąż
  )
}
