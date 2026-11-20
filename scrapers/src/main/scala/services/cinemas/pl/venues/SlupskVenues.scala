package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Slupsk: see [[PolishVenueRegistry]]. */
object SlupskVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0030", MultikinoSlupsk),
    ekobilet("kinorejs", KinoRejs),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/leborskie-centrum-kultury-fregata-1683", KinoFregata),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/miejska-biblioteka-publiczna-w-lebie-1722", KinoLeba),   // Łeba
  )
}
