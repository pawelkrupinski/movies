package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Jelenia Gora: see [[PolishVenueRegistry]]. */
object JeleniaGoraVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.JeleniaGora),
    bilety24Subdomain(KinoLot, "https://kino-lot.bilety24.pl"),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/kino-wawel-1489", KinoWawel),
  )
}
