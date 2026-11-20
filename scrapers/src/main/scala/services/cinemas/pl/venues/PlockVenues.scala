package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Plock: see [[PolishVenueRegistry]]. */
object PlockVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Plock),
    noveKino("przedwiosnie", KinoPrzedwiosnie),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoZaRogiem, "https://bilety.pokis.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/kutnowski-dom-kultury-1474", KinoKDK),
    systemBiletowy(KinoKalejdoskop, "https://ckp.systembiletowy.pl"),
    msi(KinoODEON, "https://kinoodeon.eurobilet.pl"),
  )
}
