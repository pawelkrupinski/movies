package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Plock: see [[PolishVenueRegistry]]. */
object PlockVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Plock),
    noveKino("przedwiosnie", KinoPrzedwiosnie),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoZaRogiem, "https://bilety.pokis.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/kutnowski-dom-kultury-1474", KinoKDK),
    systemBiletowy(KinoKalejdoskop, "https://ckp.systembiletowy.pl"),
    msi(KinoODEON, "https://kinoodeon.eurobilet.pl"),
  )
}
