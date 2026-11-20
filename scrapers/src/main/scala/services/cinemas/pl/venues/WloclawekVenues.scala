package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Wloclawek: see [[PolishVenueRegistry]]. */
object WloclawekVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0008", MultikinoWloclawek),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoJutrzenka, "https://kino.sierpc.pl"),
    biletyna(KinoNawojka, "https://biletyna.pl/Lipno/Kino-Nawojka"),
    msi(KinoNoweKinoWarszawa, "http://bilety.mck-gostynin.pl"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/rypinski-dom-kultury-1053", KinoBaltykRypin),   // Rypin
  )
}
