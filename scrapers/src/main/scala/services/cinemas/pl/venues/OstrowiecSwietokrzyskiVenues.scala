package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Ostrowiec Swietokrzyski: see [[PolishVenueRegistry]]. */
object OstrowiecSwietokrzyskiVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-w-ostrowcu-swietokrzyskim-1389", KinoEtiuda),   // Ostrowiec Świętokrzyski
    biletyna(KinoOOK, "https://biletyna.pl/Opatow/Opatowski-Osrodek-Kultury"),   // Opatów
    ekobilet("kinostarowka", KinoStarowka),   // Sandomierz
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    systemBiletowy(KinoKadrStaszow, "https://sta.systembiletowy.pl"),   // Staszów
  )
}
