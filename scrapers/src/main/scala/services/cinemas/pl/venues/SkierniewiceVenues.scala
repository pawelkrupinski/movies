package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Skierniewice: see [[PolishVenueRegistry]]. */
object SkierniewiceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // Filmweb (320 / 3149) went empty for both on ~2026-09-22; biletyna sells
    // every ticket for the cinema and for the CKiS concert hall's film nights.
    biletyna(KinoPolonez, "https://biletyna.pl/Skierniewice/Kinoteatr-Polonez"),   // Skierniewice
    biletyna(KinoCKiSSkierniewice, "https://biletyna.pl/Skierniewice/Centrum-Kultury-i-Sztuki-Sala-koncertowa"),   // Skierniewice
    biletyna(KinoLen, "https://biletyna.pl/Zyrardow/Kino-Len"),   // Żyrardów
    own(kit => new KinoRomaRawaClient(kit.http, KinoRomaRawa, kit.today)),   // Rawa Mazowiecka
    systemBiletowy(KinoFenix, "https://lok.systembiletowy.pl"),   // Łowicz
  )
}
