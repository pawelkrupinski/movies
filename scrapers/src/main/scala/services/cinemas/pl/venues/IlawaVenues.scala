package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Ilawa: see [[PolishVenueRegistry]]. */
object IlawaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    msi(KinoteatrPasja, "https://biletyick.miastoilawa.pl"),   // Iława
    ekobilet("mok-lubawa", KinoPokoj),   // Lubawa
    bilety24("https://www.bilety24.pl/kino/organizator/miejskie-centrum-kultury-1291", KinoteatrHarmonia),   // Nowe Miasto Lubawskie
  )
}
