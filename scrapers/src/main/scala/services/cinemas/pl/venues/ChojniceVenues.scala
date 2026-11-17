package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Chojnice: see [[PolishVenueRegistry]]. */
object ChojniceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    msi(KinoChDK, "https://bilety.ckchojnice.pl"),   // Chojnice
    biletyna(KinoUciechaCzluchow, "https://biletyna.pl/Czluchow/Kino-Uciecha"),   // Człuchów
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteki-im-jana-karnowskiego-1410", KinoZaRogiemBrusy),   // Brusy
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoPromienWiecbork, "https://biletyna.pl/Wiecbork/Kino-Promien"),   // Więcbork
    biletyna(KinoCKiSSepolno, "https://biletyna.pl/Sepolno-Krajenskie/Centrum-Kultury-i-Sztuki"),   // Sępólno Krajeńskie
    biletyna(KinoGOKLipka, "https://biletyna.pl/Lipka/Gminny-Osrodek-Kultury"),   // Lipka
    biletyna(KinoTucholskiOsrodekKultury, "https://biletyna.pl/Tuchola/Tucholski-Osrodek-Kultury"),   // Tuchola
  )
}
