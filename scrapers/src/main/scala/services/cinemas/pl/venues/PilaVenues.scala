package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Pila: see [[PolishVenueRegistry]]. */
object PilaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Pila),   // Piła
    bilety24("https://www.bilety24.pl/kino/organizator/waleckie-centrum-kultury-1598", KinoWCKWalcz),   // Wałcz
    bilety24("https://www.bilety24.pl/kino/organizator/kino-osiedlowe-trzcianka-1559", KinoOsiedlowe),   // Trzcianka
    biletyna(KinoRodlo, "https://biletyna.pl/Zlotow/Zlotowskie-Centrum-Aktywnosci-Spolecznej"),   // Złotów
    bilety24("https://www.bilety24.pl/kino/organizator/gminny-osrodek-kultury-685", KinoMewaBudzyn),   // Budzyń
    bilety24("https://www.bilety24.pl/kino/organizator/chodzieski-dom-kultury-591", KinoNotec),   // Chodzież
    bilety24("https://www.bilety24.pl/kino/organizator/nck-w-wieleniu-1344", KinoZaRogiemWielen),   // Wieleń
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-623", KinoMDKWagrowiec),   // Wągrowiec
    bilety24("https://www.bilety24.pl/kino/organizator/mck-kino-swiatowid-831", KinoSwiatowidCzarnkow),   // Czarnków
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoOKJastrowie, "https://biletyna.pl/Jastrowie/Osrodek-Kultury"),   // Jastrowie
  )
}
