package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Zakopane: see [[PolishVenueRegistry]]. */
object ZakopaneVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    biletyna(KinoSokolZakopane, "https://biletyna.pl/Zakopane/Kino-Sokol"),   // Zakopane
    // Filmweb had fallen out of step with the box office for these six by
    // 2026-09-26 (one or two stray slots against a full biletyna week).
    biletyna(KinoGiewont, "https://biletyna.pl/Zakopane/Kino-Giewont"),   // Zakopane
    biletyna(KinoMiejsce, "https://biletyna.pl/Zakopane/Kino-Miejsce"),   // Zakopane
    bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-sportu-i-promocji-w-rabce-zdroju-625", KinoSniezkaRabka),   // Rabka-Zdrój
    msi(KinoStopiak, "https://bilety.stopiakcinema.pl"),   // Nowy Targ — own site links bilety.stopiakcinema.pl, a standard MSI portal
  )
}
