package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Suwalki: see [[PolishVenueRegistry]]. */
object SuwalkiVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // Filmweb-only until 2026-09-27 (Iława's expired leaf is pinned in TlsTrust).
    msi(KinoCinemaLumiereSuwalki, "https://suwalki.cinema-lumiere.pl"),   // Suwałki
    own(kit => new KinoIskraClient(kit.http, KinoIskra, kit.today)),   // Augustów
    bilety24("https://www.bilety24.pl/kino/organizator/dom-kultury-w-goldapi-1089", KinoKulturaGoldap),   // Gołdap
    ekobilet("mazury-olecko", KinoMazur),   // Olecko
    biletyna(KinoECK, "https://biletyna.pl/Elk/Elckie-Centrum-Kultury"),   // Ełk
    msi(KinoPlanetCinemaElk, "https://elk.planetcinema.pl"),   // Ełk
  )
}
