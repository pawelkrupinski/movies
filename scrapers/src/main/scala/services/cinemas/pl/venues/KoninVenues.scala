package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Konin: see [[PolishVenueRegistry]]. */
object KoninVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // Konin + its catchment: Helios via the chain client, and each independent off
    // its own ticketing listing (bilety24 organiser / biletyna), Filmweb only where
    // it has none. Neighbouring bilety24 organiser ids are different towns: 1626 is
    // Konin's culture centre, 1621 Koło's MDK. Września's Kino Trójka (1698) is
    // intentionally not wired.
    helios(HeliosVenues.Konin),
    bilety24Subdomain(KinoOskard, "https://ckis-konin.bilety24.pl"),
    filmweb(2405, KinoZacheta),   // Kleczew
    bilety24("https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626", KinoStudyjneCentrum),   // Konin
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-kole-1621", KinoNadWarta),   // Koło
    bilety24("https://www.bilety24.pl/kino/organizator/zajezdnia-kultury-w-pleszewie-1255", KinoHel),   // Pleszew
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia),   // Słupca
    biletyna(KinoTur, "https://biletyna.pl/Turek/Kino-Tur"),   // Turek
    biletyna(KinoMok, "https://biletyna.pl/Zagorow/Gminny-Osrodek-Kultury"),   // Zagórów
  )
}
