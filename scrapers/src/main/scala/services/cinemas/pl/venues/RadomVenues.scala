package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Radom: see [[PolishVenueRegistry]]. */
object RadomVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Radom),
    multikino("0026", MultikinoRadom),
    own(kit => new McswElektrowniaCinemaClient(kit.http, McswElektrowniaCinema, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Starachowice),
    msi(KinoCentrumSkarzyskoKamienna, "https://bilet-mck.skarzysko.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/szydlowieckie-centrum-kultury-zamek-1320", KinoGornik),
    msi(KinoKozienickiDomKultury, "http://bilety.dkkozienice.pl"),
    systemBiletowy(KinoKuznica, "https://shd.systembiletowy.pl"),
    msi(KinoSwitZwolen, "https://bilety.switzwolen.pl"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    ekobilet("centrum-kultury-i-turystyki-w-ilzy-8211", KinoCKiTIlza),   // Iłża
    biletyna(KinoKsiazka, "https://biletyna.pl/Stara-Blotnica/Kino-KSIAZKA-w-GPB-w-St-Blotnicy"),   // Stara Błotnica
    own(kit => new IksorisBookingClient(kit.http, IksorisBookingPage(IksorisOrigin("https://bilety.rck.drzewica.pl")), KinoRCKDrzewica)),   // Drzewica
  )
}
