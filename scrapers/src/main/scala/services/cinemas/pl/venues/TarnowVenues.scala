package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Tarnow: see [[PolishVenueRegistry]]. */
object TarnowVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0050", MultikinoTarnow),
    msi(KinoMillenium, "https://bilety.csm.tarnow.pl", mvcPath = "/Kino/mvc/pl"),   // LiteSpeed reCAPTCHA-gates a browser UA from our Hetzner IP (since 2026-09-30), not a Decodo IP
    own(kit => new KinoMarzenieClient(kit.biletynaFetch, KinoMarzenie, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    systemBiletowy(KinoFarys, "https://kfb.systembiletowy.pl"),
    biletyna(KinoGCK, "https://biletyna.pl/Solec-Zdroj/Kino-Solec-Zdroj"),
    bilety24("https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory),
    msi(KinoPlaneta, "https://rezerwacja.planetabrzesko.pl", mvcPath = "/Rezerwacja/mvc/pl"),
    own(kit => new KinoPromienClient(kit.http, KinoPromien, kit.today)),
    systemBiletowy(KinoRegis, "https://bilety.kino.bochnia.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/dabrowski-dom-kultury-1303", KinoSokolDabrowaTarnowska),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    filmweb(2408, KinoMCKRadlow),   // Radłów
    bilety24("https://www.bilety24.pl/kino/organizator/gminne-centrum-kultury-i-czytelnictwa-w-lisiej-gorze-1658", KinoCentrumLisiaGora),   // Lisia Góra
    systemBiletowy(KinoJDK, "https://jdk.systembiletowy.pl"),   // Jasło
    multikino("0051", MultikinoMielec),   // Mielec
  )
}
