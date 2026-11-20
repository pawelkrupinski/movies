package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Rybnik: see [[PolishVenueRegistry]]. */
object RybnikVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0014", MultikinoRybnik),
    cinemaCity("1082", CinemaCityRybnik),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Zory),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-baltyk-1499", KinoBaltyk),
    ekobilet("kino-centrum-jastrzebiezdrj", KinoCentrum),
    systemBiletowy(KinoNaStarowce, "https://bilety.mok.zory.pl"),
    biletyna(KinoPegaz, "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury"),
    own(kit => new TeatrZiemiRybnickiejClient(kit.http)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoFeniks, "https://biletyna.pl/Rydultowy/Rydultowskie-Centrum-Kultury-sala-teatralna"),   // Rydułtowy
  )
}
