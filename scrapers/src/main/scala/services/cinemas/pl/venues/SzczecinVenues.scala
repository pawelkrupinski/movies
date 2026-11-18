package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Szczecin: see [[PolishVenueRegistry]]. */
object SzczecinVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Szczecin),
    multikino("0007", MultikinoSzczecin),
    own(kit => new PionierClient(kit.http, KinoPionier)),
    helios(HeliosVenues.SzczecinOutletPark),
    own(kit => new KinoZamekClient(kit.http, KinoZamekSzczecin, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    systemBiletowy(KinoKawiarnia, "https://kgl.systembiletowy.pl"),
    biletyna(KinoPDK, "https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury"),
    own(kit => new SckStargardClient(kit.http, KinoSCK)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoMOKPolice, "https://biletyna.pl/Police/Miejski-Osrodek-Kultury"),   // Police
    own(kit => new KinoEvaClient(kit.http, KinoEva)),   // Międzyzdroje
    biletyna(KinoGryfinskiDomKultury, "https://biletyna.pl/Gryfino/Sala-widowiskowa-Gryfinskiego-Domu-Kultury"),   // Gryfino
  )
}
