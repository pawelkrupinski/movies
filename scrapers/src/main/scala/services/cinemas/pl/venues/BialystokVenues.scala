package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Bialystok: see [[PolishVenueRegistry]]. */
object BialystokVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Alfa),
    helios(HeliosVenues.Biala),
    helios(HeliosVenues.Jurowiecka),
    own(kit => new KinoForumClient(kit.http, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    own(kit => new KinoSokolSokolkaClient(kit.http, KinoSokolSokolka)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new DomKulturyLapyClient(kit.http, DomKulturyLapy, kit.today)),   // Łapy
    own(kit => new KinoBielskClient(kit.http, KinoBielsk)),   // Bielsk Podlaski
  )
}
