package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Elblag: see [[PolishVenueRegistry]]. */
object ElblagVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0037", MultikinoElblag),
    cinemaCity("1099", CinemaCityElblag),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Tczew),
    filmweb(2352, KinoBaszta),
    msi(KinoPowisle, "https://kinosztumbilety.pl"),
    biletyna(KinoZulawskiOsrodekKultury, "https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury"),
    own(kit => new KinoSwiatowidElblagClient(kit.http, KinoSwiatowidElblag, kit.today)),
  )
}
