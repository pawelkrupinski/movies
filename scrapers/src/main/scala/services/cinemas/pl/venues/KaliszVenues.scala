package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Kalisz: see [[PolishVenueRegistry]]. */
object KaliszVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Kalisz),
    multikino("0042", MultikinoKalisz),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.OstrowWielkopolski),
    systemBiletowy(KinoCentrum3D, "https://kck.systembiletowy.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-echo-1159", KinoEcho),
    bilety24("https://www.bilety24.pl/kino/organizator/ostrzeszowskie-centrum-kultury-601", KinoPiastOstrzeszow),
    bilety24("https://www.bilety24.pl/kino/organizator/krotoszynski-osrodek-kultury-1668", KinoPrzedwiosnieKrotoszyn),
  )
}
