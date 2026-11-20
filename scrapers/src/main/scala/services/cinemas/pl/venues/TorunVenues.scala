package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Torun: see [[PolishVenueRegistry]]. */
object TorunVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1077", CinemaCityTorunCzerwonaDroga),
    cinemaCity("1093", CinemaCityTorunPlaza),
    own(kit => new KinoCentrumCswClient(kit.http, KinoCentrumCsw)),
    // Kino 1410's own site (kino1410.pl) links kht.systembiletowy.pl for booking —
    // an existing platform, so it reuses SystemBiletowyClient rather than a new
    // client. Mostly "event cinema" broadcasts (André Rieu, Met Opera) alongside
    // the odd real film/classic-restoration screening; OnlyMovieEventsFilter
    // keeps the broadcasts (see NonMovieEventClassifier's broadcast veto) and
    // drops anything else event-shaped.
    systemBiletowy(Kino1410, "https://kht.systembiletowy.pl"),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    biletyna(KinoMiejskieCentrumKultury, "https://biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury"),
    biletyna(KinoZdroj, "https://biletyna.pl/Ciechocinek/Kino-Zdroj"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Grudziadz),   // Grudziądz
  )
}
