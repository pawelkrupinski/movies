package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Zielona Gora: see [[PolishVenueRegistry]]. */
object ZielonaGoraVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1087", CinemaCityZielonaGora),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/nowosolski-dom-kultury-1679", KinoEuropa),
    msi(KinoMaxKino, "https://repertuar.maxkino.eu"),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-pionier-1492", KinoPionierZary),
    msi(KinoSDKSwiebodzin, "https://bilety.kino.swiebodzin.pl:4433"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoZaRogiemSiedlecClient(kit.http, KinoZaRogiemSiedlec, kit.today)),   // Siedlec
  )
}
