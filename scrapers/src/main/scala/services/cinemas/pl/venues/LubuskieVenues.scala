package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Lubuskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object LubuskieVenues {
  /** The Polish venues wired near Gorzow Wielkopolski: see [[PolishVenueRegistry]]. */
  object GorzowWielkopolskiVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Gorzow),
      multikino("0047", MultikinoGorzow),
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-sztuki-kino-60-krzesel-dkf-megaron-776", Kino60Krzesel),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoMewaMiedzychod, "https://biletyna.pl/Miedzychod/Kino-Mewa"),   // Międzychód
      bilety24("https://www.bilety24.pl/kino/organizator/barlinecki-osrodek-kultury-1052", KinoPanorama),   // Barlinek
      msi(KinoMOKMiedzyrzecz, "https://bilety.mokmiedzyrzecz.pl"),   // Międzyrzecz
    )
  }

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

  /** The Polish venues wired near Slubice: see [[PolishVenueRegistry]]. */
  object SlubiceVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      // Two towns with one cinema each and nothing else within an hour's drive —
      // pages of their own rather than a stretch onto the nearest region.
      bilety24("https://www.bilety24.pl/kino/organizator/slubicki-miejski-osrodek-kultury-1336", KinoSmok),
    )
  }
}
