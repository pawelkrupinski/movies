package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Gorzow Wielkopolski: see [[PolishVenueRegistry]]. */
object GorzowWielkopolskiVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Gorzow),
    multikino("0047", MultikinoGorzow),
    bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-sztuki-kino-60-krzesel-dkf-megaron-776", Kino60Krzesel),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoMewaMiedzychod, "https://biletyna.pl/Miedzychod/Kino-Mewa"),   // Międzychód
    bilety24("https://www.bilety24.pl/kino/organizator/barlinecki-osrodek-kultury-1052", KinoPanorama),   // Barlinek
    msi(KinoMOKMiedzyrzecz, "https://bilety.mokmiedzyrzecz.pl"),   // Międzyrzecz
  )
}
