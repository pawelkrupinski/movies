package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Leszno: see [[PolishVenueRegistry]]. */
object LesznoVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0044", MultikinoLeszno),   // Leszno
    bilety24("https://www.bilety24.pl/kino/organizator/teatr-miejski-w-lesznie-1653", KinoTeatrMiejskiLeszno),   // Leszno
    // The site's own repertoire is prose; its "Kup online" widget is this page.
    biletyna(KinoPromienRawicz, "https://biletyna.pl/Rawicz/Kino-Promien-w-Rawiczu"),   // Rawicz
    bilety24("https://www.bilety24.pl/kino/organizator/gostynski-osrodek-kultury-hutnik-z-siedziba-w-gostyniu-679", KinoPodKopula),   // Gostyń
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-centrum-kultury-rekreacji-im-jana-z-domachowa-bzdegi-w-krobi-1643", KinoKrobia),   // Krobia
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    ekobilet("dom-kultury-w-gorze-7114", KinoDKGora),   // Góra
    ekobilet("zpkwasosz", KinoZaciszeWasosz),   // Wąsosz
  )
}
