package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Gniezno: see [[PolishVenueRegistry]]. */
object GnieznoVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Gniezno),   // Gniezno
    bilety24("https://www.bilety24.pl/kino/organizator/mogilenski-dom-kultury-kino-wawrzyn-1644", KinoWawrzyn),   // Mogilno
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-publiczna-im-czeslawa-chruszczewskiego-1294", KinoMiescisko),   // Mieścisko
  )
}
