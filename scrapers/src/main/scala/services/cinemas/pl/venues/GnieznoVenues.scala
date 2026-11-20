package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Gniezno: see [[PolishVenueRegistry]]. */
object GnieznoVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.Gniezno),   // Gniezno
    bilety24("https://www.bilety24.pl/kino/organizator/mogilenski-dom-kultury-kino-wawrzyn-1644", KinoWawrzyn),   // Mogilno
    bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-publiczna-im-czeslawa-chruszczewskiego-1294", KinoMiescisko),   // Mieścisko
  )
}
