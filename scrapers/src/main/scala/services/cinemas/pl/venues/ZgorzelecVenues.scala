package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Zgorzelec: see [[PolishVenueRegistry]]. */
object ZgorzelecVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    multikino("0031", MultikinoZgorzelec),   // Zgorzelec
    bilety24("https://www.bilety24.pl/kino/organizator/bogatynski-osrodek-kultury-1659", KinoKadrBogatynia),   // Bogatynia
  )
}
