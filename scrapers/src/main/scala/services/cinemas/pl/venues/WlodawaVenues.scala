package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Wlodawa: see [[PolishVenueRegistry]]. */
object WlodawaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    filmweb(2312, KinoWDK),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoParczewClient(kit.http, KinoParczew)),   // Parczew
  )
}
