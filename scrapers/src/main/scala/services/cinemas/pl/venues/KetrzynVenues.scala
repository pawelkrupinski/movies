package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Ketrzyn: see [[PolishVenueRegistry]]. */
object KetrzynVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    own(kit => new KinoGwiazdaKetrzynClient(kit.http, KinoGwiazdaKetrzyn)),   // Kętrzyn
    ekobilet("mck-mragowo", KinoZodiak),   // Mrągowo
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoNowaFalaClient(kit.http, KinoNowaFalaGizycko)),   // Giżycko
  )
}
