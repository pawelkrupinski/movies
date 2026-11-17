package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Gliwice: see [[PolishVenueRegistry]]. */
object GliwiceVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1085", CinemaCityGliwice),
    own(kit => new KinoAmokClient(kit.http, KinoAmok, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    own(kit => new KinoScenaKulturaClient(kit.http, KinoScenaKultura)),
  )
}
