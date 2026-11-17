package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Bytom: see [[PolishVenueRegistry]]. */
object BytomVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // BCKino sells theatre/workshops/concerts through the same listing as its
    // films, tagged with a `data-group` per event ("BCKino" for films) —
    // `filmGroups` keeps only those and peels the resulting "BCKino – " title
    // prefix. Verified screening 2026-09-23.
    cinemaCity("1092", CinemaCityBytom),
    systemBiletowy(KinoBCKBytom, "https://bck.systembiletowy.pl", filmGroup = Some(EventCategory("BCKino"))),
  )
}
