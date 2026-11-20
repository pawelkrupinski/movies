package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Wyszkow: see [[PolishVenueRegistry]]. */
object WyszkowVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    biletyna(KinoDobrychFilmow, "https://biletyna.pl/Wyszkow/Wyszkowski-Osrodek-Kultury-Hutnik"),   // Wyszków
    biletyna(KinoMiGOK, "https://biletyna.pl/Lochow/Miejski-i-Gminny-Osrodek-Kultury"),   // Łochów
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoNarewClient(kit.http, KinoNarew)),   // Pułtusk
  )
}
