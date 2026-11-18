package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Dabrowa Gornicza: see [[PolishVenueRegistry]]. */
object DabrowaGorniczaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosVenues.DabrowaGornicza),
    own(kit => new VisualTicketClient(kit.http, "https://bilety.palac.art.pl", KinoKadr, locationId = 2)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoZbyszekOlkusz, "https://biletyna.pl/Olkusz/Kino-Zbyszek"),   // Olkusz
    multikino("0038", MultikinoJaworzno),   // Jaworzno
    biletyna(KinoSztuka, "https://biletyna.pl/Chrzanow/Miejski-Osrodek-Kultury-Sportu-i-Rekreacji"),   // Chrzanów
  )
}
