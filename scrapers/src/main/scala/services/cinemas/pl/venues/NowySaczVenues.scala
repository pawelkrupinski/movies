package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Nowy Sacz: see [[PolishVenueRegistry]]. */
object NowySaczVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.NowySacz),
    bilety24("https://www.bilety24.pl/kino/organizator/malopolskie-centrum-kultury-sokol-w-nowym-saczu-1225", KinoSokol),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    ekobilet("kino-jaworzyna", KinoJaworzyna),
    bilety24("https://www.bilety24.pl/kino/organizator/limanowski-dom-kultury-1368", KinoKlaps),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    bilety24("https://www.bilety24.pl/kino/organizator/mckcie-w-grybowie-kino-biala-1321", KinoMCKGrybow),   // Grybów
  )
}
