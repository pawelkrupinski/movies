package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Olsztyn: see [[PolishVenueRegistry]]. */
object OlsztynVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // Chain ids verified against each chain's own cinema-list API. Each city's
    // independent screen, where one has a machine-readable source, is added via a
    // shared platform client: FilmwebShowtimesClient (Filmweb's seances JSON, by
    // internal cinema id — verified to return non-empty seances), Bilety24Client
    // (a bilety24.pl venue), or NoveKinoClient.
    helios(HeliosVenues.Olsztyn),
    multikino("0036", MultikinoOlsztyn),
    own(kit => new KinoAwangarda2Client(kit.http, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    msi(KinoCinemaLumiere, "https://bilety.kinoszczytno.pl"),
    msi(KinoIgnacy, "https://www.biletyignacy.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/moraski-dom-kultury-1682", KinoNarie),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoGrunwaldClient(kit.http, KinoGrunwald, kit.today)),   // Olsztynek
  )
}
