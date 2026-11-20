package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Piotrkow Trybunalski: see [[PolishVenueRegistry]]. */
object PiotrkowTrybunalskiVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    // Smaller towns grouped around their largest one, every venue about an hour's
    // drive from it — the shape Konin already had for Turek, Koło and Słupca.
    helios(HeliosVenues.PiotrkowTrybunalski),   // Piotrków Trybunalski
    helios(HeliosVenues.Belchatow),   // Bełchatów
    own(kit => new IksorisBookingClient(kit.http, IksorisBookingPage(IksorisOrigin("https://bilety.mckbelchatow.pl")), KinoKulturaBelchatow)),   // Bełchatów
    helios(HeliosVenues.TomaszowMazowiecki),   // Tomaszów Mazowiecki
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoMCKTkacz, "https://biletyna.pl/Tomaszow-Mazowiecki/Miejskie-Centrum-Kultury-Filia-Tkacz"),   // Tomaszów Mazowiecki
    biletyna(KinoMDKOpoczno, "https://biletyna.pl/Opoczno/Miejski-Dom-Kultury-im-Tadeusza-Sygietynskiego"),   // Opoczno
  )
}
