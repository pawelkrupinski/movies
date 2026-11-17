package services.cinemas.pl.venues

import models._
import services.cinemas.pl._

/** The Polish venues wired near Rzeszow: see [[PolishVenueRegistry]]. */
object RzeszowVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Rzeszow),
    multikino("0028", MultikinoRzeszow),
    own(kit => new KinoZorzaClient(kit.http, KinoZorza, kit.today)),
    own(kit => new KinoZaRogiemCafeClient(kit.http, KinoZaRogiemCafe, kit.today)),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    helios(HeliosNuxt.Krosno),
    own(kit => new ArtKinoKrosnoClient(kit.http, KinoArtKino, kit.today)),
    own(kit => new KinoJednoscClient(kit.http, KinoJednosc)),
    msi(KinoMCK, "https://bilety.kinolezajsk.pl"),
    // Dębica; left MSI 2026-09
    bilety24Subdomain(KinoSniezka, "https://mokdebica.bilety24.pl"),
    own(kit => new KinoSokolBrzozowClient(kit.http, KinoSokolBrzozow)),
    msi(KinoWarszawa, "https://bilety-kino.przeworsk.um.gov.pl"),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    own(kit => new KinoSokolStrzyzowClient(kit.http, KinoSokolStrzyzow, kit.today)),   // Strzyżów
  )
}
