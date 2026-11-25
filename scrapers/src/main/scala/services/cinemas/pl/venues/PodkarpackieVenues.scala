package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Podkarpackie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object PodkarpackieVenues {
  /** The Polish venues wired near Przemysl: see [[PolishVenueRegistry]]. */
  object PrzemyslVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Przemysl),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      own(kit => new KinoCentrum3DPrzemyslClient(kit.http, KinoCentrum3DPrzemysl)),
      msi(KinoIkar, "https://kinoikar.mok-jar.pl"),
      msi(KinoNaBiegunach, "https://jaroslaw.kinonabiegunach.pl"),
      own(kit => new KinoSDKSanokClient(kit.http, KinoSDK, kit.today)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new SystemBiletowyClient(kit.http, VisualSoftPortal("https://udk.systembiletowy.pl"), KinoOrzelUstrzyki, titles = kit.titles)),   // Ustrzyki Dolne
      systemBiletowy(KinoBieszczadzkiDK, "https://bdk.systembiletowy.pl"),   // Lesko
    )
  }

  /** The Polish venues wired near Stalowa Wola: see [[PolishVenueRegistry]]. */
  object StalowaWolaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.StalowaWola),   // Stalowa Wola
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-stalowa-wola-1419", KinoWrzos),   // Stalowa Wola
      own(kit => new ModernEventsCalendarClient(kit.http, ModernEventsCalendarPage("https://nck.nisko.pl/kino-sokol/repertuar/"), KinoSokolNisko, kit.today)),   // Nisko
      own(kit => new KinoMetalowiecNowaDebaClient(kit.http, KinoMetalowiecNowaDeba, kit.today)),   // Nowa Dęba
      own(kit => new IksorisBookingClient(kit.http, IksorisBookingPage(IksorisOrigin("https://bilet.bck.lbl.pl")), KinoBCK)),   // Biłgoraj
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new KinoJOKClient(kit.http, KinoJOK, kit.today)),   // Janów Lubelski
      systemBiletowy(KinoCKiBNowaSarzyna, "https://oks.systembiletowy.pl"),   // Nowa Sarzyna
    )
  }

  /** The Polish venues wired near Rzeszow: see [[PolishVenueRegistry]]. */
  object RzeszowVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Rzeszow),
      multikino("0028", MultikinoRzeszow),
      own(kit => new KinoZorzaClient(kit.http, KinoZorza, kit.today)),
      own(kit => new KinoZaRogiemCafeClient(kit.http, KinoZaRogiemCafe, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Krosno),
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
}
