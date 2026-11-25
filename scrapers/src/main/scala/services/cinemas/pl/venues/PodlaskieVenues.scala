package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Podlaskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object PodlaskieVenues {
  /** The Polish venues wired near Bialystok: see [[PolishVenueRegistry]]. */
  object BialystokVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Alfa),
      helios(HeliosVenues.Biala),
      helios(HeliosVenues.Jurowiecka),
      own(kit => new KinoForumClient(kit.http, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      own(kit => new KinoSokolSokolkaClient(kit.http, KinoSokolSokolka)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new DomKulturyLapyClient(kit.http, DomKulturyLapy, kit.today)),   // Łapy
      own(kit => new KinoBielskClient(kit.http, KinoBielsk)),   // Bielsk Podlaski
    )
  }

  /** The Polish venues wired near Lomza: see [[PolishVenueRegistry]]. */
  object LomzaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Lomza),   // Łomża
      bilety24("https://www.bilety24.pl/kino/organizator/ostroleckie-centrum-kultury-1068", KinoJantar),   // Ostrołęka
      own(kit => new ModernEventsCalendarClient(kit.http, ModernEventsCalendarPage("https://kino.mokzambrow.pl/"), KinoCKZambrow, kit.today)),   // Zambrów
      msi(KinoOstrovia, "https://bilety.kinoostrovia.pl"),   // Ostrów Mazowiecka
      own(kit => new KinoWarsClient(kit.http, KinoWars)),   // Wysokie Mazowieckie
      biletyna(KinoDomKulturyGrajewo, "https://biletyna.pl/Grajewo/Grajewskie-Centrum-Kultury"),   // Grajewo
    )
  }

  /** The Polish venues wired near Suwalki: see [[PolishVenueRegistry]]. */
  object SuwalkiVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      // Filmweb-only until 2026-09-27 (Iława's expired leaf is pinned in TlsTrust).
      msi(KinoCinemaLumiereSuwalki, "https://suwalki.cinema-lumiere.pl"),   // Suwałki
      own(kit => new KinoIskraClient(kit.http, KinoIskra, kit.today)),   // Augustów
      bilety24("https://www.bilety24.pl/kino/organizator/dom-kultury-w-goldapi-1089", KinoKulturaGoldap),   // Gołdap
      ekobilet("mazury-olecko", KinoMazur),   // Olecko
      biletyna(KinoECK, "https://biletyna.pl/Elk/Elckie-Centrum-Kultury"),   // Ełk
      msi(KinoPlanetCinemaElk, "https://elk.planetcinema.pl"),   // Ełk
    )
  }
}
