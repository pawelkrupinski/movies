package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Lodzkie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object LodzkieVenues {
  /** The Polish venues wired near Lodz: see [[PolishVenueRegistry]]. */
  object LodzVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1080", CinemaCityManufaktura),
      multikino("0023", MultikinoLodz),
      helios(HeliosVenues.Lodz),
      // Charlie's homepage (the old CharlieClient) renders today only; its portal lists
      // every booked day. The portal also sells Kino Spójnia's screenings, which
      // KinoSpojniaClient scrapes from Spójnia's own site.
      msi(KinoCharlie, "https://bilety.charlie.pl", otherVenueLocations = Set("Kino Spójna")),
      own(kit => new KinematografLodzClient(kit.http, KinematografLodz, kit.today, titles = kit.titles)),
      own(kit => new NckfClient(kit.http, Nckf, kit.today)),
      own(kit => new KinoTatryClient(kit.http, KinoTatry, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      own(kit => new KinoSpojniaClient(kit.http, KinoSpojnia)),
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-stary-mlyn-w-zgierzu-1697", KinoStaryMlyn),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoMOKGlowno, "https://biletyna.pl/Glowno/Miejski-Osrodek-Kultury"),   // Głowno
      own(kit => new KinoTomiClient(kit.http, KinoTomi)),   // Pabianice
      helios(HeliosVenues.Pabianice),   // Pabianice
      msi(KinoGornikLeczyca, "https://kinogornik.eurobilet.pl"),   // Łęczyca
    )
  }

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

  /** The Polish venues wired near Skierniewice: see [[PolishVenueRegistry]]. */
  object SkierniewiceVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      // Filmweb (320 / 3149) went empty for both on ~2026-09-22; biletyna sells
      // every ticket for the cinema and for the CKiS concert hall's film nights.
      biletyna(KinoPolonez, "https://biletyna.pl/Skierniewice/Kinoteatr-Polonez"),   // Skierniewice
      biletyna(KinoCKiSSkierniewice, "https://biletyna.pl/Skierniewice/Centrum-Kultury-i-Sztuki-Sala-koncertowa"),   // Skierniewice
      biletyna(KinoLen, "https://biletyna.pl/Zyrardow/Kino-Len"),   // Żyrardów
      own(kit => new KinoRomaRawaClient(kit.http, KinoRomaRawa, kit.today)),   // Rawa Mazowiecka
      systemBiletowy(KinoFenix, "https://lok.systembiletowy.pl"),   // Łowicz
    )
  }

  /** The Polish venues wired near Wielun: see [[PolishVenueRegistry]]. */
  object WielunVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      filmweb(2034, KinoSyrena),   // Wieluń
      msi(KinoTeatrSieradz, "https://bilety.scksieradz.pl", filmLabel = Some("FILM")),   // Sieradz
      msi(KinoSlonce, "https://kinoslonce.pl"),   // Wieruszów
      own(kit => new IksorisRepertoireClient(kit.http, IksorisOrigin("https://kinosokolnia.org"), KinoSokolniaKepno, kit.today)),   // Kępno
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      msi(KinoWRatuszu, "https://bilety.mdk-zdunskawola.pl"),   // Zduńska Wola
    )
  }
}
