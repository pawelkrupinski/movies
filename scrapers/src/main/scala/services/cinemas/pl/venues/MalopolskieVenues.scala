package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Malopolskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object MalopolskieVenues {
  /** The Polish venues wired near Krakow: see [[PolishVenueRegistry]]. */
  object KrakowVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1090", CinemaCityBonarka),
      cinemaCity("1076", CinemaCityKazimierz),
      cinemaCity("1064", CinemaCityZakopianka),
      multikino("0005", MultikinoKrakow),
      // the two Mikro screens share one instance
      systemBiletowy(KinoMikro, "https://bilety.kinomikro.pl", institution = Some(Institution("Kino Mikro"))),
      systemBiletowy(MikroBronowice, "https://bilety.kinomikro.pl", institution = Some(Institution("Mikro Bronowice"))),
      own(kit => new KinoSfinksClient(kit.http, KinoSfinks)),
      own(kit => new KinoPodBaranamiClient(kit.http, KinoPodBaranami, kit.today)),
      msiByDate(KinoKijow, "https://kupbilet.kijow.pl"),
      own(kit => new KinoKikaClient(kit.http, KinoKika)),
      own(kit => new KinoAgrafkaClient(kit.http, KinoAgrafka)),
      own(kit => new KinoParadoxClient(kit.http, KinoParadox)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      systemBiletowy(KinoMuzaMyslenice, "https://bilety.kino.myslenice.pl"),   // Myślenice
      ekobilet("dk-wolbrom", KinoRadosc),   // Wolbrom
      systemBiletowy(KinoGryf, "https://bilety.ckis.miechow.eu"),   // Miechów
      bilety24("https://www.bilety24.pl/kino/organizator/kino-wazka-w-niepolomicach-pole-kultury-samorzadowa-instytucja-kultury-miasta-i-gminy-niepolomice-1273", KinoWazka),   // Niepołomice
      ekobilet("kino-wielicka-mediateka", KinoWielickaMediateka),   // Wieliczka
      bilety24("https://www.bilety24.pl/kino/organizator/kino-lubon-2-0-1549", KinoLubon),   // Mszana Dolna
    )
  }

  /** The Polish venues wired near Nowy Sacz: see [[PolishVenueRegistry]]. */
  object NowySaczVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.NowySacz),
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

  /** The Polish venues wired near Tarnow: see [[PolishVenueRegistry]]. */
  object TarnowVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0050", MultikinoTarnow),
      msi(KinoMillenium, "https://bilety.csm.tarnow.pl", mvcPath = "/Kino/mvc/pl"),   // LiteSpeed reCAPTCHA-gates a browser UA from our Hetzner IP (since 2026-09-30), not a Decodo IP
      own(kit => new KinoMarzenieClient(kit.biletynaFetch, KinoMarzenie, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      systemBiletowy(KinoFarys, "https://kfb.systembiletowy.pl"),
      biletyna(KinoGCK, "https://biletyna.pl/Solec-Zdroj/Kino-Solec-Zdroj"),
      bilety24("https://www.bilety24.pl/kino/organizator/gorlickie-centrum-kultury-1581", KinoKolory),
      msi(KinoPlaneta, "https://rezerwacja.planetabrzesko.pl", mvcPath = "/Rezerwacja/mvc/pl"),
      own(kit => new KinoPromienClient(kit.http, KinoPromien, kit.today)),
      systemBiletowy(KinoRegis, "https://bilety.kino.bochnia.pl"),
      bilety24("https://www.bilety24.pl/kino/organizator/dabrowski-dom-kultury-1303", KinoSokolDabrowaTarnowska),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      filmweb(2408, KinoMCKRadlow),   // Radłów
      bilety24("https://www.bilety24.pl/kino/organizator/gminne-centrum-kultury-i-czytelnictwa-w-lisiej-gorze-1658", KinoCentrumLisiaGora),   // Lisia Góra
      systemBiletowy(KinoJDK, "https://jdk.systembiletowy.pl"),   // Jasło
      multikino("0051", MultikinoMielec),   // Mielec
    )
  }

  /** The Polish venues wired near Zakopane: see [[PolishVenueRegistry]]. */
  object ZakopaneVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      biletyna(KinoSokolZakopane, "https://biletyna.pl/Zakopane/Kino-Sokol"),   // Zakopane
      // Filmweb had fallen out of step with the box office for these six by
      // 2026-09-26 (one or two stray slots against a full biletyna week).
      biletyna(KinoGiewont, "https://biletyna.pl/Zakopane/Kino-Giewont"),   // Zakopane
      biletyna(KinoMiejsce, "https://biletyna.pl/Zakopane/Kino-Miejsce"),   // Zakopane
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-sportu-i-promocji-w-rabce-zdroju-625", KinoSniezkaRabka),   // Rabka-Zdrój
      msi(KinoStopiak, "https://bilety.stopiakcinema.pl"),   // Nowy Targ — own site links bilety.stopiakcinema.pl, a standard MSI portal
    )
  }
}
