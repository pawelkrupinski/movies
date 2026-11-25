package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Dolnoslaskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object DolnoslaskieVenues {
  /** The Polish venues wired near Jelenia Gora: see [[PolishVenueRegistry]]. */
  object JeleniaGoraVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.JeleniaGora),
      bilety24Subdomain(KinoLot, "https://kino-lot.bilety24.pl"),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/kino-wawel-1489", KinoWawel),
    )
  }

  /** The Polish venues wired near Walbrzych: see [[PolishVenueRegistry]]. */
  object WalbrzychVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1091", CinemaCityWalbrzych),
      bilety24Subdomain(KinoApolloWalbrzych, "https://kino-apollo.bilety24.pl"),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      msi(KinoMOKNowaRuda, "https://bilety.nowaruda.pl"),
      ekobilet("mokis-bielawa", KinoMOKiS),
      own(kit => new KinoSlezaClient(kit.http, KinoSleza)),
      own(kit => new KinoZbyszekClient(kit.http, KinoZbyszek)),
      multikino("0041", MultikinoKlodzko),
      multikino("0043", MultikinoSwidnica),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoSCKStrzegom, "https://biletyna.pl/Strzegom/Kino-SCK"),   // Strzegom
    )
  }

  /** The Polish venues wired near Legnica: see [[PolishVenueRegistry]]. */
  object LegnicaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Legnica),
      bilety24Subdomain(KinoPiast, "https://kino-piast.bilety24.pl"),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Lubin),
      own(kit => new KinoAurumClient(kit.http, KinoAurum)),
      own(kit => new CyfroweKinoClient(kit.http, KinoCyfroweKino, titles = kit.titles)),
      bilety24("https://www.bilety24.pl/kino/organizator/boleslawiecki-osrodek-kultury-miedzynarodowe-centrum-ceramiki-w-boleslawcu-1586", KinoForumBoleslawiec),
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-muza-w-lubinie-dolny-slask-1375", KinoMuzaLubin),
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689", KinoPCA),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoMOKGlogow, "https://biletyna.pl/Glogow/Miejski-Osrodek-Kultury"),   // Głogów
      biletyna(TeatrGryphius, "https://biletyna.pl/Glogow/Teatr-im-Andreasa-Gryphiusa"),   // Głogów
      multikino("0048", MultikinoGlogow),   // Głogów
      own(kit => new KinoKulturaChojnowClient(kit.http, KinoKulturaChojnow)),   // Chojnów
    )
  }

  /** The Polish venues wired near Wroclaw: see [[PolishVenueRegistry]]. */
  object WroclawVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      cinemaCity("1097", CinemaCityWroclavia),
      cinemaCity("1067", CinemaCityKorona),
      multikino("0010", MultikinoPasazGrunwaldzki),
      helios(HeliosVenues.Magnolia),
      helios(HeliosVenues.AlejaBielany),
      own(kit => new NoweHoryzontyClient(kit.http, kit.today)),
      own(kit => new DcfClient(kit.http)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      own(kit => new Bilety24SubdomainClient(kit.http, "https://kulturalne-oborniki.bilety24.pl/repertuar/", KinoAstra, today = kit.today, titles = kit.titles)),
      filmweb(1645, KinoDyskusyjnyKlubFilmowyPolitechnika),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoNowyPafawag, "https://biletyna.pl/Wroclaw/Centrum-Kultury-Wroclaw-Zachod"),   // Wrocław
      bilety24("https://www.bilety24.pl/kino/organizator/kino-odra-1601", KinoOdraBrzegDolny),   // Brzeg Dolny
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-sztuki-w-olawie-1498", KinoOdraOlawa),   // Oława
      msi(GoKinoOlawa, "https://olawa.gokino.pl"),   // Oława
      msi(KinoGrazyna, "https://bilety.kinostrzelin.pl"),   // Strzelin
      bilety24("https://www.bilety24.pl/kino/organizator/kino-polonia-1090", KinoPoloniaTrzebnica),   // Trzebnica
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-osrodek-kultury-i-sztuki-w-olesnicy-1193", KinoMOKiSOlesnica),   // Oleśnica
    )
  }

  /** The Polish venues wired near Zgorzelec: see [[PolishVenueRegistry]]. */
  object ZgorzelecVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0031", MultikinoZgorzelec),   // Zgorzelec
      bilety24("https://www.bilety24.pl/kino/organizator/bogatynski-osrodek-kultury-1659", KinoKadrBogatynia),   // Bogatynia
    )
  }
}
