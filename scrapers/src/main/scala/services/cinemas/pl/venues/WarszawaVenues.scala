package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venues wired near Warszawa: see [[PolishVenueRegistry]]. */
object WarszawaVenues extends PolishVenueRegistry {
  val venues: Seq[PolishVenue] = Seq(
    cinemaCity("1074", CinemaCityArkadia),
    cinemaCity("1061", CinemaCityBemowo),
    cinemaCity("1096", CinemaCityGaleriaPolnocna),
    cinemaCity("1069", CinemaCityJanki),
    cinemaCity("1070", CinemaCityMokotow),
    cinemaCity("1068", CinemaCityPromenada),
    cinemaCity("1060", CinemaCitySadyba),
    multikino("0013", MultikinoZloteTarasy),
    multikino("0040", MultikinoMlociny),
    multikino("0052", MultikinoReduta),
    multikino("0024", MultikinoTargowek),
    multikino("0025", MultikinoWolaPark),
    helios(HeliosVenues.BlueCity),
    own(kit => new MuranowClient(kit.http, kit.today)),
    bilety24Subdomain(KinoLuna, "https://kinoluna.bilety24.pl"),
    bilety24("https://www.bilety24.pl/kino/organizator/kino-elektronik-631", KinoElektronik),
    own(kit => new IluzjonClient(kit.http, kit.today)),
    own(kit => new KinoGramClient(kit.http)),
    own(kit => new KinoKulturaClient(kit.http)),
    own(kit => new AmondoClient(kit.http)),
    own(kit => new BokClient(kit.http, "kino-na-boku", KinoNaBoku, kit.today, titles = kit.titles)),
    own(kit => new BokClient(kit.http, "kino-glebocka-66", KinoGlebocka66, kit.today, titles = kit.titles)),
    own(kit => new KinomuzeumClient(kit.http, kit.today)),
    own(kit => new SwitClient(kit.http)),
    own(kit => new PromKepaClient(kit.http)),
    own(kit => new FalenicaClient(kit.http)),
    own(kit => new SdkClient(kit.http)),
    noveKino("atlantic", KinoAtlantic),
    own(kit => new KinotekaClient(kit.http, kit.titles)),
    own(kit => new UjazdowskiClient(kit.http, kit.today)),
    own(kit => new CytadelaClient(kit.http)),
    noveKino("wisla", KinoWisla),
    biletyna(AdaKinoStudyjne, "https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne"),
    own(kit => new AlternatywyClient(kit.http, kit.today, titles = kit.titles)),
    // Filmweb-only until 2026-09-27; each venue's own box office listed more
    // (usually twice the showtimes over the same fortnight) — see the commit.
    biletyna(KinoWKadrze, "https://biletyna.pl/Warszawa/Dom-Kultury-Kadr"),
  )

  override val filmwebCatchment: Seq[PolishVenue] = Seq(
    own(kit => new PrahaClient(kit.http, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha, kit.today)),
  )

  override val nearbyTowns: Seq[PolishVenue] = Seq(
    biletyna(KinoDomSztukiUrsynow, "https://biletyna.pl/Warszawa/Kino-Domu-Sztuki-Ursynow"),   // Warszawa
    biletyna(KinoTerminalKultury, "https://biletyna.pl/Warszawa/Terminal-Kultury"),   // Warszawa
    biletyna(KinoBielanskiOsrodekKultury, "https://biletyna.pl/Warszawa/Bielanski-Osrodek-Kultury"),   // Warszawa
    biletyna(KinoWawerskaStrefaKultury, "https://biletyna.pl/Warszawa/Wawerska-Strefa-Kultury-sala-widowiskowa"),   // Warszawa
    biletyna(KinoBasn, "https://biletyna.pl/Piastow/sala-widowiskowa"),   // Piastów
    biletyna(KinoKasynoOficerskie, "https://biletyna.pl/Nowy-Dwor-Mazowiecki/Kasyno-Oficerskie-Sala-kinowa"),   // Nowy Dwór Mazowiecki
    bilety24("https://www.bilety24.pl/kino/organizator/osrodek-kultury-gminy-grodzisk-mazowiecki-1231", KinoGrodziskieCentrumKultury),   // Grodzisk Mazowiecki
    own(kit => new KinoBajkaBlonieClient(kit.http, KinoBajkaBlonie, kit.today)),   // Błonie
    biletyna(KinoGrojeckiOsrodekKultury, "https://biletyna.pl/Grojec/Grojecki-Osrodek-Kultury"),   // Grójec
    biletyna(KinoUciecha, "https://biletyna.pl/Gora-Kalwaria/Kino-Uciecha"),   // Góra Kalwaria
    helios(HeliosVenues.Wolomin),   // Wołomin
    biletyna(KinoKulturaWolomin, "https://biletyna.pl/Wolomin/Kino-Kultura"),   // Wołomin
    filmweb(2079, KinoTeatrKurtyna),   // Sulejówek
    multikino("0039", MultikinoPruszkow),   // Pruszków
    // Filmweb (3137 / 1532) had gone empty for both by 2026-09-27 while each kept
    // selling its film nights on biletyna. CKiS Pruszków screens in the Centrum
    // Dziedzictwa Kulturowego hall; Grajewo's "Dom Kultury" is the Grajewskie
    // Centrum Kultury, whose page mixes films with concerts and plays.
    biletyna(KinoCKiSPruszkow, "https://biletyna.pl/Pruszkow/Sala-widowiskowa-Centrum-Dziedzictwa-Kulturowego"),   // Pruszków
    helios(HeliosVenues.Legionowo),   // Legionowo
    biletyna(KinoOaza, "https://biletyna.pl/Otwock/Kino-Oaza-Otwockie-Centrum-Kultury"),   // Otwock
  )
}
