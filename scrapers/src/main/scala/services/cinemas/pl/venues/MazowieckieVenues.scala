package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Mazowieckie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object MazowieckieVenues {
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

  /** The Polish venues wired near Siedlce: see [[PolishVenueRegistry]]. */
  object SiedlceVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Siedlce),   // Siedlce
      noveKino("siedlce", NoveKinoSiedlce),   // Siedlce
      filmweb(2073, KinoSokolSokolowPodlaski),   // Sokołów Podlaski
      // One screen Filmweb split across two ids (1658 "Kino Sława", 1850 "Kino za
      // Rogiem", the room's older name), sharing slots; biletyna sells all of it.
      biletyna(KinoSlawa, "https://biletyna.pl/Miedzyrzec-Podlaski/Kino-Slawa"),   // Międzyrzec Podlaski
      biletyna(KinoKongres, "https://biletyna.pl/Wegrow/Wegrowski-Osrodek-Kultury"),   // Węgrów
      own(kit => new KinoLukowClient(kit.http, KinoLukow)),   // Łuków
      cinemaCity("1100", CinemaCityBialaPodlaska),   // Biała Podlaska
      systemBiletowy(KinoWilga, "https://grl.systembiletowy.pl"),   // Garwolin
    )
  }

  /** The Polish venues wired near Wyszkow: see [[PolishVenueRegistry]]. */
  object WyszkowVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      biletyna(KinoDobrychFilmow, "https://biletyna.pl/Wyszkow/Wyszkowski-Osrodek-Kultury-Hutnik"),   // Wyszków
      biletyna(KinoMiGOK, "https://biletyna.pl/Lochow/Miejski-i-Gminny-Osrodek-Kultury"),   // Łochów
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      own(kit => new KinoNarewClient(kit.http, KinoNarew)),   // Pułtusk
    )
  }

  /** The Polish venues wired near Radom: see [[PolishVenueRegistry]]. */
  object RadomVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Radom),
      multikino("0026", MultikinoRadom),
      own(kit => new McswElektrowniaCinemaClient(kit.http, McswElektrowniaCinema, kit.today)),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Starachowice),
      msi(KinoCentrumSkarzyskoKamienna, "https://bilet-mck.skarzysko.pl"),
      bilety24("https://www.bilety24.pl/kino/organizator/szydlowieckie-centrum-kultury-zamek-1320", KinoGornik),
      msi(KinoKozienickiDomKultury, "http://bilety.dkkozienice.pl"),
      systemBiletowy(KinoKuznica, "https://shd.systembiletowy.pl"),
      msi(KinoSwitZwolen, "https://bilety.switzwolen.pl"),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      ekobilet("centrum-kultury-i-turystyki-w-ilzy-8211", KinoCKiTIlza),   // Iłża
      biletyna(KinoKsiazka, "https://biletyna.pl/Stara-Blotnica/Kino-KSIAZKA-w-GPB-w-St-Blotnicy"),   // Stara Błotnica
      own(kit => new IksorisBookingClient(kit.http, IksorisBookingPage(IksorisOrigin("https://bilety.rck.drzewica.pl")), KinoRCKDrzewica)),   // Drzewica
    )
  }

  /** The Polish venues wired near Plock: see [[PolishVenueRegistry]]. */
  object PlockVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Plock),
      noveKino("przedwiosnie", KinoPrzedwiosnie),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      msi(KinoZaRogiem, "https://bilety.pokis.pl"),
      bilety24("https://www.bilety24.pl/kino/organizator/kutnowski-dom-kultury-1474", KinoKDK),
      systemBiletowy(KinoKalejdoskop, "https://ckp.systembiletowy.pl"),
      msi(KinoODEON, "https://kinoodeon.eurobilet.pl"),
    )
  }

  /** The Polish venues wired near Ciechanow: see [[PolishVenueRegistry]]. */
  object CiechanowVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      bilety24("https://www.bilety24.pl/kino/organizator/powiatowe-centrum-kultury-i-sztuki-im-marii-konopnickiej-1307", KinoLydynia),   // Ciechanów
      biletyna(KinoKosmosMlawa, "https://biletyna.pl/Mlawa/Miejski-Dom-Kultury-kino-Kosmos"),   // Mława
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-im-stanislawa-ostoi-kotkowskiego-w-przasnyszu-1482", KinoSwiatowidPrzasnysz),   // Przasnysz
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-makowie-mazowieckim-1485", KinoMDKMakow),   // Maków Mazowiecki
      biletyna(KinoApolloDzialdowo, "https://biletyna.pl/Dzialdowo/Miejski-Dom-Kultury"),   // Działdowo
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoNOKNasielsk, "https://biletyna.pl/Nasielsk/Nasielski-Osrodek-Kultury"),   // Nasielsk
      ekobilet("kinoton", KinoTon),   // Żuromin
    )
  }
}
