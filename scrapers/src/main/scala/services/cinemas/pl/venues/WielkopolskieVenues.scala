package services.cinemas.pl.venues

import models.*
import services.cinemas.pl.*

/** The Polish venue registries of the Wielkopolskie voivodeship, one object a town: see [[PolishVenueRegistry]] and [[PolishTowns]]. */
object WielkopolskieVenues {
  /** The Polish venues wired near Poznan: see [[PolishVenueRegistry]]. */
  object PoznanVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino(MultikinoClient.PoznanStaryBrowarId, Multikino),
      own(kit => new CharlieMonroeClient(kit.http)),
      own(kit => new KinoPalacoweClient(kit.http, titles = kit.titles)),
      helios(HeliosVenues.Poznan),
      cinemaCity("1078", CinemaCityPoznanPlaza),
      cinemaCity("1081", CinemaCityKinepolis),
      own(kit => new KinoMuzaClient(kit.http, kit.today, titles = kit.titles)),
      own(kit => new KinoBulgarskaClient(kit.http, kit.today)),
      own(kit => new KinoApolloClient(kit.http, titles = kit.titles)),
      own(kit => new RialtoClient(kit.http)),
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoCKPrzezmierowo, "https://biletyna.pl/Przezmierowo/Dom-Kultury"),   // Przeźmierowo
      biletyna(KinoNOKNowyTomysl, "https://biletyna.pl/Nowy-Tomysl/Nowotomyski-Osrodek-Kultury"),   // Nowy Tomyśl
      bilety24("https://www.bilety24.pl/kino/organizator/kino-baszta-477", KinoBasztaSroda),   // Środa Wielkopolska
      bilety24("https://www.bilety24.pl/kino/organizator/kino-halszka-494", KinoHalszka),   // Szamotuły
      bilety24("https://www.bilety24.pl/kino/organizator/sremski-osrodek-kultury-kinoteatr-slonko-478", KinoteatrSlonko),   // Śrem
      bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-kino-miasta-i-gminy-buk-596", KinoWielkopolanin),   // Buk
      bilety24("https://www.bilety24.pl/kino/organizator/przedsiebiorstwo-wielobranzowe-trojka-s-c-475", KinoTrojka),   // Września
      bilety24("https://www.bilety24.pl/kino/organizator/centrum-kultury-i-biblioteka-imienia-powstancow-wielkopolskich-w-opalenicy-967", KinoOpalenica),   // Opalenica
      bilety24("https://www.bilety24.pl/kino/organizator/kinoteatr-gwiazda-962", KinoteatrGwiazda),   // Wronki
    )
  }

  /** The Polish venues wired near Konin: see [[PolishVenueRegistry]]. */
  object KoninVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      // Konin + its catchment: Helios via the chain client, and each independent off
      // its own ticketing listing (bilety24 organiser / biletyna), Filmweb only where
      // it has none. Neighbouring bilety24 organiser ids are different towns: 1626 is
      // Konin's culture centre, 1621 Koło's MDK. Września's Kino Trójka (1698) is
      // intentionally not wired.
      helios(HeliosVenues.Konin),
      bilety24Subdomain(KinoOskard, "https://ckis-konin.bilety24.pl"),
      filmweb(2405, KinoZacheta),   // Kleczew
      bilety24("https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626", KinoStudyjneCentrum),   // Konin
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-kole-1621", KinoNadWarta),   // Koło
      bilety24("https://www.bilety24.pl/kino/organizator/zajezdnia-kultury-w-pleszewie-1255", KinoHel),   // Pleszew
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-w-slupcy-1423", KinoSokolnia),   // Słupca
      biletyna(KinoTur, "https://biletyna.pl/Turek/Kino-Tur"),   // Turek
      biletyna(KinoMok, "https://biletyna.pl/Zagorow/Gminny-Osrodek-Kultury"),   // Zagórów
    )
  }

  /** The Polish venues wired near Gniezno: see [[PolishVenueRegistry]]. */
  object GnieznoVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Gniezno),   // Gniezno
      bilety24("https://www.bilety24.pl/kino/organizator/mogilenski-dom-kultury-kino-wawrzyn-1644", KinoWawrzyn),   // Mogilno
      bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-publiczna-im-czeslawa-chruszczewskiego-1294", KinoMiescisko),   // Mieścisko
    )
  }

  /** The Polish venues wired near Kalisz: see [[PolishVenueRegistry]]. */
  object KaliszVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Kalisz),
      multikino("0042", MultikinoKalisz),
    )

    override val filmwebCatchment: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.OstrowWielkopolski),
      systemBiletowy(KinoCentrum3D, "https://kck.systembiletowy.pl"),
      bilety24("https://www.bilety24.pl/kino/organizator/kino-echo-1159", KinoEcho),
      bilety24("https://www.bilety24.pl/kino/organizator/ostrzeszowskie-centrum-kultury-601", KinoPiastOstrzeszow),
      bilety24("https://www.bilety24.pl/kino/organizator/krotoszynski-osrodek-kultury-1668", KinoPrzedwiosnieKrotoszyn),
    )
  }

  /** The Polish venues wired near Leszno: see [[PolishVenueRegistry]]. */
  object LesznoVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      multikino("0044", MultikinoLeszno),   // Leszno
      bilety24("https://www.bilety24.pl/kino/organizator/teatr-miejski-w-lesznie-1653", KinoTeatrMiejskiLeszno),   // Leszno
      // The site's own repertoire is prose; its "Kup online" widget is this page.
      biletyna(KinoPromienRawicz, "https://biletyna.pl/Rawicz/Kino-Promien-w-Rawiczu"),   // Rawicz
      bilety24("https://www.bilety24.pl/kino/organizator/gostynski-osrodek-kultury-hutnik-z-siedziba-w-gostyniu-679", KinoPodKopula),   // Gostyń
      bilety24("https://www.bilety24.pl/kino/organizator/biblioteka-i-centrum-kultury-rekreacji-im-jana-z-domachowa-bzdegi-w-krobi-1643", KinoKrobia),   // Krobia
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      ekobilet("dom-kultury-w-gorze-7114", KinoDKGora),   // Góra
      ekobilet("zpkwasosz", KinoZaciszeWasosz),   // Wąsosz
    )
  }

  /** The Polish venues wired near Pila: see [[PolishVenueRegistry]]. */
  object PilaVenues extends PolishVenueRegistry {
    val venues: Seq[PolishVenue] = Seq(
      helios(HeliosVenues.Pila),   // Piła
      bilety24("https://www.bilety24.pl/kino/organizator/waleckie-centrum-kultury-1598", KinoWCKWalcz),   // Wałcz
      bilety24("https://www.bilety24.pl/kino/organizator/kino-osiedlowe-trzcianka-1559", KinoOsiedlowe),   // Trzcianka
      biletyna(KinoRodlo, "https://biletyna.pl/Zlotow/Zlotowskie-Centrum-Aktywnosci-Spolecznej"),   // Złotów
      bilety24("https://www.bilety24.pl/kino/organizator/gminny-osrodek-kultury-685", KinoMewaBudzyn),   // Budzyń
      bilety24("https://www.bilety24.pl/kino/organizator/chodzieski-dom-kultury-591", KinoNotec),   // Chodzież
      bilety24("https://www.bilety24.pl/kino/organizator/nck-w-wieleniu-1344", KinoZaRogiemWielen),   // Wieleń
      bilety24("https://www.bilety24.pl/kino/organizator/miejski-dom-kultury-623", KinoMDKWagrowiec),   // Wągrowiec
      bilety24("https://www.bilety24.pl/kino/organizator/mck-kino-swiatowid-831", KinoSwiatowidCzarnkow),   // Czarnków
    )

    override val nearbyTowns: Seq[PolishVenue] = Seq(
      biletyna(KinoOKJastrowie, "https://biletyna.pl/Jastrowie/Osrodek-Kultury"),   // Jastrowie
    )
  }
}
