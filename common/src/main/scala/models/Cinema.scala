package models

sealed abstract class Cinema(val displayName: String, val pillName: String) extends Source {
  /** Whether the venue screens all year or only in a season ([[Opening]]): generated from
   *  its name for the data-driven rosters (`data/scripts/seasonal_venues.py`), overridden
   *  by hand for a seasonal Polish or UK venue below.
   *
   *  A member, NOT a defaulted constructor parameter: a default lives on the `Cinema`
   *  companion, so every case object's initialiser would start `Cinema`'s — the roster
   *  object `GermanRoster` reads while it builds — and two threads entering from either
   *  end deadlock on class initialisation (seen in a parallel test run). */
  def opening: Opening = Opening.YearRound
  /** Stable kebab-case id for per-cinema title rules (`TitleNormalizer.cinemaClean`).
   *  Derived from the case-object name so a shared portal client (Bilety24,
   *  SystemBiletowy, Ekobilet, …) can clean each venue it serves without a
   *  hand-maintained map: `KinoOskard` → `"kino-oskard"`, `KinoNaStarowce` →
   *  `"kino-na-starowce"`. */
  lazy val slug: String =
    toString.replaceAll("([a-z0-9])([A-Z])", "$1-$2").toLowerCase(java.util.Locale.ROOT)
}

// ── Poznań ───────────────────────────────────────────────────────────────────

case object KinoApollo extends Cinema("Kino Apollo", "Apollo")

case object KinoBulgarska extends Cinema("Kino Bułgarska 19", "Bułgarska 19")

case object CharlieMonroe extends Cinema("Kino Malta Charlie Monroe", "Malta Charlie Monroe")

case object Helios extends Cinema("Helios Posnania", "Helios")

case object CinemaCityKinepolis extends Cinema("Cinema City Kinepolis", "Kinepolis")

case object KinoMuza extends Cinema("Kino Muza", "Muza")

case object Multikino extends Cinema("Multikino Stary Browar", "Multikino")

case object KinoPalacowe extends Cinema("Kino Pałacowe", "Pałacowe")

case object CinemaCityPoznanPlaza extends Cinema("Cinema City Poznań Plaza", "Poznań Plaza")

case object Rialto extends Cinema("Kino Rialto", "Rialto")

// ── Wrocław ──────────────────────────────────────────────────────────────────

case object CinemaCityWroclavia extends Cinema("Cinema City Wroclavia", "Wroclavia")

case object CinemaCityKorona extends Cinema("Cinema City Korona", "Korona")

case object MultikinoPasazGrunwaldzki extends Cinema("Multikino Pasaż Grunwaldzki", "Pasaż Grunwaldzki")

case object HeliosMagnolia extends Cinema("Helios Magnolia Park", "Magnolia")

case object HeliosAlejaBielany extends Cinema("Helios Aleja Bielany", "Aleja Bielany")

case object KinoNoweHoryzonty extends Cinema("Kino Nowe Horyzonty", "Nowe Horyzonty")

case object DolnoslaskieCentrumFilmowe extends Cinema("Dolnośląskie Centrum Filmowe", "DCF")

// ── Warszawa ─────────────────────────────────────────────────────────────────

case object CinemaCityArkadia extends Cinema("Cinema City Arkadia", "Arkadia")

case object CinemaCityBemowo extends Cinema("Cinema City Bemowo", "Bemowo")

case object CinemaCityGaleriaPolnocna extends Cinema("Cinema City Galeria Północna", "Galeria Północna")

case object CinemaCityJanki extends Cinema("Cinema City Janki", "Janki")   // Janki — Cinema City API

case object CinemaCityMokotow extends Cinema("Cinema City Mokotów", "Mokotów")

case object CinemaCityPromenada extends Cinema("Cinema City Promenada", "Promenada")

case object CinemaCitySadyba extends Cinema("Cinema City Sadyba", "Sadyba")

case object MultikinoZloteTarasy extends Cinema("Multikino Złote Tarasy", "Złote Tarasy")

case object MultikinoMlociny extends Cinema("Multikino Młociny", "Młociny")

case object MultikinoReduta extends Cinema("Multikino Reduta", "Reduta")

case object MultikinoTargowek extends Cinema("Multikino Targówek", "Targówek")

case object MultikinoWolaPark extends Cinema("Multikino Wola Park", "Wola Park")

case object HeliosBlueCity extends Cinema("Helios Blue City", "Blue City")

case object KinoMuranow extends Cinema("Kino Muranów", "Muranów")

case object KinoLuna extends Cinema("Kino Luna", "Luna")

case object KinoElektronik extends Cinema("Kino Elektronik", "Elektronik")

case object KinoIluzjon extends Cinema("Kino Iluzjon", "Iluzjon")

case object KinoGram extends Cinema("KinoGram", "KinoGram")

case object KinoKultura extends Cinema("Kino Kultura", "Kultura")

case object KinoAmondo extends Cinema("Kino Amondo", "Amondo")

case object KinoNaBoku extends Cinema("Kino na Boku", "na Boku")

case object KinoGlebocka66 extends Cinema("Kino Głębocka 66", "Głębocka 66")

case object Kinomuzeum extends Cinema("KINOMUZEUM", "Kinomuzeum")

case object KinoSwit extends Cinema("Kino Świt", "Świt")

case object KinoKepa extends Cinema("Kino Kępa", "Kępa")

case object StacjaFalenica extends Cinema("KINOkawiarnia Stacja Falenica", "Stacja Falenica")

case object SluzewskiDomKultury extends Cinema("Służewski Dom Kultury", "SDK")

case object KinoAtlantic extends Cinema("Kino Atlantic", "Atlantic")

case object Kinoteka extends Cinema("Kinoteka", "Kinoteka")

case object Ujazdowski extends Cinema("Kino U-jazdowski", "U-jazdowski")

case object KinoCytadela extends Cinema("Kino Cytadela", "Cytadela")

case object KinoAlternatywy extends Cinema("Kino Alternatywy", "Alternatywy")   // Ursynowskie Centrum Kultury — own site

// ── Kraków ───────────────────────────────────────────────────────────────────

case object CinemaCityBonarka extends Cinema("Cinema City Bonarka", "Bonarka")

case object CinemaCityKazimierz extends Cinema("Cinema City Kazimierz", "Kazimierz")

case object CinemaCityZakopianka extends Cinema("Cinema City Zakopianka", "Zakopianka")

case object MultikinoKrakow extends Cinema("Multikino Kraków", "Multikino")

case object KinoMikro extends Cinema("Kino Mikro", "Mikro")

case object MikroBronowice extends Cinema("Mikro Bronowice", "Mikro Bronowice")

case object KinoSfinks extends Cinema("Kino Sfinks", "Sfinks")

// ── Łódź ─────────────────────────────────────────────────────────────────────

case object CinemaCityManufaktura extends Cinema("Cinema City Manufaktura", "Manufaktura")

case object MultikinoLodz extends Cinema("Multikino Łódź", "Multikino")

case object HeliosLodz extends Cinema("Helios Łódź", "Helios")

case object KinoCharlie extends Cinema("Kino Charlie", "Charlie")

// ── Katowice ─────────────────────────────────────────────────────────────────

case object CinemaCityPunkt44 extends Cinema("Cinema City Punkt 44", "Punkt 44")

case object CinemaCitySilesia extends Cinema("Cinema City Silesia", "Silesia")

case object MultikinoKatowice extends Cinema("Multikino Katowice", "Multikino")

case object HeliosKatowice extends Cinema("Helios Katowice", "Helios")

case object KinoKosmos extends Cinema("Kino Kosmos", "Kosmos")

case object KinoSwiatowid extends Cinema("Kino Światowid", "Światowid")

case object KinoteatrRialto extends Cinema("Kinoteatr Rialto", "Kinoteatr Rialto")

// ── Szczecin ─────────────────────────────────────────────────────────────────

case object HeliosSzczecin extends Cinema("Helios Kupiec", "Helios")

case object MultikinoSzczecin extends Cinema("Multikino Szczecin", "Multikino")

case object KinoPionier extends Cinema("Kino Pionier 1907", "Pionier")

// ── Białystok ────────────────────────────────────────────────────────────────

case object HeliosAlfa extends Cinema("Helios Alfa", "Alfa")

case object HeliosBiala extends Cinema("Helios Biała", "Biała")

case object HeliosJurowiecka extends Cinema("Helios Jurowiecka", "Jurowiecka")

case object KinoForum extends Cinema("Kino Forum", "Forum")

// ── Trójmiasto (Gdańsk · Gdynia · Sopot) ─────────────────────────────────────

case object MultikinoGdansk extends Cinema("Multikino Gdańsk", "Multikino")

case object HeliosMetropolia extends Cinema("Helios Metropolia", "Metropolia")

case object HeliosForum extends Cinema("Helios Forum", "Forum")

case object HeliosRiviera extends Cinema("Helios Riviera", "Riviera")

case object KinoSpektrum extends Cinema("Kino Spektrum", "Spektrum")

case object KinoKameralne extends Cinema("Kino Kameralne Cafe", "Kameralne")

case object KinoIkm extends Cinema("Kino IKM", "IKM")

case object KinoMuzeumGdansk extends Cinema("Kino Muzeum", "Muzeum")

case object KinoZak extends Cinema("Kino Żak", "Żak")

case object KinoPort extends Cinema("KinoPort", "KinoPort")

// ── Bydgoszcz ────────────────────────────────────────────────────────────────

case object CinemaCityBydgoszcz extends Cinema("Cinema City Bydgoszcz", "Cinema City")

case object MultikinoBydgoszcz extends Cinema("Multikino Bydgoszcz", "Multikino")

case object HeliosBydgoszcz extends Cinema("Helios Bydgoszcz", "Helios")

case object KinoOrzel extends Cinema("Kino Orzeł", "Orzeł")

// ── Lublin ───────────────────────────────────────────────────────────────────

case object CinemaCityLublinFelicity extends Cinema("Cinema City Felicity", "Felicity")

case object CinemaCityLublinPlaza extends Cinema("Cinema City Lublin Plaza", "Lublin Plaza")

case object MultikinoLublin extends Cinema("Multikino Lublin", "Multikino")

case object KinoBajka extends Cinema("Kino Bajka", "Bajka")

// ── Częstochowa ──────────────────────────────────────────────────────────────

case object CinemaCityCzestochowaJurajska extends Cinema("Cinema City Jurajska", "Jurajska")

case object CinemaCityCzestochowaWolnosc extends Cinema("Cinema City Wolność", "Wolność")

// ── Radom ────────────────────────────────────────────────────────────────────

case object HeliosRadom extends Cinema("Helios Radom", "Helios")

case object MultikinoRadom extends Cinema("Multikino Radom", "Multikino")

// ── Sosnowiec ────────────────────────────────────────────────────────────────

case object HeliosSosnowiec extends Cinema("Helios Sosnowiec", "Helios")

case object CinemaCitySosnowiec extends Cinema("Cinema City Sosnowiec", "Cinema City")

// ── Toruń ────────────────────────────────────────────────────────────────────

case object CinemaCityTorunCzerwonaDroga extends Cinema("Cinema City Czerwona Droga", "Czerwona Droga")

case object CinemaCityTorunPlaza extends Cinema("Cinema City Toruń Plaza", "Plaza")

// ── Kielce ───────────────────────────────────────────────────────────────────

case object HeliosKielce extends Cinema("Helios Kielce", "Helios")

case object MultikinoKielce extends Cinema("Multikino Kielce", "Multikino")

// ── Rzeszów ──────────────────────────────────────────────────────────────────

case object HeliosRzeszow extends Cinema("Helios Rzeszów", "Helios")

case object MultikinoRzeszow extends Cinema("Multikino Rzeszów", "Multikino")

case object KinoZorza extends Cinema("Kino Zorza", "Zorza")

// ── Gliwice ──────────────────────────────────────────────────────────────────

case object CinemaCityGliwice extends Cinema("Cinema City Gliwice", "Cinema City")

// ── Zabrze ───────────────────────────────────────────────────────────────────

case object MultikinoZabrze extends Cinema("Multikino Zabrze", "Multikino")

// ── Art-house / independent additions (parity with Filmweb listings) ──────────

// Kraków
case object KinoPodBaranami extends Cinema("Kino Pod Baranami", "Pod Baranami")
case object KinoKijow       extends Cinema("Kino Kijów", "Kijów")
case object KinoKika        extends Cinema("Kino Kika", "Kika")
case object KinoAgrafka     extends Cinema("Kino Agrafka", "Agrafka")
case object KinoParadox     extends Cinema("Kino Paradox", "Paradox")
// Warszawa
case object KinoWisla       extends Cinema("Kino Wisła", "Wisła")
// Szczecin
case object HeliosOutletPark   extends Cinema("Helios Outlet Park", "Outlet Park")
case object KinoZamekSzczecin  extends Cinema("Kino Zamek", "Zamek")
// Trójmiasto
case object Cinema1Gdansk   extends Cinema("Cinema1", "Cinema1")
// Łódź
case object KinematografLodz extends Cinema("Kino Kinematograf", "Kinematograf")
case object Nckf            extends Cinema("Kino NCKF EC1", "NCKF EC1")
// Lublin
case object KinoCkLublin    extends Cinema("Kino CK Lublin", "CK")
// Częstochowa
case object OkfIluzja       extends Cinema("OKF Iluzja", "OKF Iluzja")
// Radom
case object McswElektrowniaCinema extends Cinema("Kino MCSW Elektrownia", "Elektrownia")
// Toruń
case object KinoCentrumCsw  extends Cinema("Kino Centrum CSW Toruń", "CSW")
// Kielce
case object KinoFenomen     extends Cinema("Kino Fenomen (WDK)", "Fenomen")
case object KinoMoskwa      extends Cinema("Kino Moskwa", "Moskwa")
// Rzeszów
case object KinoZaRogiemCafe extends Cinema("Kino za Rogiem Cafe", "za Rogiem")
// Gliwice
case object KinoAmok        extends Cinema("Kino Amok", "Amok")
// Zabrze
case object KinoRoma        extends Cinema("Kino Roma", "Roma")

// ── More art-house / independents (previously-skipped, now implemented) ───────
case object AdaKinoStudyjne      extends Cinema("ADA Kino Studyjne", "ADA")
case object GdynskieCentrumFilmowe extends Cinema("Gdyńskie Centrum Filmowe", "GCF")

// ── New mid-size cities — national-chain branches only (chains-first sweep) ────
// Each city's independent/art-house screen, where one exists, lands later as a
// bespoke scraper. IDs verified live against each chain's own cinema-list API.

// Olsztyn
case object HeliosOlsztyn    extends Cinema("Helios Olsztyn", "Helios")
case object MultikinoOlsztyn extends Cinema("Multikino Olsztyn", "Multikino")
// Bielsko-Biała
case object HeliosBielskoBiala     extends Cinema("Helios Bielsko-Biała", "Helios")
case object CinemaCityBielskoBiala extends Cinema("Cinema City Bielsko-Biała", "Cinema City")
// Opole
case object HeliosOpoleKarolinka extends Cinema("Helios Karolinka", "Karolinka")
case object HeliosOpoleSolaris   extends Cinema("Helios Solaris", "Solaris")
// Rybnik
case object MultikinoRybnik  extends Cinema("Multikino Rybnik", "Multikino")
case object CinemaCityRybnik extends Cinema("Cinema City Rybnik", "Cinema City")
// Gorzów Wielkopolski
case object HeliosGorzow    extends Cinema("Helios Gorzów Wielkopolski", "Helios")
case object MultikinoGorzow extends Cinema("Multikino Gorzów Wielkopolski", "Multikino")
// Elbląg
case object MultikinoElblag  extends Cinema("Multikino Elbląg", "Multikino")
case object CinemaCityElblag extends Cinema("Cinema City Elbląg", "Cinema City")
// Koszalin
case object HeliosKoszalin    extends Cinema("Helios Koszalin", "Helios")
case object MultikinoKoszalin extends Cinema("Multikino Koszalin", "Multikino")
// Kalisz
case object HeliosKalisz    extends Cinema("Helios Kalisz", "Helios")
case object MultikinoKalisz extends Cinema("Multikino Kalisz", "Multikino")
// Zielona Góra
case object CinemaCityZielonaGora extends Cinema("Cinema City Zielona Góra", "Cinema City")
// Tychy
case object MultikinoTychy extends Cinema("Multikino Tychy", "Multikino")
// Wałbrzych
case object CinemaCityWalbrzych extends Cinema("Cinema City Wałbrzych", "Cinema City")
// Tarnów
case object MultikinoTarnow extends Cinema("Multikino Tarnów", "Multikino")
// Włocławek
case object MultikinoWloclawek extends Cinema("Multikino Włocławek", "Multikino")
// Legnica
case object HeliosLegnica extends Cinema("Helios Legnica", "Helios")
// Płock
case object HeliosPlock extends Cinema("Helios Płock", "Helios")
// Bytom
case object CinemaCityBytom extends Cinema("Cinema City Bytom", "Cinema City")
// Dąbrowa Górnicza
case object HeliosDabrowaGornicza extends Cinema("Helios Dąbrowa Górnicza", "Helios")
// Nowy Sącz
case object HeliosNowySacz extends Cinema("Helios Nowy Sącz", "Helios")
// Słupsk
case object MultikinoSlupsk extends Cinema("Multikino Słupsk", "Multikino")
// Jelenia Góra
case object HeliosJeleniaGora extends Cinema("Helios Jelenia Góra", "Helios")
// Przemyśl
case object HeliosPrzemysl extends Cinema("Helios Przemyśl", "Helios")
// Konin
case object HeliosKonin extends Cinema("Helios Konin", "Helios")
case object KinoOskard  extends Cinema("Kino Oskard", "Oskard")  // Konin — bilety24

// ── Independent cinemas served via shared platform clients ────────────────────
// No bespoke scraper needed: each reuses an existing, fixture-tested client —
// FilmwebShowtimesClient (by Filmweb internal cinema id), Bilety24Client (by
// bilety24.pl venue URL), or NoveKinoClient (by novekino.pl slug). Filmweb ids
// were verified to return non-empty seances on current dates.
case object KinoAwangarda2      extends Cinema("Kino Awangarda 2", "Awangarda 2")     // Olsztyn — filmweb
case object KinoKryterium       extends Cinema("Kino Kryterium", "Kryterium")         // Koszalin — filmweb
case object KinoRejs            extends Cinema("Kino Rejs", "Rejs")                   // Słupsk — filmweb
case object KinoKreska          extends Cinema("Kino Kreska", "Kreska")              // Bielsko-Biała — filmweb
case object KinoMeduza          extends Cinema("Kino Meduza", "Meduza")              // Opole — filmweb
case object KinoKadr            extends Cinema("Kino Studyjne Kadr", "Kadr")          // Dąbrowa Górnicza — filmweb
case object KinoSokol           extends Cinema("Kino Sokół", "Sokół")               // Nowy Sącz — filmweb
case object KinoMillenium       extends Cinema("Kino Millenium", "Millenium")         // Tarnów — filmweb
case object Kino60Krzesel       extends Cinema("Kino 60 Krzeseł", "60 Krzeseł")      // Gorzów Wielkopolski — filmweb
case object KinoTatry           extends Cinema("Kino Tatry", "Tatry")               // Łódź — filmweb
case object KinoChatkaZaka      extends Cinema("Kino Chatka Żaka", "Chatka Żaka")    // Lublin — filmweb
case object KinoApolloWalbrzych extends Cinema("Kino Apollo Wałbrzych", "Apollo")     // Wałbrzych — bilety24 (DCF)
case object KinoPiast           extends Cinema("Kino Piast", "Piast")               // Legnica — bilety24 (DCF)
case object KinoLot             extends Cinema("Kino Lot", "Lot")                   // Jelenia Góra — bilety24 (DCF)
case object KinoPrzedwiosnie    extends Cinema("Kino Przedwiośnie", "Przedwiośnie")  // Płock — novekino
// Konin catchment: the remaining venues Filmweb lists under its Konin showtimes
// page, each in a nearby town. Helios Konin (chain client) and Kino Oskard
// (Bilety24, above) are already covered; Września's Kino Trójka is excluded.
case object KinoZacheta         extends Cinema("Kino Zachęta", "Zachęta")            // Kleczew — filmweb
case object KinoNadWarta        extends Cinema("Kino nad Wartą", "nad Wartą")        // Koło — bilety24 1621 (filmweb 1526 lists a day or two)
case object KinoStudyjneCentrum extends Cinema("Kino Studyjne Centrum", "Centrum")  // Konin — bilety24
case object KinoHel             extends Cinema("Kino Hel", "Hel")                   // Pleszew — filmweb
case object KinoSokolnia        extends Cinema("Kino Sokolnia", "Sokolnia")          // Słupca — filmweb
case object KinoTur             extends Cinema("Kino Tur", "Tur")                   // Turek — filmweb
case object KinoMok             extends Cinema("Kino MOK", "MOK")                   // Zagórów — filmweb

// ── Filmweb catchment cinemas (nearby towns Filmweb groups under each city) ──
// Each is served by FilmwebShowtimesClient via its Filmweb internal cinema id
// (verified non-empty seances 2026-06). Wired in `PolishScraperCatalog`, under its town's group.
// wroclaw
case object KinoAstra extends Cinema("Kino Astra", "Astra")   // Oborniki Śląskie — kulturalne-oborniki.bilety24.pl
case object KinoDyskusyjnyKlubFilmowyPolitechnika extends Cinema("Dyskusyjny Klub Filmowy Politechnika", "Wrocław")   // Wrocław — filmweb 1645
// warszawa
case object KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha extends Cinema("Kino Praha", "Praha")   // Warszawa — filmweb 2180
// lodz
case object KinoSpojnia extends Cinema("Kino Spójnia", "Spójnia")   // Aleksandrów Łódzki — filmweb 2403
case object KinoStaryMlyn extends Cinema("Kino Stary Młyn", "Stary Młyn")   // Zgierz — filmweb 2443
// katowice
case object CinemaCity extends Cinema("Cinema City Ruda Śląska", "Cinema City")   // Ruda Śląska — filmweb 388
case object KinoPatria extends Cinema("Patria", "Patria")   // Ruda Śląska — filmweb 352
// szczecin
case object KinoKawiarnia extends Cinema("Kino Kawiarnia", "Kawiarnia")   // Goleniów — filmweb 117
case object KinoPDK extends Cinema("Kino PDK", "PDK")   // Pyrzyce — filmweb 2363
case object KinoSCK extends Cinema("Kino SCK", "SCK")   // Stargard — filmweb 1941
// bialystok
case object KinoSokolSokolka extends Cinema("Kino Sokół Sokółka", "Sokół")   // Sokółka — filmweb 1659
// trojmiasto
case object KinoNaSzekspirowskim extends Cinema("Kino na Szekspirowskim", "Gdańsk")   // Gdańsk — filmweb 2100
case object MultikinoRumia extends Cinema("Multikino Rumia", "Rumia")   // Rumia — filmweb 1464
// bydgoszcz
case object KinoKinomax extends Cinema("Kinomax", "Kinomax")   // Inowrocław — filmweb 1124
case object KinoRondo extends Cinema("Kinoteatr Rondo", "Rondo")   // Chełmno — filmweb 3121
// lublin
case object KinoLewart extends Cinema("Kino Lewart", "Lewart")   // Lubartów — filmweb 1697
case object KinoMetalowiec extends Cinema("Metalowiec", "Metalowiec")   // Kraśnik — filmweb 285
// czestochowa
case object KinoDKFRumcajs extends Cinema("DKF Rumcajs", "DKF Rumcajs")   // Częstochowa — rumcajs.czest.pl
case object KinoKarolinka extends Cinema("Kino Karolinka", "Karolinka")   // Lubliniec — filmweb 1719
case object KinoMDK extends Cinema("Kino MDK", "MDK")   // Radomsko — filmweb 1732
case object KinoMOKCentrum extends Cinema("Kino MOK Centrum", "MOK Centrum")   // Zawiercie — filmweb 1525
case object KinoZacisze extends Cinema("Zacisze", "Zacisze")   // Piekary Śląskie — filmweb 2350
// radom
case object HeliosStarachowice extends Cinema("Helios Starachowice", "Starachowice")   // Starachowice — filmweb 1845
case object KinoCentrumSkarzyskoKamienna extends Cinema("Kino Centrum Skarżysko-Kamienna", "Centrum")   // Skarżysko-Kamienna — filmweb 1705
case object KinoGornik extends Cinema("Kino Górnik", "Górnik")   // Szydłowiec — filmweb 3064
case object KinoKozienickiDomKultury extends Cinema("Kozienicki Dom Kultury", "Kozienice")   // Kozienice — filmweb 1913
case object KinoKuznica extends Cinema("Kino Kuźnica", "Kuźnica")   // Suchedniów — filmweb 1713
case object KinoSwitZwolen extends Cinema("Świt Zwoleń", "Świt")   // Zwoleń — filmweb 2342
// torun
case object KinoMiejskieCentrumKultury extends Cinema("Miejskie Centrum Kultury", "Aleksandrów")   // Aleksandrów Kujawski — filmweb 3119
case object KinoZdroj extends Cinema("Kino Zdrój", "Zdrój")   // Ciechocinek — filmweb 1771
// kielce
case object KinoCK extends Cinema("Kino CK", "CK")   // Jędrzejów — filmweb 2351
case object KinoKoneckieCentrumKultury extends Cinema("Koneckie Centrum Kultury", "Końskie")   // Końskie — filmweb 3128
// rzeszow
case object HeliosKrosno extends Cinema("Helios Krosno", "Krosno")   // Krosno — filmweb 2014
case object KinoArtKino extends Cinema("artKino", "artKino")   // Krosno — filmweb 1122
case object KinoJednosc extends Cinema("Kino Jedność", "Jedność")   // Sędziszów Małopolski — filmweb 2335
case object KinoMCK extends Cinema("Kino MCK", "MCK")   // Leżajsk — filmweb 2344
case object KinoSniezka extends Cinema("Kino Śnieżka", "Śnieżka")   // Dębica — filmweb 1500
case object KinoSokolBrzozow extends Cinema("Kino Sokół Brzozów", "Sokół")   // Brzozów — filmweb 1477
case object KinoWarszawa extends Cinema("Kino Warszawa", "Warszawa")   // Przeworsk — filmweb 2346
// gliwice
case object KinoScenaKultura extends Cinema("Kino Scena Kultura", "Scena Kultura")   // Knurów — kinoscenakultura.pl
// olsztyn
case object KinoCinemaLumiere extends Cinema("Cinema Lumiere", "Cinema Lumiere")   // Szczytno — filmweb 2357
case object KinoIgnacy extends Cinema("Kino Ignacy", "Ignacy")   // Lidzbark Warmiński — filmweb 2354
case object KinoNarie extends Cinema("Kino Narie", "Narie")   // Morąg — filmweb 2355
// bielsko-biala
case object KinoJanosik extends Cinema("Janosik", "Janosik")   // Żywiec — filmweb 157
case object KinoPckulKino extends Cinema("Kino PCKul", "Pszczyna")   // Pszczyna — filmweb 3248
case object KinoSwitCzechowiceDziedzice extends Cinema("Świt", "Świt")   // Czechowice-Dziedzice — filmweb 135
case object KinoTeatrElektryczny extends Cinema("Teatr Elektryczny", "Skoczów")   // Skoczów — filmweb 3141
case object KinoWislaBrzeszcze extends Cinema("Kino Wisła Brzeszcze", "Wisła")   // Brzeszcze — filmweb 1490
case object MultikinoCzechowiceDziedzice extends Cinema("Multikino Czechowice-Dziedzice", "Multikino")   // Czechowice-Dziedzice — filmweb 1776
// opole
case object HeliosKedzierzynKozle extends Cinema("Helios Kędzierzyn-Koźle", "Helios")   // Kędzierzyn-Koźle — filmweb 1703
case object KinoBajkaKluczbork extends Cinema("Kino Bajka Kluczbork", "Bajka")   // Kluczbork — filmweb 2320
case object KinoChemik extends Cinema("Chemik", "Chemik")   // Kędzierzyn-Koźle — filmweb 619
case object KinoDiana extends Cinema("Kino Diana", "Diana")   // Prudnik — filmweb 2343
case object KinoKrapkowice extends Cinema("Kino Krapkowice", "Krapkowice")   // Krapkowice — filmweb 1681
case object KinoStudio extends Cinema("Studio", "Studio")   // Opole — filmweb 431
case object KinoTwierdza extends Cinema("Kino Twierdza", "Twierdza")   // Kędzierzyn-Koźle — filmweb 1672
// rybnik
case object HeliosZory extends Cinema("Helios Żory", "Helios")   // Żory — filmweb 2326
case object KinoBaltyk extends Cinema("Bałtyk", "Bałtyk")   // Racibórz — filmweb 168
case object KinoCentrum extends Cinema("Centrum", "Centrum")   // Jastrzębie Zdrój — filmweb 514
case object KinoNaStarowce extends Cinema("Na Starówce", "Na Starówce")   // Żory — filmweb 588
case object KinoPegaz extends Cinema("Pegaz", "Pegaz")   // Wodzisław Śląski — filmweb 3148
case object KinoTeatrZiemiRybnickiej extends Cinema("Teatr Ziemi Rybnickiej", "Rybnik")   // Rybnik — filmweb 3140
// elblag
case object HeliosTczew extends Cinema("Helios Tczew", "Helios")   // Tczew — filmweb 1673
case object KinoBaszta extends Cinema("Baszta", "Baszta")   // Braniewo — filmweb 2352
case object KinoPowisle extends Cinema("Kino Powiśle", "Powiśle")   // Sztum — filmweb 1798
case object KinoZulawskiOsrodekKultury extends Cinema("Żuławski Ośrodek Kultury", "Nowy")   // Nowy Dwór Gdański — filmweb 3131
// koszalin
case object KinoBajkaDarlowo extends Cinema("Bajka", "Bajka")   // Darłowo — filmweb 255
case object KinoCentrumBialogard extends Cinema("Kino Centrum", "Centrum")   // Białogard — filmweb 1675
case object KinoDK extends Cinema("Kino DK", "DK")   // Sławno — filmweb 2365
case object KinoGOK extends Cinema("Kino GOK", "GOK")   // Tychowo — filmweb 2414
case object KinoGoplana extends Cinema("Kino Goplana", "Goplana")   // Połczyn — filmweb 1864
case object KinoWybrzeze extends Cinema("Wybrzeże", "Wybrzeże")   // Kołobrzeg — filmweb 276
// kalisz
case object HeliosOstrowWlkp extends Cinema("Helios Ostrów Wlkp.", "Ostrów Wlkp.")   // Ostrów Wlkp. — filmweb 2372
case object KinoCentrum3D extends Cinema("Centrum 3D", "Centrum 3D")   // Kalisz — filmweb 1513
case object KinoEcho extends Cinema("Kino Echo", "Echo")   // Jarocin — filmweb 1484
case object KinoPiastOstrzeszow extends Cinema("Piast", "Piast")   // Ostrzeszów — filmweb 2359
case object KinoPrzedwiosnieKrotoszyn extends Cinema("Przedwiośnie", "Przedwiośnie")   // Krotoszyn — filmweb 1121
// zielona-gora
case object KinoEuropa extends Cinema("Europa", "Europa")   // Nowa Sól — filmweb 1955
case object KinoMaxKino extends Cinema("Max Kino", "Max Kino")   // Świebodzin — filmweb 2171
case object KinoPionierZary extends Cinema("Pionier", "Pionier")   // Żary — filmweb 1430
case object KinoSDKSwiebodzin extends Cinema("Kino ŚDK", "ŚDK")   // Świebodzin — filmweb 2331
// tychy
case object KinoNaszeKino extends Cinema("Nasze Kino", "Nasze Kino")   // Oświęcim — filmweb 1480
case object KinoPlanetCinema extends Cinema("Planet Cinema", "Planet Cinema")   // Oświęcim — MSI
// walbrzych
case object KinoMOKNowaRuda extends Cinema("Kino MOK Nowa Ruda", "MOK")   // Nowa Ruda — filmweb 1493
case object KinoMOKiS extends Cinema("Kino MOKiS", "MOKiS")   // Bielawa — filmweb 197
case object KinoSleza extends Cinema("Kino Ślęża", "Ślęża")   // Sobótka — filmweb 2410
case object KinoZbyszek extends Cinema("Kino Zbyszek", "Zbyszek")   // Dzierżoniów — filmweb 1530
case object MultikinoKlodzko extends Cinema("Multikino Kłodzko", "Multikino")   // Kłodzko — filmweb 2990
case object MultikinoSwidnica extends Cinema("Multikino Świdnica", "Świdnica")   // Świdnica — filmweb 2993
// tarnow
case object KinoFarys extends Cinema("Farys", "Farys")   // Biecz — filmweb 2315
case object KinoGCK extends Cinema("Kino GCK", "GCK")   // Solec-Zdrój — filmweb 2411
case object KinoKolory extends Cinema("Kino Kolory", "Kolory")   // Gorlice — filmweb 2404
case object KinoPlaneta extends Cinema("Kino Planeta", "Planeta")   // Brzesko — filmweb 1481
case object KinoPromien extends Cinema("Kino Promień", "Promień")   // Tuchów — kinotuchow.pl
case object KinoRegis extends Cinema("Regis", "Regis")   // Bochnia — filmweb 1294
case object KinoSokolDabrowaTarnowska extends Cinema("Kino Sokół Dąbrowa Tarnowska", "Sokół")   // Dąbrowa Tarnowska — filmweb 1488
// wloclawek
case object KinoJutrzenka extends Cinema("Jutrzenka", "Jutrzenka")   // Sierpc — filmweb 2341
case object KinoNawojka extends Cinema("Kino Nawojka", "Nawojka")   // Lipno — filmweb 3130
case object KinoNoweKinoWarszawa extends Cinema("Nowe Kino Warszawa", "Gostynin")   // Gostynin — filmweb 3246
// legnica
case object HeliosLubin extends Cinema("Helios Lubin", "Lubin")   // Lubin — filmweb 1420
case object KinoAurum extends Cinema("Kino Aurum", "Aurum")   // Złotoryja — filmweb 1718
case object KinoCyfroweKino extends Cinema("Cyfrowe Kino", "Cyfrowe Kino")   // Środa Śląska — filmweb 2313
case object KinoForumBoleslawiec extends Cinema("Forum", "Forum")   // Bolesławiec — filmweb 373
case object KinoMuzaLubin extends Cinema("Muza", "Muza")   // Lubin — filmweb 288
case object KinoPCA extends Cinema("Kino PCA", "PCA")   // Polkowice — filmweb 1139
// plock
case object KinoZaRogiem extends Cinema("Kino za Rogiem", "za Rogiem")   // Płock — filmweb 1949
case object KinoKDK extends Cinema("Kino KDK", "KDK")   // Kutno — filmweb 1134
case object KinoKalejdoskop extends Cinema("Kino Kalejdoskop", "Kalejdoskop")   // Płońsk — filmweb 1702
case object KinoODEON extends Cinema("Kino ODEON", "ODEON")   // Sochaczew — filmweb 2128
// nowy-sacz
case object KinoJaworzyna extends Cinema("Jaworzyna", "Jaworzyna")   // Krynica Zdrój — filmweb 561
case object KinoKlaps extends Cinema("Klaps", "Klaps")   // Limanowa — filmweb 1137
// slupsk
case object KinoFregata extends Cinema("Fregata", "Fregata")   // Lębork — filmweb 2123
// jelenia-gora
case object KinoWawel extends Cinema("Kino Wawel", "Wawel")   // Lubań — filmweb 1721
// przemysl
case object KinoCentrum3DPrzemysl extends Cinema("Centrum 3D Przemyśl", "Centrum 3D")   // Przemyśl — ck.przemysl.pl (was filmweb 1786, went empty)
case object KinoIkar extends Cinema("Kino Ikar", "Ikar")   // Jarosław — filmweb 1707
case object KinoNaBiegunach extends Cinema("Kino Na Biegunach", "Na Biegunach")   // Jarosław — filmweb 2172
case object KinoSDK extends Cinema("Kino SDK", "SDK")   // Sanok — bilety.sdksanok.pl

// ── Nearby-town cinemas and new regional hubs (2026-09 sweep) ─────────────────
// Every venue Filmweb, bilety24 or a chain lists as currently screening in a town
// no page covered yet. A town within ~45 km of an existing city joins that city;
// the rest are grouped into regions around their largest town, every venue about
// an hour's drive from it. Wired in `PolishScraperCatalog`, under its town's group; the page
// that lists each is generated into `PolishPages`.
// szczecin
case object KinoMOKPolice extends Cinema("Kino MOK Police", "MOK Police")   // Police — biletyna Miejski-Osrodek-Kultury
case object KinoEva extends Cinema("Kino Eva", "Eva")   // Międzyzdroje — own site kino-eva.com.pl
// bielsko-biala
case object KinoCentrumWadowice extends Cinema("Kino Centrum Wadowice", "Centrum Wadowice")   // Wadowice — biletyna Wadowickie-Centrum-Kultury
case object CinemaCityCieszyn extends Cinema("Cinema City Cieszyn", "Cieszyn")   // Cieszyn — Cinema City API
case object KinoCKiF extends Cinema("Kino CKiF", "CKiF")   // Sucha Beskidzka — systembiletowy bilety.ckifsucha.pl
// warszawa
case object KinoGrodziskieCentrumKultury extends Cinema("Kino Grodziskiego Centrum Kultury", "GCK Grodzisk")   // Grodzisk Mazowiecki — bilety24
case object KinoBajkaBlonie extends Cinema("Kino Bajka Błonie", "Bajka")   // Błonie — kino.blonie.pl
case object KinoGrojeckiOsrodekKultury extends Cinema("Grójecki Ośrodek Kultury", "Grójec")   // Grójec — filmweb 1683
case object KinoUciecha extends Cinema("Kino Uciecha", "Uciecha")   // Góra Kalwaria — biletyna Kino-Uciecha
case object HeliosWolomin extends Cinema("Helios Wołomin", "Helios Wołomin")   // Wołomin — Helios REST
case object KinoKulturaWolomin extends Cinema("Kino Kultura Wołomin", "Kultura Wołomin")   // Wołomin — biletyna Kino-Kultura
case object KinoTeatrKurtyna extends Cinema("Kino-Teatr Kurtyna", "Kurtyna")   // Sulejówek — filmweb 2079
case object MultikinoPruszkow extends Cinema("Multikino Pruszków", "Multikino Pruszków")   // Pruszków — Multikino API
case object KinoCKiSPruszkow extends Cinema("Kino CKiS Pruszków", "CKiS Pruszków")   // Pruszków — biletyna CDK Sala widowiskowa
case object HeliosLegionowo extends Cinema("Helios Legionowo", "Helios Legionowo")   // Legionowo — Helios REST
case object KinoOaza extends Cinema("Kino Oaza", "Oaza")   // Otwock — biletyna Kino-Oaza-Otwockie-Centrum-Kultury
// lodz
case object KinoTomi extends Cinema("Kino Tomi", "Tomi")   // Pabianice — own site kinotomi.pl
case object HeliosPabianice extends Cinema("Helios Pabianice", "Helios Pabianice")   // Pabianice — Helios REST
case object KinoGornikLeczyca extends Cinema("Kino Górnik Łęczyca", "Górnik")   // Łęczyca — MSI kinogornik.eurobilet.pl
// dabrowa-gornicza
case object KinoZbyszekOlkusz extends Cinema("Kino Zbyszek Olkusz", "Zbyszek")   // Olkusz — biletyna Kino-Zbyszek
case object MultikinoJaworzno extends Cinema("Multikino Jaworzno", "Multikino")   // Jaworzno — Multikino API
// krakow
case object KinoMuzaMyslenice extends Cinema("Kino Muza Myślenice", "Muza Myślenice")   // Myślenice — systembiletowy bilety.kino.myslenice.pl
case object KinoRadosc extends Cinema("Kino Radość", "Radość")   // Wolbrom — ekobilet dk-wolbrom
case object KinoGryf extends Cinema("Kino Gryf", "Gryf")   // Miechów — systembiletowy bilety.ckis.miechow.eu
case object KinoWazka extends Cinema("Kino Ważka", "Ważka")   // Niepołomice — bilety24
case object KinoWielickaMediateka extends Cinema("Kino Wielicka Mediateka", "Wielicka Mediateka")   // Wieliczka — ekobilet kino-wielicka-mediateka
case object KinoLubon extends Cinema("Kino Luboń 2.0", "Luboń")   // Mszana Dolna — bilety24
// poznan
case object KinoBasztaSroda extends Cinema("Kino Baszta Środa", "Baszta Środa")   // Środa Wielkopolska — bilety24
case object KinoHalszka extends Cinema("Kino Halszka", "Halszka")   // Szamotuły — bilety24
case object KinoteatrSlonko extends Cinema("Kinoteatr Słonko", "Słonko")   // Śrem — bilety24
case object KinoWielkopolanin extends Cinema("Kino Wielkopolanin", "Wielkopolanin")   // Buk — bilety24
case object KinoTrojka extends Cinema("Kino Trójka", "Trójka")   // Września — bilety24
case object KinoOpalenica extends Cinema("Kino Opalenica", "Opalenica")   // Opalenica — bilety24
case object KinoteatrGwiazda extends Cinema("Kinoteatr Gwiazda", "Gwiazda")   // Wronki — bilety24
// opole
case object KinoCentrumBrzeg extends Cinema("Kino Centrum Brzeg", "Centrum Brzeg")   // Brzeg — bilety24
case object KinoCinemaN extends Cinema("Cinema N 3D", "Cinema N")   // Nysa — systembiletowy bilety.kino.nysa.pl
// wroclaw
case object KinoOdraBrzegDolny extends Cinema("Kino Odra Brzeg Dolny", "Odra Brzeg Dolny")   // Brzeg Dolny — bilety24
case object KinoOdraOlawa extends Cinema("Kino Odra Oława", "Odra Oława")   // Oława — bilety24
case object GoKinoOlawa extends Cinema("GO KINO Oława", "GO KINO")   // Oława — MSI olawa.gokino.pl
case object KinoGrazyna extends Cinema("Kino Grażyna", "Grażyna")   // Strzelin — MSI bilety.kinostrzelin.pl
case object KinoPoloniaTrzebnica extends Cinema("Kino Polonia Trzebnica", "Polonia")   // Trzebnica — bilety24
case object KinoMOKiSOlesnica extends Cinema("Kino MOKiS Oleśnica", "MOKiS Oleśnica")   // Oleśnica — bilety24
// kielce
case object KinoZdrojBusko extends Cinema("Kino Zdrój Busko", "Zdrój")   // Busko-Zdrój — biletyna BSCK
case object KinoMuzaWloszczowa extends Cinema("Kino Muza Włoszczowa", "Muza Włoszczowa")   // Włoszczowa — biletyna Dom-Kultury-we-Wloszczowie
// trojmiasto
case object KinoWCK extends Cinema("Wejherowskie Centrum Kultury", "WCK Wejherowo")   // Wejherowo — iKsoris bilety.wck.org.pl
case object KinoZaRogiemChmielno extends Cinema("Kino za Rogiem Chmielno", "Chmielno")   // Chmielno — biletyna Kino-za-Rogiem-w-Chmielnie
case object KinoZeglarz extends Cinema("Kino Żeglarz", "Żeglarz")   // Jastarnia — bilety24
case object KinoKCK extends Cinema("Kino Kartuskie Centrum Kultury", "KCK Kartuzy")   // Kartuzy — bilety24
case object KinoCKiSPruszczGdanski extends Cinema("Kino CKiS Pruszcz Gdański", "Pruszcz Gdański")   // Pruszcz Gdański — bilety24
// bydgoszcz
case object KinoRelaks extends Cinema("Kino Relaks", "Relaks")   // Nakło nad Notecią — biletyna Nakielski-Osrodek-Kultury
// tarnow
case object KinoMCKRadlow extends Cinema("Kino MCK Radłów", "MCK Radłów")   // Radłów — filmweb 2408
case object KinoCentrumLisiaGora extends Cinema("Kino Centrum Lisia Góra", "Lisia Góra")   // Lisia Góra — bilety24
case object KinoJDK extends Cinema("Kino JDK", "JDK Jasło")   // Jasło — systembiletowy jdk.systembiletowy.pl
case object MultikinoMielec extends Cinema("Multikino Mielec", "Multikino Mielec")   // Mielec — Multikino API
// gorzow-wielkopolski
case object KinoPanorama extends Cinema("Kino Panorama", "Panorama")   // Barlinek — bilety24
// nowy-sacz
case object KinoMCKGrybow extends Cinema("Kino MCKCziE Grybów", "Grybów")   // Grybów — bilety24
// lublin
case object KinoCzyn extends Cinema("Kino-Teatr Czyn", "Czyn")   // Poniatowa — bilety24
// torun
case object HeliosGrudziadz extends Cinema("Helios Grudziądz", "Grudziądz")   // Grudziądz — Helios REST
// wloclawek
case object KinoBaltykRypin extends Cinema("Kino Bałtyk Rypin", "Bałtyk Rypin")   // Rypin — bilety24
// slupsk
case object KinoLeba extends Cinema("Kino MBP Łeba", "Łeba")   // Łeba — bilety24
// legnica
case object MultikinoGlogow extends Cinema("Multikino Głogów", "Multikino Głogów")   // Głogów — Multikino API
// piotrkow-trybunalski
case object HeliosPiotrkow extends Cinema("Helios Piotrków Trybunalski", "Helios Piotrków")   // Piotrków Trybunalski — Helios REST
case object HeliosBelchatow extends Cinema("Helios Bełchatów", "Helios Bełchatów")   // Bełchatów — Helios REST
case object KinoKulturaBelchatow extends Cinema("Kino Kultura Bełchatów", "Kultura")   // Bełchatów — iKsoris bilety.mckbelchatow.pl
case object HeliosTomaszow extends Cinema("Helios Tomaszów Mazowiecki", "Helios Tomaszów")   // Tomaszów Mazowiecki — Helios REST
// siedlce
case object HeliosSiedlce extends Cinema("Helios Siedlce", "Helios")   // Siedlce — Helios REST
case object NoveKinoSiedlce extends Cinema("Nove Kino Siedlce", "Nove Kino")   // Siedlce — NoveKino siedlce
case object KinoSokolSokolowPodlaski extends Cinema("Kino Sokół Sokołów Podlaski", "Sokół")   // Sokołów Podlaski — filmweb 2073
case object KinoSlawa extends Cinema("Kino Sława", "Sława")   // Międzyrzec Podlaski — biletyna Kino-Slawa
case object KinoKongres extends Cinema("Kino Kongres", "Kongres")   // Węgrów — biletyna Wegrowski-Osrodek-Kultury
case object KinoLukow extends Cinema("Kino Łuków", "Łuków")   // Łuków — own site kino.lukow.pl
case object CinemaCityBialaPodlaska extends Cinema("Cinema City Biała Podlaska", "Cinema City")   // Biała Podlaska — Cinema City API
case object KinoWilga extends Cinema("Kino Wilga", "Wilga")   // Garwolin — systembiletowy grl.systembiletowy.pl
// pila
case object HeliosPila extends Cinema("Helios Piła", "Helios")   // Piła — Helios REST
case object KinoWCKWalcz extends Cinema("Kino Wałeckie Centrum Kultury", "Wałcz")   // Wałcz — bilety24
case object KinoOsiedlowe extends Cinema("Kino Osiedlowe", "Osiedlowe")   // Trzcianka — bilety24
case object KinoRodlo extends Cinema("Kino Rodło", "Rodło")   // Złotów — biletyna Zlotowskie-Centrum-Aktywnosci-Spolecznej
case object KinoMewaBudzyn extends Cinema("Kino Mewa Budzyń", "Mewa")   // Budzyń — bilety24
case object KinoNotec extends Cinema("Kino Noteć", "Noteć")   // Chodzież — bilety24
case object KinoZaRogiemWielen extends Cinema("Kino za Rogiem Wieleń", "Wieleń")   // Wieleń — bilety24
case object KinoMDKWagrowiec extends Cinema("Kino MDK Wągrowiec", "MDK Wągrowiec")   // Wągrowiec — bilety24
case object KinoSwiatowidCzarnkow extends Cinema("Kino Światowid Czarnków", "Światowid")   // Czarnków — bilety24
// ostrowiec-swietokrzyski
case object KinoEtiuda extends Cinema("Kino Etiuda", "Etiuda")   // Ostrowiec Świętokrzyski — bilety24
case object KinoOOK extends Cinema("Opatowski Ośrodek Kultury", "Opatów")   // Opatów — biletyna Opatowski-Osrodek-Kultury
case object KinoStarowka extends Cinema("Kino Starówka", "Starówka")   // Sandomierz — ekobilet kinostarowka
// gniezno
case object HeliosGniezno extends Cinema("Helios Gniezno", "Helios")   // Gniezno — Helios REST
case object KinoWawrzyn extends Cinema("Kino Wawrzyn", "Wawrzyn")   // Mogilno — bilety24
case object KinoMiescisko extends Cinema("Kino Mieścisko", "Mieścisko")   // Mieścisko — bilety24
// suwalki
case object KinoCinemaLumiereSuwalki extends Cinema("Cinema Lumiere Suwałki", "Cinema Lumiere")   // Suwałki — MSI suwalki.cinema-lumiere.pl
case object KinoIskra extends Cinema("Kino Iskra", "Iskra")   // Augustów — own site kino-iskra.pl
case object KinoKulturaGoldap extends Cinema("Kino Kultura Gołdap", "Gołdap")   // Gołdap — bilety24
case object KinoMazur extends Cinema("Kino Mazur", "Mazur")   // Olecko — ekobilet mazury-olecko
case object KinoECK extends Cinema("Kino ECK", "ECK")   // Ełk — biletyna Elckie-Centrum-Kultury
case object KinoPlanetCinemaElk extends Cinema("Planet Cinema Ełk", "Planet Cinema")   // Ełk — MSI
// stalowa-wola
case object HeliosStalowaWola extends Cinema("Helios Stalowa Wola", "Helios")   // Stalowa Wola — Helios REST
case object KinoWrzos extends Cinema("Kino Wrzos", "Wrzos")   // Stalowa Wola — bilety24
case object KinoSokolNisko extends Cinema("Kino Sokół Nisko", "Sokół")   // Nisko — own site nck.nisko.pl
case object KinoMetalowiecNowaDeba extends Cinema("Kino Metalowiec Nowa Dęba", "Metalowiec")   // Nowa Dęba — own site soknowadeba.pl
case object KinoBCK extends Cinema("Kino BCK", "BCK")   // Biłgoraj — iKsoris bilet.bck.lbl.pl
// zamosc
case object KinoStylowy extends Cinema("CKF Stylowy", "Stylowy")   // Zamość — own site stylowy.net
case object KinoZorzaChelm extends Cinema("Kino Zorza Chełm", "Zorza")   // Chełm — bilety24
case object KinoMorskieOko extends Cinema("Kino Morskie Oko", "Morskie Oko")   // Krasnystaw — biletyna Kino-Morskie-Oko
case object KinoPlon extends Cinema("Kino Plon", "Plon")   // Hrubieszów — iKsoris kinoplon.pl
// leszno
case object MultikinoLeszno extends Cinema("Multikino Leszno", "Multikino")   // Leszno — Multikino API
case object KinoTeatrMiejskiLeszno extends Cinema("Kino Teatru Miejskiego w Lesznie", "Teatr Miejski")   // Leszno — bilety24
case object KinoPromienRawicz extends Cinema("Kino Promień Rawicz", "Promień")   // Rawicz — biletyna Kino-Promien-w-Rawiczu
case object KinoPodKopula extends Cinema("Kino Pod Kopułą", "Pod Kopułą")   // Gostyń — bilety24
case object KinoKrobia extends Cinema("Kino Krobia", "Krobia")   // Krobia — bilety24
// lomza
case object HeliosLomza extends Cinema("Helios Łomża", "Helios")   // Łomża — Helios REST
case object KinoJantar extends Cinema("Kino Jantar", "Jantar")   // Ostrołęka — bilety24
case object KinoCKZambrow extends Cinema("Kino Centrum Kultury Zambrów", "Zambrów")   // Zambrów — own site kino.mokzambrow.pl
case object KinoOstrovia extends Cinema("Kino Ostrovia", "Ostrovia")   // Ostrów Mazowiecka — MSI bilety.kinoostrovia.pl
case object KinoWars extends Cinema("Kino Wars", "Wars")   // Wysokie Mazowieckie — own site kino.wysokiemazowieckie.pl
case object KinoDomKulturyGrajewo extends Cinema("Kino Dom Kultury Grajewo", "Grajewo")   // Grajewo — biletyna Grajewskie Centrum Kultury
// pulawy
case object KinoSybilla extends Cinema("Kino Sybilla", "Sybilla")   // Puławy — NoveKino sybilla
case object KinoRenesans extends Cinema("Kino Renesans", "Renesans")   // Ryki — ekobilet kino-renesans
// skierniewice
case object KinoPolonez extends Cinema("Kino Polonez", "Polonez")   // Skierniewice — biletyna Kinoteatr-Polonez
case object KinoCKiSSkierniewice extends Cinema("Kino CKiS Skierniewice", "CKiS")   // Skierniewice — biletyna CKiS Sala koncertowa
case object KinoLen extends Cinema("Kino Len", "Len")   // Żyrardów — biletyna Kino-Len
case object KinoRomaRawa extends Cinema("Kino Roma Rawa Mazowiecka", "Roma")   // Rawa Mazowiecka — own site mdkrawa.pl
case object KinoFenix extends Cinema("Kino Fenix", "Fenix")   // Łowicz — systembiletowy lok.systembiletowy.pl
// starogard-gdanski
case object CinemaCityStarogard extends Cinema("Cinema City Starogard Gdański", "Cinema City")   // Starogard Gdański — Cinema City API
case object KinoRemus extends Cinema("Kino Remus", "Remus")   // Kościerzyna — own site remus.hostingasp.pl
// ciechanow
case object KinoLydynia extends Cinema("Kino Łydynia", "Łydynia")   // Ciechanów — bilety24
case object KinoKosmosMlawa extends Cinema("Kino Kosmos Mława", "Kosmos")   // Mława — biletyna Miejski-Dom-Kultury-kino-Kosmos
case object KinoSwiatowidPrzasnysz extends Cinema("Kino Światowid Przasnysz", "Światowid")   // Przasnysz — bilety24
case object KinoMDKMakow extends Cinema("Kino MDK Maków Mazowiecki", "MDK Maków")   // Maków Mazowiecki — bilety24
case object KinoApolloDzialdowo extends Cinema("Kino Apollo Działdowo", "Apollo")   // Działdowo — biletyna Miejski-Dom-Kultury
// wielun
case object KinoSyrena extends Cinema("Kino-Teatr Syrena", "Syrena")   // Wieluń — filmweb 2034
case object KinoTeatrSieradz extends Cinema("Kino Teatr Sieradz", "Sieradz")   // Sieradz — MSI bilety.scksieradz.pl
case object KinoSlonce extends Cinema("Kino Słońce", "Słońce")   // Wieruszów — filmweb 2334
case object KinoSokolniaKepno extends Cinema("Kino Sokolnia Kępno", "Sokolnia")   // Kępno — iKsoris kinosokolnia.org
// chojnice
case object KinoChDK extends Cinema("Kino ChDK", "ChDK")   // Chojnice — MSI bilety.ckchojnice.pl
case object KinoUciechaCzluchow extends Cinema("Kino Uciecha Człuchów", "Uciecha")   // Człuchów — biletyna Kino-Uciecha
case object KinoZaRogiemBrusy extends Cinema("Kino za Rogiem Brusy", "Brusy")   // Brusy — bilety24
// zgorzelec
case object MultikinoZgorzelec extends Cinema("Multikino Zgorzelec", "Multikino")   // Zgorzelec — Multikino API
case object KinoKadrBogatynia extends Cinema("Kino Kadr Bogatynia", "Kadr")   // Bogatynia — bilety24
// ilawa
case object KinoteatrPasja extends Cinema("Kinoteatr Pasja", "Pasja")   // Iława — MSI biletyick.miastoilawa.pl
case object KinoPokoj extends Cinema("Kino Pokój", "Pokój")   // Lubawa — ekobilet mok-lubawa
case object KinoteatrHarmonia extends Cinema("Kinoteatr Harmonia", "Harmonia")   // Nowe Miasto Lubawskie — bilety24
// ketrzyn
case object KinoGwiazdaKetrzyn extends Cinema("Kino Gwiazda Kętrzyn", "Gwiazda")   // Kętrzyn — own site kino.ketrzyn.pl
case object KinoZodiak extends Cinema("Kino Zodiak", "Zodiak")   // Mrągowo — ekobilet mck-mragowo
// zakopane
case object KinoSokolZakopane extends Cinema("Kino Sokół Zakopane", "Sokół")   // Zakopane — biletyna Kino-Sokol
case object KinoGiewont extends Cinema("Kino Giewont", "Giewont")   // Zakopane — biletyna Kino-Giewont
case object KinoMiejsce extends Cinema("Kino Miejsce", "Miejsce")   // Zakopane — biletyna Kino-Miejsce
case object KinoSniezkaRabka extends Cinema("Kino Śnieżka Rabka", "Śnieżka")   // Rabka-Zdrój — bilety24
// wyszkow
case object KinoDobrychFilmow extends Cinema("Kino Dobrych Filmów", "Dobrych Filmów")   // Wyszków — biletyna Wyszkowski-Osrodek-Kultury-Hutnik
case object KinoMiGOK extends Cinema("Kino MiGOK", "MiGOK")   // Łochów — biletyna Miejski-i-Gminny-Osrodek-Kultury
// zlocieniec
case object KinoMewaZlocieniec extends Cinema("Kino Mewa Złocieniec", "Mewa")   // Złocieniec — bilety24
case object KinoWKadrze extends Cinema("Kino w Kadrze", "w Kadrze")   // Warszawa — biletyna Dom-Kultury-Kadr
// slubice
case object KinoSmok extends Cinema("Kino Smok", "Smok")   // Słubice — bilety24
// wlodawa
case object KinoWDK extends Cinema("Kino WDK Włodawa", "WDK")   // Włodawa — filmweb 2312
case object KinoOKDrawsko extends Cinema("Kino OK Drawsko Pomorskie", "Drawsko")   // Drawsko Pomorskie — bilety24

// biletyna.pl-only venues (2026-09-23 sweep of its hall sitemap): screening now,
// listed nowhere else we read.
case object KinoMCKTkacz extends Cinema("MCK Tkacz", "Tkacz")   // Tomaszów Mazowiecki — biletyna
case object KinoMDKOpoczno extends Cinema("MDK Opoczno", "Opoczno")   // Opoczno — biletyna
case object KinoDomSztukiUrsynow extends Cinema("Kino Domu Sztuki", "Dom Sztuki")   // Warszawa — biletyna
case object KinoTerminalKultury extends Cinema("Terminal Kultury Gocław", "Terminal Kultury")   // Warszawa — biletyna
case object KinoBielanskiOsrodekKultury extends Cinema("Bielański Ośrodek Kultury", "BOK Bielany")   // Warszawa — biletyna
case object KinoWawerskaStrefaKultury extends Cinema("Wawerska Strefa Kultury", "Wawer")   // Warszawa — biletyna
case object KinoBasn extends Cinema("Kino Baśń", "Baśń")   // Piastów — biletyna
case object KinoKasynoOficerskie extends Cinema("Kasyno Oficerskie", "Kasyno Oficerskie")   // Nowy Dwór Mazowiecki — biletyna
case object KinoNowyPafawag extends Cinema("Centrum Kultury Nowy Pafawag", "Nowy Pafawag")   // Wrocław — biletyna
case object TeatrBoto extends Cinema("Teatr Boto", "Boto")   // Sopot — biletyna
case object TeatrAtelier extends Cinema("Teatr Atelier", "Atelier")   // Sopot — biletyna
case object KinoCKGniewino extends Cinema("Centrum Kultury Gniewino", "Gniewino")   // Gniewino — biletyna
case object KinoMOKGlogow extends Cinema("MOK Głogów", "MOK Głogów")   // Głogów — biletyna
case object TeatrGryphius extends Cinema("Teatr im. Andreasa Gryphiusa", "Gryphius")   // Głogów — biletyna
case object KinoCKSwiecie extends Cinema("Centrum Kultury w Świeciu", "Świecie")   // Świecie — biletyna
case object KinoMOKGlowno extends Cinema("MOK Głowno", "Głowno")   // Głowno — biletyna
case object KinoPromienWiecbork extends Cinema("Kino Promień Więcbork", "Promień")   // Więcbork — biletyna
case object KinoCKiSSepolno extends Cinema("CKiS Sępólno Krajeńskie", "Sępólno")   // Sępólno Krajeńskie — biletyna
case object KinoGOKLipka extends Cinema("GOK Lipka", "Lipka")   // Lipka — biletyna
case object KinoNOKNasielsk extends Cinema("Nasielski Ośrodek Kultury", "Nasielsk")   // Nasielsk — biletyna
case object KinoFeniks extends Cinema("RCK Feniks", "Feniks")   // Rydułtowy — biletyna
case object KinoCKPrzezmierowo extends Cinema("Centrum Kultury Przeźmierowo", "Przeźmierowo")   // Przeźmierowo — biletyna
case object KinoNOKNowyTomysl extends Cinema("Nowotomyski Ośrodek Kultury", "Nowy Tomyśl")   // Nowy Tomyśl — biletyna
case object KinoMewaMiedzychod extends Cinema("Kino Mewa Międzychód", "Mewa")   // Międzychód — biletyna
case object KinoPromyk extends Cinema("GOK Promyk", "Promyk")   // Bystra — biletyna
case object KinoKsiazka extends Cinema("Kino Książka", "Książka")   // Stara Błotnica — biletyna
case object KinoChDKChoszczno extends Cinema("Choszczeński Dom Kultury", "Choszczno")   // Choszczno — biletyna
case object KinoDOKDrawno extends Cinema("Drawieński Ośrodek Kultury", "Drawno")   // Drawno — biletyna
case object KinoMGOKRecz extends Cinema("MGOK Recz", "Recz")   // Recz — biletyna
case object KinoWRatuszu extends Cinema("Kino w Ratuszu", "Ratusz")   // Zduńska Wola — MSI
case object KinoOKJastrowie extends Cinema("Ośrodek Kultury Jastrowie", "Jastrowie")   // Jastrowie — biletyna
case object KinoMarzenie extends Cinema("Kino Marzenie", "Marzenie")   // Tarnów — own site
case object KinoPlanetCinemaZabrze extends Cinema("Planet Cinema Zabrze", "Planet Cinema")   // Zabrze — MSI
case object KinoPlanetCinemaZawiercie extends Cinema("Planet Cinema Zawiercie", "Planet Cinema")   // Zawiercie — MSI
case object KinoSwiatowidElblag extends Cinema("Kino Światowid Elbląg", "Światowid Elbląg")   // Elbląg — own site

// 2026-09-23 scraper sweep: own-site / systembiletowy venues found screening now.
case object KinoBCKBytom extends Cinema("BCKino Bytom", "BCKino")   // Bytom — systembiletowy
case object KinoNowaFalaGizycko extends Cinema("Kino Nowa Fala", "Nowa Fala")   // Giżycko — own site
case object KinoOrzelUstrzyki extends Cinema("Kino Orzeł Ustrzyki Dolne", "Orzeł")   // Ustrzyki Dolne — systembiletowy
case object KinoGrajfka extends Cinema("Kino Grajfka", "Grajfka")   // Chorzów — own site

// Venues verified screening from a 2026-09-23 nearby-towns sweep. Every other
// candidate town in that sweep (Brodnica, Barcin, Białobrzegi, Kościan,
// Koluszki, Piekoszów, Kwidzyn, Solec Kujawski, and the "Kino za Rogiem"
// screens in Sompolno/Stare Miasto/Strzałkowo) turned out to have no real
// dated programme — either no scrapable source at all, or a source (biletyna
// events feed, a bilety24 organiser page, an ekobilet venue page, a coigdzie.pl
// listing) that carried zero ScreeningEvents / was dark / turned out to be a
// generic national catalog reused verbatim across unrelated towns — so none of
// them are modelled.
case object KinoKadrStaszow extends Cinema("Kino Kadr Staszów", "Kadr")   // Staszów — systemBiletowy
case object KinoSCKStrzegom extends Cinema("Kino SCK Strzegom", "SCK")   // Strzegom — biletyna
case object KinoSztuka extends Cinema("Kino Sztuka", "Sztuka")   // Chrzanów — biletyna

// small independent venues (2026-09-23 sweep, no shared ticketing platform)
case object DomKulturyLapy extends Cinema("Dom Kultury w Łapach", "Łapy")   // Łapy — own site
case object KinoBielsk extends Cinema("Kino Bielsk", "Bielsk")   // Bielsk Podlaski — own site
case object KinoNarew extends Cinema("Kino Narew", "Narew")   // Pułtusk — own site
case object KinoGrunwald extends Cinema("Kino Grunwald", "Grunwald")   // Olsztynek — own site
case object KinoParczew extends Cinema("Kino Parczew", "Parczew")   // Parczew — own site

// 2026-09-23 nearby-towns sweep, batch 2.
case object KinoMOKMiedzyrzecz extends Cinema("Kino MOK Międzyrzecz", "MOK Międzyrzecz")   // Międzyrzecz — MSI
case object KinoZaRogiemSiedlec extends Cinema("Kino za Rogiem w Siedlcu", "Siedlec")   // Siedlec — own site
// Verified screening from a 2026-09-23 nearby-towns sweep (ekobilet/systembiletowy
// venues). Iłża (CKiT, ekobilet slug centrum-kultury-i-turystyki-w-ilzy-8211) was
// checked in the same sweep and rejected: its earliest real screening is
// 2026-10-08, one day past the ~2-week verification window, so it is deliberately
// NOT modelled here.
case object KinoDKGora extends Cinema("Dom Kultury w Górze", "DK Góra")   // Góra — ekobilet
case object KinoZaciszeWasosz extends Cinema("Kino Zacisze (Wąsosz)", "Zacisze Wąsosz")   // Wąsosz — ekobilet
case object KinoMilenium extends Cinema("Kino Milenium", "Milenium")   // Milejów — ekobilet
case object KinoOpolanka extends Cinema("Kino Opolanka", "Opolanka")   // Opole Lubelskie — ekobilet
case object KinoTon extends Cinema("Kino Ton", "Ton")   // Żuromin — ekobilet
case object KinoCKiTIlza extends Cinema("Kino CKiT Iłża", "CKiT")   // Iłża — ekobilet
case object KinoBieszczadzkiDK extends Cinema("Bieszczadzki Dom Kultury", "BDK")   // Lesko — systembiletowy
// Venues verified from a 2026-09-23 lead sweep of 8 named towns. Tarnowo
// Podgórne's lead (GOK Sezam) turned out to be the same physical venue as
// KinoCKPrzezmierowo, already modelled above (Przeźmierowo is the village
// within Tarnowo Podgórne gmina where its hall sits) — not re-added. Kęty's
// biletyna place page carries only theatre/cabaret events, no films; Piaseczno's
// carries exactly one isolated film screening ever, not a recurring programme —
// neither is modelled.
case object KinoGryfinskiDomKultury extends Cinema("Gryfiński Dom Kultury", "Gryfiński DK")   // Gryfino — biletyna
case object KinoTucholskiOsrodekKultury extends Cinema("Tucholski Ośrodek Kultury", "TOK Tuchola")   // Tuchola — biletyna
case object KinoManhattanJanikowo extends Cinema("Kino Manhattan", "Manhattan")   // Janikowo — MSI
case object KinoAleKinoLibiaz extends Cinema("Ale! Kino", "Ale! Kino")   // Libiąż — MSI
case object KinoWolnoscSzczecinek extends Cinema("Kino Wolność", "Wolność")   // Szczecinek — MSI
// 2026-09-23 nearby-towns sweep, second pass. Each own site's booking link led
// to a ticketing platform we already have a client for, so all four reuse an
// existing client rather than a new one; only Kino Kultura Chojnów (own ASP.NET
// booking subsite, no browsable listing) needed a bespoke parser. Kino Café
// (Konin, kinooskard.pl/kino-cafe) was checked and deliberately NOT modelled —
// its "Kup bilet" links resolve to event ids already present in the existing
// Kino Oskard bilety24 feed, just suffixed "/Kino Cafe"; adding it would double
// the room's screenings.
case object Kino1410 extends Cinema("Kino 1410", "1410")   // Toruń — systembiletowy
case object KinoFrajda extends Cinema("Kino Frajda", "Frajda")   // Chorzów — systembiletowy
case object KinoStopiak extends Cinema("Stopiak Cinema", "Stopiak")   // Nowy Targ — MSI
case object KinoKulturaChojnow extends Cinema("Kino Kultura Chojnów", "Kultura Chojnów")   // Chojnów — own site
case object KinoLotSwidnik extends Cinema("Kino Lot Świdnik", "Lot Świdnik")   // Świdnik — biletyna
// second 2026-09-23 nearby-towns sweep: verified screening now.
case object KinoKadrTomaszowLubelski extends Cinema("Kino Kadr (Tomaszowski Dom Kultury)", "Kadr TL")   // Tomaszów Lubelski — own site
case object KinoJOK extends Cinema("Kino JOK (Janowski Ośrodek Kultury)", "JOK")   // Janów Lubelski — own site
case object KinoCKiBNowaSarzyna extends Cinema("Kino CKiB Nowa Sarzyna", "CKiB")   // Nowa Sarzyna — systembiletowy
case object KinoSokolStrzyzow extends Cinema("Kino Sokół Strzyżów", "Sokół")   // Strzyżów — own site
case object KinoRCKDrzewica extends Cinema("Kino RCK Drzewica", "RCK Drzewica")   // Drzewica — own site
case object KinoMDKMyszkow extends Cinema("Kino MDK Myszków", "MDK Myszków")   // Myszków — own site

// ── Network-level detail sources ──────────────────────────────────────────────
// Not a physical venue: a synthetic source that holds the per-film detail
// (synopsis/cast/director/genres/countries/trailer) a chain fetches ONCE per
// network and shares across all its locations, instead of each venue enriching
// its own slot. The venue slots keep their showtimes + filmUrl; the chain slot
// supplies the detail, picked up by `MovieRecord`'s film-level merged accessors.
// It extends `Cinema` so the genre fallback (`cinemaData`, a type-match) sees it,
// and is registered in `Source.all` (for `Source.priority` + Mongo-key lookup) —
// but deliberately NOT in `Cinema.all` / `byCity`, so the per-city/venue
// invariants hold and it never renders as a venue row. See `CinemaCityScraper` /
// `EnrichDetailsHandler`.
case object CinemaCityChain extends Cinema("Cinema City", "Cinema City")
// Network-level detail source for Cineworld UK, mirroring CinemaCityChain: the
// chain's 87 venues share one `detailGroup` ("cineworld"), so their deferred
// detail (synopsis/cast/director/certificate, fetched from the site's
// `movies?ids=` endpoint since the 2026-09-17 relaunch — see
// `services.cinemas.uk.CineworldClient`) lands in this ONE synthetic slot
// instead of an arbitrary venue's. Not in `Cinema.all`, so no scraper/coverage
// obligation; added to `Source.all` for priority + slot-key resolution.
case object CineworldChain extends Cinema("Cineworld", "Cineworld")
// Network-level detail source for Regal US, mirroring CineworldChain: the chain's
// ~400 venues share one `detailGroup` ("regal"), so the per-film detail their
// listing does NOT carry (runtime/rating/synopsis/cast/director, fetched from
// `/api/Movies?hoCode=`) lands in this ONE synthetic slot instead of ~400 copies.
// Not in `Cinema.all`, so no scraper/coverage obligation; added to `Source.all`
// for priority + slot-key resolution.
case object RegalChain extends Cinema("Regal", "Regal")


// ── United Kingdom (Flicks and the venues' own sites) — data-driven ─────────
// `UkRosterData` (generated from data/uk/roster.json by data/uk/scripts/generate_roster.py)
// declares every UK venue once, as a top-level `val` named as the `case object` it used to be
// (`ActOneActon`), so code and tests address a venue by name as before; which page lists it is
// `UkPages`, and what scrapes it `services.cinemas.uk.UkSourcesData`, both generated alongside.
// `name` is that Scala name, and stands in for the case object's `toString` and `hashCode`:
// `slug` (title rules, metric labels) is derived from it, and every hash-keyed collection of
// cinemas iterates as it did.
final class UkCinema(venueDisplayName: String, venuePillName: String, name: String, override val opening: Opening)
  extends Cinema(venueDisplayName, venuePillName) {
  def this(venueDisplayName: String, venuePillName: String, name: String) = this(venueDisplayName, venuePillName, name, Opening.YearRound)
  override def toString: String = name
  override def hashCode: Int = name.hashCode
}

// ── Germany (Filmstarts) — data-driven from the full roster ──────────────────
// `GermanRosterData` (generated from data/germany/pages.json + venues.json) carries
// 436 town and cluster pages / 1,515 cinemas. Each venue is a `GermanCinema` instance
// built ONCE in `GermanRoster` and reused everywhere (the page's cinemas,
// `Cinema.byCity`, and the scrape catalog) — so the same `Source` instance is used
// throughout and identity equality holds like the hand-authored `case object` cinemas did.
final class GermanCinema(venueDisplayName: String, venuePillName: String, override val opening: Opening = Opening.YearRound)
  extends Cinema(venueDisplayName, venuePillName)

// ── United States (Flicks) — data-driven from the full roster ────────────────
// `UsRosterData` (generated from data/us/pages.json) carries 1,995 town and
// cluster pages / 4,999 cinemas. Same shape as `GermanCinema`: each venue is
// built ONCE in `UsRoster` and reused everywhere, so one `Source` instance
// serves the page, `Cinema.byCity` and the scrape catalog, and identity
// equality holds like the hand-authored `case object` cinemas.
final class UsCinema(venueDisplayName: String, venuePillName: String, override val opening: Opening = Opening.YearRound)
  extends Cinema(venueDisplayName, venuePillName)

// ── Spain (SensaCine) — data-driven from the full roster ─────────────────────
// `SpanishRosterData` (generated from data/spain/pages.json + provinces.json + ocine.json)
// carries 240 town and cluster pages / 599 cinemas. Same shape as `GermanCinema` and
// `UsCinema`: each venue is built ONCE in `SpanishRoster` and reused everywhere, so one
// `Source` instance serves the page, `Cinema.byCity` and the scrape catalog, and
// identity equality holds like the hand-authored `case object` cinemas.
final class SpanishCinema(venueDisplayName: String, venuePillName: String, override val opening: Opening = Opening.YearRound)
  extends Cinema(venueDisplayName, venuePillName)

object Cinema {
  /** The display names a generated roster may NOT reuse: every Polish and UK venue's
   *  ([[polishAndUk]]), every network pseudo-cinema's, and every venue of the rosters
   *  built before it (`earlier`) — see `GermanRoster.claimedElsewhere`. */
  private[models] def displayNamesClaimedBy(earlier: Seq[(String, Seq[Cinema])]*): Set[String] =
    ((polishAndUk +: earlier).flatMap(_.flatMap(_._2)) ++ CinemaNetworks.all).map(_.displayName).toSet

  /** The chains' network pseudo-cinemas — [[CinemaNetworks.all]], under the name `services.identity` reads. */
  val Networks: Seq[Cinema] = CinemaNetworks.all

  /** The 41 major Polish cities, by label, each the slug its venues are filed under in
   *  [[PolishPages.majorCinemas]] — in page order, which doubles as the per-source merge priority
   *  (`Source.all`): Poznań's venues, the original ten, keep their resolved precedence. A label here
   *  is the city's `City` object's nominative. */
  private val majorPolishCities: Seq[(String, String)] = Seq(
    "Poznań"              -> "poznan",
    "Wrocław"             -> "wroclaw",
    "Warszawa"            -> "warszawa",
    "Kraków"              -> "krakow",
    "Łódź"                -> "lodz",
    "Katowice"            -> "katowice",
    "Szczecin"            -> "szczecin",
    "Białystok"           -> "bialystok",
    "Trójmiasto"          -> "trojmiasto",
    "Bydgoszcz"           -> "bydgoszcz",
    "Lublin"              -> "lublin",
    "Częstochowa"         -> "czestochowa",
    "Radom"               -> "radom",
    "Sosnowiec"           -> "sosnowiec",
    "Toruń"               -> "torun",
    "Kielce"              -> "kielce",
    "Rzeszów"             -> "rzeszow",
    "Gliwice"             -> "gliwice",
    "Zabrze"              -> "zabrze",
    "Olsztyn"             -> "olsztyn",
    "Bielsko-Biała"       -> "bielsko-biala",
    "Opole"               -> "opole",
    "Rybnik"              -> "rybnik",
    "Gorzów Wielkopolski" -> "gorzow-wielkopolski",
    "Elbląg"              -> "elblag",
    "Koszalin"            -> "koszalin",
    "Kalisz"              -> "kalisz",
    "Zielona Góra"        -> "zielona-gora",
    "Tychy"               -> "tychy",
    "Wałbrzych"           -> "walbrzych",
    "Tarnów"              -> "tarnow",
    "Włocławek"           -> "wloclawek",
    "Legnica"             -> "legnica",
    "Płock"               -> "plock",
    "Bytom"               -> "bytom",
    "Dąbrowa Górnicza"    -> "dabrowa-gornicza",
    "Nowy Sącz"           -> "nowy-sacz",
    "Słupsk"              -> "slupsk",
    "Jelenia Góra"        -> "jelenia-gora",
    "Przemyśl"            -> "przemysl",
    "Konin"               -> "konin",
  )

  /** Poland's and the UK's venues — named before any generated roster's. Split out from [[byCity]] so
   *  `GermanRoster` can see which display names are already claimed before it materialises
   *  the generated roster; see `GermanRoster.claimedElsewhere`. Declared BEFORE `byCity`
   *  so it is assigned by the time that field forces the roster. */
  val polishAndUk: Seq[(String, Seq[Cinema])] =
    majorPolishCities.map { case (label, slug) => label -> PolishPages.majorCinemas(slug) } ++
    PolishPages.rows.map(r => r.nominative -> r.cinemas) ++
    (("London" -> UkPages.london.cinemas) +: UkPages.rows.map(r => ukLabel(r) -> r.cinemas))

  /** A UK page's label here, unique as [[byCity]]'s must be: the page's name,
   *  with its county where two pages share one — the two Bangors are both an area. */
  private def ukLabel(r: UkPages.Row): String =
    if (UkPages.rows.count(_.nominative == r.nominative) > 1) s"${r.nominative} (${r.county})" else r.nominative

  /** Every city's venues in page order, paired with the city's display label.
   *  Single source of truth for `all` and for the uptime page's per-city
   *  grouping — add a city here and both pick it up.
   *
   *  LAZY, along with everything derived from it, and that is load-bearing rather than a
   *  micro-optimisation. `GermanRoster` reads `Cinema.polishAndUk` while building itself,
   *  and `UsRoster` reads `polishAndUk` AND `GermanRoster.byCity`, so `Cinema` and both
   *  rosters initialise each other. Forcing the German roster from
   *  a strict field here made the outcome depend on which object the JVM happened to touch
   *  first: enter through `Cinema` and `polishAndUk` is assigned in time, enter through
   *  `GermanRoster` and it reads a null, dying with `ExceptionInInitializerError`. That is
   *  why `CinemaScraperCatalogSpec` could not be run on its own — it passed only inside a
   *  full suite where some earlier spec happened to touch `Cinema` first. Deferring the
   *  roster means entering through `GermanRoster` completes `Cinema`'s own fields without
   *  re-entering the roster, so either entry point works. */
  lazy val byCity: Seq[(String, Seq[Cinema])] =
    polishAndUk ++
      GermanRoster.byCity ++  // Germany: by Bundesland, the level above its 436 town pages (data-driven)
      UsRoster.byCity ++      // USA: 55 states/territories, the level its venues are grouped at
      SpanishRoster.byCity   // Spain: by province, the level above its 240 town pages (data-driven)

  lazy val all: Seq[Cinema] = byCity.flatMap(_._2)

  lazy val pillMap: Map[String, String] = all.map(c => c.displayName -> c.pillName).toMap

  /** Resolve a cinema from the `displayName` it is stored under — the wire key
   *  every per-cinema row uses (`movie_slots`, `screenings`, `cinema_scrapes`).
   *  Unambiguous by construction: `GermanRoster` qualifies any generated venue
   *  whose name would collide with a Polish or UK one, and `SourceWireKeySpec`
   *  fails the build on a duplicate. */
  lazy val byDisplayName: Map[String, Cinema] = all.map(c => c.displayName -> c).toMap

  /** City display label for each cinema — derived from `byCity`, the single
   *  source of truth. Used by the debug source-data view to disambiguate
   *  same-named chains (e.g. the many "Helios" venues across cities). */
  lazy val cityByCinema: Map[Cinema, String] =
    byCity.flatMap { case (city, cinemas) => cinemas.map(_ -> city) }.toMap

  def cityOf(cinema: Cinema): Option[String] = cityByCinema.get(cinema)
}
