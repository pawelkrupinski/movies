package models

sealed abstract class Cinema(val displayName: String, val pillName: String) extends Source {
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

case object CinemaCityJanki extends Cinema("Cinema City Janki", "Janki")

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
case object KinoNadWarta        extends Cinema("Kino nad Wartą", "nad Wartą")        // Koło — filmweb
case object KinoHel             extends Cinema("Kino Hel", "Hel")                   // Pleszew — filmweb
case object KinoSokolnia        extends Cinema("Kino Sokolnia", "Sokolnia")          // Słupca — filmweb
case object KinoTur             extends Cinema("Kino Tur", "Tur")                   // Turek — filmweb
case object KinoMok             extends Cinema("Kino MOK", "MOK")                   // Zagórów — filmweb

// ── Filmweb catchment cinemas (nearby towns Filmweb groups under each city) ──
// Each is served by FilmwebShowtimesClient via its Filmweb internal cinema id
// (verified non-empty seances 2026-06). Wired in CinemaScraperCatalog.filmwebExtra.
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
case object KinoPlanetCinema extends Cinema("Planet Cinema", "Planet Cinema")   // Oświęcim — filmweb 1528
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
case object KinoZaRogiem extends Cinema("Kino za Rogiem", "za Rogiem")   // Płock — filmweb 1949
// legnica
case object HeliosLubin extends Cinema("Helios Lubin", "Lubin")   // Lubin — filmweb 1420
case object KinoAurum extends Cinema("Kino Aurum", "Aurum")   // Złotoryja — filmweb 1718
case object KinoCyfroweKino extends Cinema("Cyfrowe Kino", "Cyfrowe Kino")   // Środa Śląska — filmweb 2313
case object KinoForumBoleslawiec extends Cinema("Forum", "Forum")   // Bolesławiec — filmweb 373
case object KinoMuzaLubin extends Cinema("Muza", "Muza")   // Lubin — filmweb 288
case object KinoPCA extends Cinema("Kino PCA", "PCA")   // Polkowice — filmweb 1139
// plock
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
case object KinoCentrum3DPrzemysl extends Cinema("Centrum 3D Przemyśl", "Centrum 3D")   // Przemyśl — filmweb 1786
case object KinoIkar extends Cinema("Kino Ikar", "Ikar")   // Jarosław — filmweb 1707
case object KinoNaBiegunach extends Cinema("Kino Na Biegunach", "Na Biegunach")   // Jarosław — filmweb 2172
case object KinoSDK extends Cinema("Kino SDK", "SDK")   // Sanok — bilety.sdksanok.pl

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


// ── United Kingdom (Flicks-sourced) ──
// ── United Kingdom · London (Flicks) ──
case object ActOneActon extends Cinema("Act One Cinema Acton", "Act One Acton")
case object ArthouseCrouchEnd extends Cinema("ArtHouse Crouch End", "ArtHouse Crouch End")
case object BarbicanLondonCinema1 extends Cinema("Barbican London", "Barbican London")
case object BfiLondonImax extends Cinema("BFI London IMAX", "BFI London IMAX")
case object BfiLondonSouthbank extends Cinema("BFI London Southbank", "BFI London Southbank")
case object CastleCinemaHackney extends Cinema("Castle Cinema Hackney", "Castle Hackney")
case object SidcupStoryteller extends Cinema("Castle Sidcup", "Castle Sidcup")
case object ChiswickCinema extends Cinema("Chiswick Cinema", "Chiswick")
case object CineworldGreenwich extends Cinema("Cineworld at The O2 Greenwich", "Cineworld at The O2 Greenwich")
case object CineworldBexleyheath extends Cinema("Cineworld Bexleyheath", "Cineworld Bexleyheath")
case object CineworldEnfield extends Cinema("Cineworld Enfield", "Cineworld Enfield")
case object CineworldFeltham extends Cinema("Cineworld Feltham", "Cineworld Feltham")
case object CineworldIlford extends Cinema("Cineworld Ilford", "Cineworld Ilford")
case object CineworldLeicesterSquare extends Cinema("Cineworld Leicester Square", "Cineworld Leicester Square")
case object CineworldLondonHounslow extends Cinema("Cineworld London Hounslow", "Cineworld London Hounslow")
case object CineworldSouthRuislip extends Cinema("Cineworld South Ruislip", "Cineworld South Ruislip")
case object CineworldWandsworth extends Cinema("Cineworld Wandsworth", "Cineworld Wandsworth")
case object CineworldWembley extends Cinema("Cineworld Wembley", "Cineworld Wembley")
case object CineworldWestIndiaQuay extends Cinema("Cineworld West India Quay", "Cineworld West India Quay")
case object CineworldWoodGreen extends Cinema("Cineworld Wood Green", "Cineworld Wood Green")
case object CineLumiereLondon extends Cinema("Ciné Lumière London", "Ciné Lumière London")
case object CloseUpFilmCentreShoreditch extends Cinema("Close-Up Film Centre Shoreditch", "Close-Up Film Centre Shoreditch")
case object CrouchEndPicturehouse extends Cinema("Crouch End Picturehouse", "Crouch End Picturehouse")
case object CurzonCinemaAldgate extends Cinema("Curzon Cinema Aldgate", "Curzon Aldgate")
case object CurzonCinemaBloomsbury extends Cinema("Curzon Cinema Bloomsbury", "Curzon Bloomsbury")
case object CurzonCinemaCamden extends Cinema("Curzon Cinema Camden", "Curzon Camden")
case object CurzonCinemaHoxton extends Cinema("Curzon Cinema Hoxton", "Curzon Hoxton")
case object CurzonCinemaKingston extends Cinema("Curzon Cinema Kingston", "Curzon Kingston")
case object CurzonCinemaMayfair extends Cinema("Curzon Cinema Mayfair", "Curzon Mayfair")
case object CurzonCinemaRichmond extends Cinema("Curzon Cinema Richmond", "Curzon Richmond")
case object CurzonCinemaSeaContainersMondrian extends Cinema("Curzon Cinema Sea Containers (Mondrian)", "Curzon Sea Containers")
case object CurzonCinemaVictoria extends Cinema("Curzon Cinema Victoria", "Curzon Victoria")
case object CurzonSoho extends Cinema("Curzon Soho", "Curzon Soho")
case object CurzonWimbledon extends Cinema("Curzon Wimbledon", "Curzon Wimbledon")
case object DavidLeanCinemaCroydon extends Cinema("David Lean Cinema Croydon", "David Lean Croydon")
case object ElectricCinemaLondon extends Cinema("Electric Cinema Portobello", "Electric Portobello")
case object ElectricCinemaWhiteCity extends Cinema("Electric Cinema White City", "Electric White City")
case object EverymanAtTheWhiteleyLondon extends Cinema("Everyman at The Whiteley London", "Everyman at The Whiteley London")
case object EverymanBrentford extends Cinema("Everyman Brentford", "Everyman Brentford")
case object EverymanCinemaBakerStreet extends Cinema("Everyman Cinema Baker Street", "Everyman Baker Street")
case object EverymanCinemaBarnet extends Cinema("Everyman Cinema Barnet", "Everyman Barnet")
case object EverymanCinemaBelsizeParkHampstead extends Cinema("Everyman Cinema Belsize Park Hampstead", "Everyman Belsize Park Hampstead")
case object EverymanCinemaBoroughYards extends Cinema("Everyman Cinema Borough Yards", "Everyman Borough Yards")
case object EverymanCinemaBroadgate extends Cinema("Everyman Cinema Broadgate", "Everyman Broadgate")
case object EverymanCinemaCanaryWharf extends Cinema("Everyman Cinema Canary Wharf", "Everyman Canary Wharf")
case object EverymanCinemaChelsea extends Cinema("Everyman Cinema Chelsea", "Everyman Chelsea")
case object EverymanCinemaCrystalPalace extends Cinema("Everyman Cinema Crystal Palace", "Everyman Crystal Palace")
case object EverymanCinemaEgham extends Cinema("Everyman Cinema Egham", "Everyman Egham")
case object EverymanCinemaEsher extends Cinema("Everyman Cinema Esher", "Everyman Esher")
case object EverymanCinemaHampstead extends Cinema("Everyman Cinema Hampstead", "Everyman Hampstead")
case object EverymanCinemaKingSCross extends Cinema("Everyman Cinema King's Cross", "Everyman King's Cross")
case object EverymanCinemaMaidaVale extends Cinema("Everyman Cinema Maida Vale", "Everyman Maida Vale")
case object EverymanCinemaMuswellHill extends Cinema("Everyman Cinema Muswell Hill", "Everyman Muswell Hill")
case object EverymanCinemaStratfordInternational extends Cinema("Everyman Cinema Stratford International", "Everyman Stratford International")
case object EverymanCinemaWaltonOnThames extends Cinema("Everyman Cinema Walton", "Everyman Walton")
case object EverymanCinemaIslington extends Cinema("Everyman Screen on the Green Islington", "Everyman Screen on the Green Islington")
case object FinsburyParkPicturehouse extends Cinema("Finsbury Park Picturehouse", "Finsbury Park Picturehouse")
case object ForestCinemasWalthamstow extends Cinema("Forest Cinemas Walthamstow", "Forest Walthamstow")
case object GenesisTowerHamlets extends Cinema("Genesis Cinema Whitechapel", "Genesis Whitechapel")
case object InstituteOfContemporaryArts extends Cinema("Institute of Contemporary Arts", "Institute of Contemporary Arts")
case object Jw3Hampstead extends Cinema("JW3 Hampstead", "JW3 Hampstead")
case object KilnKilburn extends Cinema("Kiln Cinema Kilburn", "Kiln Kilburn")
case object LeatherheadTheatreCinemaLeatherhead extends Cinema("Leatherhead Cinema", "Leatherhead")
case object LexiKensalRise extends Cinema("Lexi Cinema Kensal Rise", "Lexi Kensal Rise")
case object LumiereRomford extends Cinema("Lumiere Romford", "Lumiere Romford")
case object NovaCinemaWoking extends Cinema("Nova Cinema Woking", "Nova Woking")
case object OdeonCinemaActon extends Cinema("Odeon Cinema Acton", "Odeon Acton")
case object OdeonCinemaBeckenham extends Cinema("Odeon Cinema Beckenham", "Odeon Beckenham")
case object OdeonCinemaEpsom extends Cinema("Odeon Cinema Epsom", "Odeon Epsom")
case object OdeonCinemaGreenwich extends Cinema("Odeon Cinema Greenwich", "Odeon Greenwich")
case object OdeonCinemaHolloway extends Cinema("Odeon Cinema Holloway", "Odeon Holloway")
case object OdeonCinemaKingston extends Cinema("Odeon Cinema Kingston", "Odeon Kingston")
case object OdeonCinemaOrpington extends Cinema("Odeon Cinema Orpington", "Odeon Orpington")
case object OdeonCinemaRichmond extends Cinema("Odeon Cinema Richmond", "Odeon Richmond")
case object OdeonCinemaSouthWoodford extends Cinema("Odeon Cinema South Woodford", "Odeon South Woodford")
case object OdeonCinemaStreatham extends Cinema("Odeon Cinema Streatham", "Odeon Streatham")
case object OdeonCinemaTottenhamCourtRoad extends Cinema("Odeon Cinema Tottenham Court Road", "Odeon Tottenham Court Road")
case object OdeonCinemaUxbridge extends Cinema("Odeon Cinema Uxbridge", "Odeon Uxbridge")
case object OdeonCinemaWimbledon extends Cinema("Odeon Cinema Wimbledon", "Odeon Wimbledon")
case object OdeonCinemaLuxeHaymarket extends Cinema("Odeon Luxe Haymarket", "Odeon Luxe Haymarket")
case object OdeonLuxeIslington extends Cinema("Odeon Luxe Islington", "Odeon Luxe Islington")
case object OdeonLuxeLeeValley extends Cinema("Odeon Luxe Lee Valley", "Odeon Luxe Lee Valley")
case object OdeonCinemaLuxeLeicesterSquare extends Cinema("Odeon Luxe Leicester Square", "Odeon Luxe Leicester Square")
case object OdeonCinemaLuxePutney extends Cinema("Odeon Luxe Putney", "Odeon Luxe Putney")
case object OdeonLuxeSwissCottage extends Cinema("Odeon Luxe Swiss Cottage", "Odeon Luxe Swiss Cottage")
case object OdeonLuxeWestEnd extends Cinema("Odeon Luxe West End", "Odeon Luxe West End")
case object OlympicCinemaBarnes extends Cinema("Olympic Cinema Barnes", "Olympic Barnes")
case object EmpireCinemaSutton extends Cinema("Omniplex Sutton (formerly Empire)", "Omniplex Sutton")
case object Peckhamplex extends Cinema("Peckhamplex", "Peckhamplex")
case object PhoenixCinemaEastFinchley extends Cinema("Phoenix Cinema East Finchley", "Phoenix East Finchley")
case object PicturehouseCentralLondon extends Cinema("Picturehouse Central London", "Picturehouse Central London")
case object PicturehouseClapham extends Cinema("Picturehouse Clapham", "Picturehouse Clapham")
case object PicturehouseEalingFilmworks extends Cinema("Picturehouse Ealing Filmworks", "Picturehouse Ealing Filmworks")
case object PicturehouseEastDulwich extends Cinema("Picturehouse East Dulwich", "Picturehouse East Dulwich")
case object PicturehouseEpsomSquare extends Cinema("Picturehouse Epsom", "Picturehouse Epsom")
case object PicturehouseGreenwich extends Cinema("Picturehouse Greenwich", "Picturehouse Greenwich")
case object PicturehouseHackney extends Cinema("Picturehouse Hackney", "Picturehouse Hackney")
case object PicturehouseWestNorwood extends Cinema("Picturehouse West Norwood", "Picturehouse West Norwood")
case object PrinceCharlesLondon extends Cinema("Prince Charles London", "Prince Charles London")
case object RegentStreetCinemaLondon extends Cinema("Regent Street Cinema London", "Regent Street London")
case object RichMixBethnalGreen extends Cinema("Rich Mix Shoreditch", "Rich Mix Shoreditch")
case object RioDalston extends Cinema("Rio Cinema Dalston", "Rio Dalston")
case object RiversideStudiosHammersmith extends Cinema("Riverside Studios Hammersmith", "Riverside Studios Hammersmith")
case object RooftopFilmClubPeckhamBusseyBuilding extends Cinema("Rooftop Film Club Peckham (Bussey Building)", "Rooftop Film Club Peckham")
case object RooftopFilmClubStratfordRoofEast extends Cinema("Rooftop Film Club Stratford (Roof East)", "Rooftop Film Club Stratford")
case object ScienceMuseumLondonImax extends Cinema("Science Museum London IMAX", "Science Museum London IMAX")
case object ArchlightCinemas extends Cinema("The Arches (The Cinema in the Power Station)", "The Arches")
case object TheArzner extends Cinema("The Arzner", "The Arzner")
case object TheCinemaAtSelfridges extends Cinema("The Cinema at Selfridges", "The at Selfridges")
case object TheCinemaInThePowerStation extends Cinema("The Cinema in the Power Station", "The in the Power Station")
case object TheGardenCinema extends Cinema("The Garden Cinema", "The Garden")
case object TheGatePicturehouseLondon extends Cinema("The Gate, Picturehouse London", "The Gate, Picturehouse London")
case object TheLightCinemasAddlestone extends Cinema("The Light Addlestone", "The Light Addlestone")
case object TheNickelLondon extends Cinema("The Nickel London", "The Nickel London")
case object TheRitzyPicturehouseBrixton extends Cinema("The Ritzy Picturehouse Brixton", "The Ritzy Picturehouse Brixton")
case object VueCinemasBromley extends Cinema("Vue Cinemas Bromley", "Vue Bromley")
case object VueCinemasDagenham extends Cinema("Vue Cinemas Dagenham", "Vue Dagenham")
case object VueCinemasEltham extends Cinema("Vue Cinemas Eltham", "Vue Eltham")
case object VueCinemasFinchleyRoadSwissCottage extends Cinema("Vue Cinemas Finchley Road", "Vue Finchley Road")
case object VueCinemasFulham extends Cinema("Vue Cinemas Fulham", "Vue Fulham")
case object VueCinemasHarrow extends Cinema("Vue Cinemas Harrow", "Vue Harrow")
case object VueCinemasIslington extends Cinema("Vue Cinemas Islington", "Vue Islington")
case object VueCinemasFinchley extends Cinema("Vue Cinemas North Finchley", "Vue North Finchley")
case object VueCinemasPiccadillyCircus extends Cinema("Vue Cinemas Piccadilly", "Vue Piccadilly")
case object VueCinemasPurleyWayCroydon extends Cinema("Vue Cinemas Purley Way Croydon", "Vue Purley Way Croydon")
case object VueCinemasRomford extends Cinema("Vue Cinemas Romford", "Vue Romford")
case object VueCinemasStainesUponThames extends Cinema("Vue Cinemas Staines", "Vue Staines")
case object VueCinemasStratford extends Cinema("Vue Cinemas Stratford", "Vue Stratford")
case object VueCinemasWestEnd extends Cinema("Vue Cinemas West End (Leicester Square)", "Vue West End")
case object VueCinemasWestfieldShepherdSBush extends Cinema("Vue Cinemas Westfield Shepherd's Bush", "Vue Westfield Shepherd's Bush")
case object VueCinemasWoodGreen extends Cinema("Vue Cinemas Wood Green", "Vue Wood Green")
case object WatermansArtCentreBrentford extends Cinema("Watermans Art Centre Brentford", "Watermans Art Centre Brentford")
case object WyllyottsTheatrePottersBar extends Cinema("Wyllyotts Theatre Potters Bar", "Wyllyotts Theatre Potters Bar")
// ── United Kingdom · Manchester (Flicks) ──
case object CineworldAshtonUnderLyne extends Cinema("Cineworld Ashton-under-Lyne", "Cineworld Ashton-under-Lyne")
case object CineworldManchester extends Cinema("Cineworld Didsbury", "Cineworld Didsbury")
case object CultplexManchester extends Cinema("Cultplex Manchester", "Cultplex Manchester")
case object EverymanManchesterStJohns extends Cinema("Everyman Cinema Manchester St John's", "Everyman Manchester St John's")
case object FlixTreehouseManchester extends Cinema("Flix Treehouse Manchester", "Flix Treehouse Manchester")
case object HomeManchester extends Cinema("HOME Manchester", "HOME Manchester")
case object LeighFilmFactory extends Cinema("Leigh Film Factory", "Leigh Film Factory")
case object NorthernLightSale extends Cinema("Northern Light Sale", "Northern Light Sale")
case object OdeonCinemaManchesterGreatNorthern extends Cinema("Odeon Cinema Manchester Great Northern", "Odeon Manchester Great Northern")
case object OdeonCinemaManchesterTraffordCentre extends Cinema("Odeon Cinema Manchester Trafford Centre", "Odeon Manchester Trafford Centre")
case object OdeonCinemaOldham extends Cinema("Odeon Cinema Oldham", "Odeon Oldham")
case object EmpireCinemaWigan extends Cinema("Omniplex Wigan (formerly Empire)", "Omniplex Wigan")
case object PlazaStockport extends Cinema("Plaza Stockport", "Plaza Stockport")
case object ReelCinemaRochdale extends Cinema("Reel Cinema Rochdale", "Reel Rochdale")
case object RegentMarple extends Cinema("Regent Cinema Marple", "Regent Marple")
case object SavoyHeatonMoor extends Cinema("Savoy Cinema Heaton Moor", "Savoy Heaton Moor")
case object TheLightCinemasStockport extends Cinema("The Light Stockport", "The Light Stockport")
case object VueCinemasManchesterPrintworks extends Cinema("Vue Cinemas Manchester Printworks", "Vue Manchester Printworks")
case object VueCinemasManchesterQuayside extends Cinema("Vue Cinemas Manchester Quayside", "Vue Manchester Quayside")
// ── United Kingdom · Norwich (Flicks) ──
case object ArcCinemaGreatYarmouth extends Cinema("Arc Cinema Great Yarmouth", "Arc Great Yarmouth")
case object CentralCinemaFakenham extends Cinema("Central Cinema Fakenham", "Central Fakenham")
case object CornExchangeCinemaKingSLynn extends Cinema("Corn Exchange Cinema King's Lynn", "Corn Exchange King's Lynn")
case object EastCoastCinemaLowestoft extends Cinema("East Coast Cinema Lowestoft", "East Coast Lowestoft")
case object LittleTheatreSheringham extends Cinema("Little Theatre Sheringham", "Little Theatre Sheringham")
case object MajesticKingSLynn extends Cinema("Majestic King's Lynn", "Majestic King's Lynn")
case object MarinaTheatreLowestoft extends Cinema("Marina Theatre Lowestoft", "Marina Theatre Lowestoft")
case object OdeonNorwich extends Cinema("Odeon Cinema Norwich", "Odeon Norwich")
case object OrionDereham extends Cinema("Orion Dereham", "Orion Dereham")
case object PalaceCinemaGorlestonOnSea extends Cinema("Palace Cinema Gorleston-on-Sea", "Palace Gorleston-on-Sea")
case object CinemaCityPicturehouseNorwich extends Cinema("Picturehouse Cinema City Norwich", "Picturehouse City Norwich")
case object RegalMovieplexCromer extends Cinema("Regal Movieplex Cromer", "Regal Movieplex Cromer")
case object TheLightThetford extends Cinema("The Light Thetford", "The Light Thetford")
case object VueCinemasNorwich extends Cinema("Vue Cinemas Norwich", "Vue Norwich")
// ── United Kingdom · Aberdeenshire (Flicks) ──
case object ArcCinemaPeterhead extends Cinema("Arc Cinema Peterhead", "Arc Peterhead")
case object BelmontFilmhouse extends Cinema("Belmont Filmhouse", "Belmont Filmhouse")
case object CineworldQueensLinkAberdeen extends Cinema("Cineworld (Queens Link) Aberdeen", "Cineworld (Queens Link) Aberdeen")
case object CineworldUnionSquareAberdeen extends Cinema("Cineworld (Union Square) Aberdeen", "Cineworld (Union Square) Aberdeen")
case object MorayPlayhouse extends Cinema("Moray Playhouse", "Moray Playhouse")
case object Number30Huntly extends Cinema("Number 30 Huntly", "Number 30 Huntly")
case object TheBarnBanchory extends Cinema("The Barn Banchory", "The Barn Banchory")
case object VictoriaHallEllon extends Cinema("Victoria Hall Ellon", "Victoria Hall Ellon")
// ── United Kingdom · Antrim (Flicks) ──
case object IMCCinemaBallymena extends Cinema("IMC Cinema Ballymena", "IMC Ballymena")
case object MovieHouseGlengormley extends Cinema("Movie House Glengormley", "Movie House Glengormley")
case object OmniplexAntrim extends Cinema("Omniplex Antrim", "Omniplex Antrim")
case object OmniplexCarrickfergus extends Cinema("Omniplex Carrickfergus", "Omniplex Carrickfergus")
case object OmniplexLarne extends Cinema("Omniplex Larne", "Omniplex Larne")
// ── United Kingdom · Armagh (Flicks) ──
case object OmniplexCraigavon extends Cinema("Omniplex Craigavon", "Omniplex Craigavon")
// ── United Kingdom · Ayrshire and Arran (Flicks) ──
case object AstoriaCinemaAyr extends Cinema("Astoria Cinema Ayr", "Astoria Ayr")
case object CinemaSaltcoatsPremierLeisure extends Cinema("Cinema Saltcoats (Premier Leisure)", "Saltcoats (Premier Leisure)")
case object OdeonCinemaKilmarnock extends Cinema("Odeon Cinema Kilmarnock", "Odeon Kilmarnock")
// ── United Kingdom · Bedfordshire (Flicks) ──
case object CineworldLuton extends Cinema("Cineworld Luton", "Cineworld Luton")
case object VueCinemasBedford extends Cinema("Vue Cinemas Bedford", "Vue Bedford")
// ── United Kingdom · Belfast (Flicks) ──
case object CineworldBelfast extends Cinema("Cineworld Belfast", "Cineworld Belfast")
case object MovieHouseCitySideBelfast extends Cinema("Movie House (City Side) Belfast", "Movie House (City Side) Belfast")
case object OdeonCinemaBelfast extends Cinema("Odeon Cinema Belfast", "Odeon Belfast")
case object OmniplexBelfast extends Cinema("Omniplex Belfast", "Omniplex Belfast")
case object OmniplexLisburn extends Cinema("Omniplex Lisburn", "Omniplex Lisburn")
case object QueenSFilmTheatreBelfast extends Cinema("Queen's Film Theatre Belfast", "Queen's Film Theatre Belfast")
case object StrandArtsCentreBelfast extends Cinema("Strand Arts Centre Belfast", "Strand Arts Centre Belfast")
case object TheAvenueCinemaBelfast extends Cinema("The Avenue Cinema Belfast", "The Avenue Belfast")
// ── United Kingdom · Berkshire (Flicks) ──
case object CineworldBracknell extends Cinema("Cineworld Bracknell", "Cineworld Bracknell")
case object CornExchangeNewburyScreenOne extends Cinema("Corn Exchange Newbury (Screen One)", "Corn Exchange Newbury (Screen One)")
case object EverymanCinemaWokingham extends Cinema("Everyman Cinema Wokingham", "Everyman Wokingham")
case object OdeonLuxeMaidenhead extends Cinema("Odeon Luxe Maidenhead", "Odeon Luxe Maidenhead")
case object ReadingBiscuitFactory extends Cinema("Reading Biscuit Factory", "Reading Biscuit Factory")
case object ShowcaseDeLuxReading extends Cinema("Showcase de Lux Reading", "Showcase de Lux Reading")
case object SouthHillParkArtsCentreBracknell extends Cinema("South Hill Park Arts Centre Bracknell", "South Hill Park Arts Centre Bracknell")
case object TheAssemblyAtHeckfieldPlace extends Cinema("The Assembly at Heckfield Place", "The Assembly at Heckfield Place")
case object TheOldCourtWindsor extends Cinema("The Old Court Windsor", "The Old Court Windsor")
case object VueCinemasNewbury extends Cinema("Vue Cinemas Newbury", "Vue Newbury")
case object VueCinemasReading extends Cinema("Vue Cinemas Reading", "Vue Reading")
// ── United Kingdom · Birmingham (Flicks) ──
case object ArtrixBromsgrove extends Cinema("Artrix Bromsgrove", "Artrix Bromsgrove")
case object CineworldBroadStreetBirmingham extends Cinema("Cineworld Broad Street, Birmingham", "Cineworld Broad Street, Birmingham")
case object CineworldNECBirmingham extends Cinema("Cineworld NEC Birmingham", "Cineworld NEC Birmingham")
case object CineworldSolihull extends Cinema("Cineworld Solihull", "Cineworld Solihull")
case object EverymanCinemaBirmingham extends Cinema("Everyman Cinema Birmingham", "Everyman Birmingham")
case object MidlandsArtsCentreBirmingham extends Cinema("Midlands Arts Centre Birmingham", "Midlands Arts Centre Birmingham")
case object MockingbirdCinemaKitchenBirmingham extends Cinema("Mockingbird Cinema & Kitchen Birmingham", "Mockingbird & Kitchen Birmingham")
case object OdeonBirminghamNewStreet extends Cinema("Odeon Birmingham New Street", "Odeon Birmingham New Street")
case object OdeonLuxeBirminghamBroadwayPlaza extends Cinema("Odeon Luxe Birmingham Broadway Plaza", "Odeon Luxe Birmingham Broadway Plaza")
case object OmniplexBirmingham extends Cinema("Omniplex Birmingham", "Omniplex Birmingham")
case object ReelCinemaQuinton extends Cinema("Reel Cinema Quinton", "Reel Quinton")
case object RoyalCinemasSuttonColdfield extends Cinema("Royal Cinemas Sutton Coldfield", "Royal Sutton Coldfield")
case object VueCinemasBirmingham extends Cinema("Vue Cinemas Birmingham", "Vue Birmingham")
// ── United Kingdom · Bristol (Flicks) ──
case object CubeCinemaBristol extends Cinema("Cube Cinema Bristol", "Cube Bristol")
case object EverymanCinemaBristol extends Cinema("Everyman Cinema Bristol", "Everyman Bristol")
case object OdeonCabotCircus extends Cinema("Odeon Cabot Circus", "Odeon Cabot Circus")
case object ScottCinemasBristolWestburyPark extends Cinema("Scott Cinemas Bristol (Westbury Park)", "Scott Bristol (Westbury Park)")
case object ShowcaseBristolAvonmeads extends Cinema("Showcase Bristol Avonmeads", "Showcase Bristol Avonmeads")
case object VueCinemasBristolCribbsCauseway extends Cinema("Vue Cinemas Bristol Cribbs Causeway", "Vue Bristol Cribbs Causeway")
case object VueCinemasBristolLongwellGreen extends Cinema("Vue Cinemas Bristol Longwell Green", "Vue Bristol Longwell Green")
case object WatershedBristol extends Cinema("Watershed Bristol", "Watershed Bristol")
// ── United Kingdom · Buckinghamshire (Flicks) ──
case object CineworldHighWycombe extends Cinema("Cineworld High Wycombe", "Cineworld High Wycombe")
case object CineworldMiltonKeynes extends Cinema("Cineworld Milton Keynes", "Cineworld Milton Keynes")
case object EverymanCinemaGerrardsCross extends Cinema("Everyman Cinema Gerrards Cross", "Everyman Gerrards Cross")
case object EverymanCinemaMarlow extends Cinema("Everyman Cinema Marlow", "Everyman Marlow")
case object OdeonCinemaAylesbury extends Cinema("Odeon Cinema Aylesbury", "Odeon Aylesbury")
case object OdeonCinemaMiltonKeynes extends Cinema("Odeon Cinema Milton Keynes", "Odeon Milton Keynes")
case object OmniplexHighWycombeFormerlyEmpire extends Cinema("Omniplex High Wycombe (formerly Empire)", "Omniplex High Wycombe (formerly Empire)")
case object VillagePictureHouseCuddington extends Cinema("Village Picture House Cuddington", "Village Picture House Cuddington")
// ── United Kingdom · Cambridgeshire (Flicks) ──
case object ArtsCinemaJohnClareTheatrePeterborough extends Cinema("Arts Cinema (John Clare Theatre) Peterborough", "Arts (John Clare Theatre) Peterborough")
case object ArtsPicturehouseCambridge extends Cinema("Arts Picturehouse Cambridge", "Arts Picturehouse Cambridge")
case object CineworldEly extends Cinema("Cineworld Ely", "Cineworld Ely")
case object CineworldHuntingdon extends Cinema("Cineworld Huntingdon", "Cineworld Huntingdon")
case object CineworldStNeots extends Cinema("Cineworld St Neots", "Cineworld St Neots")
case object ElyCommunityCinema extends Cinema("Ely Community Cinema", "Ely Community")
case object EverymanCinemaCambridge extends Cinema("Everyman Cinema Cambridge", "Everyman Cambridge")
case object KeyTheatrePeterborough extends Cinema("Key Theatre Peterborough", "Key Theatre Peterborough")
case object LuxeWisbech extends Cinema("Luxe Wisbech", "Luxe Wisbech")
case object OdeonLuxePeterborough extends Cinema("Odeon Luxe Peterborough", "Odeon Luxe Peterborough")
case object ShowcaseDeLuxPeterborough extends Cinema("Showcase de Lux Peterborough", "Showcase de Lux Peterborough")
case object TheLightCambridge extends Cinema("The Light Cambridge", "The Light Cambridge")
case object TheLightWisbech extends Cinema("The Light Wisbech", "The Light Wisbech")
// ── United Kingdom · Cardiff (Flicks) ──
case object ChapterCardiff extends Cinema("Chapter Cardiff", "Chapter Cardiff")
case object CineworldCardiff extends Cinema("Cineworld Cardiff", "Cineworld Cardiff")
case object EverymanCinemaCardiff extends Cinema("Everyman Cinema Cardiff", "Everyman Cardiff")
case object OdeonCinemaCardiff extends Cinema("Odeon Cinema Cardiff", "Odeon Cardiff")
case object ShowcaseCinemaCardiff extends Cinema("Showcase Cinema Cardiff", "Showcase Cardiff")
// ── United Kingdom · Central Scotland (Flicks) ──
case object ChalmersAlloaCinema extends Cinema("Chalmers Alloa Cinema", "Chalmers Alloa")
case object CineworldFalkirk extends Cinema("Cineworld Falkirk", "Cineworld Falkirk")
case object HippodromeBoNess extends Cinema("Hippodrome Bo'ness", "Hippodrome Bo'ness")
case object MacrobertArtCentreStirling extends Cinema("Macrobert Art Centre Stirling", "Macrobert Art Centre Stirling")
case object VueCinemasStirling extends Cinema("Vue Cinemas Stirling", "Vue Stirling")
// ── United Kingdom · Cheshire (Flicks) ──
case object BuxtonCinemaPavilionArtsCentre extends Cinema("Buxton Cinema, Pavilion Arts Centre", "Buxton , Pavilion Arts Centre")
case object CinemacMacclesfield extends Cinema("Cinemac Macclesfield", "Cinemac Macclesfield")
case object CineworldWarrington extends Cinema("Cineworld Warrington", "Cineworld Warrington")
case object CurzonCinemaKnutsford extends Cinema("Curzon Cinema Knutsford", "Curzon Knutsford")
case object EverymanCinemaAltrincham extends Cinema("Everyman Cinema Altrincham", "Everyman Altrincham")
case object OdeonCinemaCrewe extends Cinema("Odeon Cinema Crewe", "Odeon Crewe")
case object OdeonCinemaNorthwichBaronsQuay extends Cinema("Odeon Cinema Northwich Barons Quay", "Odeon Northwich Barons Quay")
case object OdeonLuxeWarrington extends Cinema("Odeon Luxe Warrington", "Odeon Luxe Warrington")
case object PicturehouseChester extends Cinema("Picturehouse Chester", "Picturehouse Chester")
case object ReelCinemaWidnes extends Cinema("Reel Cinema Widnes", "Reel Widnes")
case object RexWilmslow extends Cinema("Rex Wilmslow", "Rex Wilmslow")
case object StoryhouseChester extends Cinema("Storyhouse Chester", "Storyhouse Chester")
case object VueCinemasAltrincham extends Cinema("Vue Cinemas Altrincham", "Vue Altrincham")
case object VueCinemasCheshireOaks extends Cinema("Vue Cinemas Cheshire Oaks", "Vue Cheshire Oaks")
// ── United Kingdom · Clwyd (Flicks) ──
case object CineworldLlandudno extends Cinema("Cineworld Llandudno", "Cineworld Llandudno")
case object MerlinScalaPrestatyn extends Cinema("Merlin Scala Prestatyn", "Merlin Scala Prestatyn")
case object StrandCinemaRhyl extends Cinema("Strand Cinema Rhyl", "Strand Rhyl")
case object TheatrColwyn extends Cinema("Theatr Colwyn", "Theatr Colwyn")
// ── United Kingdom · Cornwall (Flicks) ──
case object CineworldPlymouth extends Cinema("Cineworld Plymouth", "Cineworld Plymouth")
case object FilmhouseNewlyn extends Cinema("Filmhouse Newlyn", "Filmhouse Newlyn")
case object FloraCinemaHelston extends Cinema("Flora Cinema Helston", "Flora Helston")
case object MerlinCapitolBodmin extends Cinema("Merlin Capitol Bodmin", "Merlin Capitol Bodmin")
case object MerlinRegalCinemaRedruth extends Cinema("Merlin Regal Cinema Redruth", "Merlin Regal Redruth")
case object MerlinSavoyPenzance extends Cinema("Merlin Savoy Penzance", "Merlin Savoy Penzance")
case object PhoenixCinemaFalmouth extends Cinema("Phoenix Cinema Falmouth", "Phoenix Falmouth")
case object PlymouthArtsCinema extends Cinema("Plymouth Arts Cinema", "Plymouth Arts")
case object RebelCinema extends Cinema("Rebel Cinema", "Rebel")
case object RoyalStIvesCinema extends Cinema("Royal St Ives Cinema", "Royal St Ives")
case object TheAstraCinemaStMawgan extends Cinema("The Astra Cinema St. Mawgan", "The Astra St. Mawgan")
case object ThePolyFalmouth extends Cinema("The Poly Falmouth", "The Poly Falmouth")
case object VueCinemasPlymouth extends Cinema("Vue Cinemas Plymouth", "Vue Plymouth")
case object WTWLighthouseNewquay extends Cinema("WTW Lighthouse Newquay", "WTW Lighthouse Newquay")
case object WTWPlazaTruro extends Cinema("WTW Plaza Truro", "WTW Plaza Truro")
case object WTWRegalWadebridge extends Cinema("WTW Regal Wadebridge", "WTW Regal Wadebridge")
case object WTWWhiteRiverCinema extends Cinema("WTW White River Cinema", "WTW White River")
// ── United Kingdom · County Durham (Flicks) ──
case object ARCStocktonOnTees extends Cinema("ARC Stockton-on-Tees", "ARC Stockton-on-Tees")
case object CineworldDaltonParkMurtonCounty extends Cinema("Cineworld Dalton Park (Murton County)", "Cineworld Dalton Park (Murton County)")
case object EmpireTheatreConsett extends Cinema("Empire Theatre Consett", "Empire Theatre Consett")
case object EverymanCinemaDurham extends Cinema("Everyman Cinema Durham", "Everyman Durham")
case object FuseCommunityCinemaPrudhoe extends Cinema("Fuse Community Cinema Prudhoe", "Fuse Community Prudhoe")
case object GalaCinemaDurham extends Cinema("Gala Cinema Durham", "Gala Durham")
case object OdeonLuxeDurham extends Cinema("Odeon Luxe Durham", "Odeon Luxe Durham")
case object ShowcaseCinemaDeLuxTeesside extends Cinema("Showcase Cinema de Lux Teesside", "Showcase de Lux Teesside")
case object VueCinemasDarlington extends Cinema("Vue Cinemas Darlington", "Vue Darlington")
case object VueCinemasHartlepool extends Cinema("Vue Cinemas Hartlepool", "Vue Hartlepool")
// ── United Kingdom · Cumbria (Flicks) ──
case object BreweryArtsCentreKendal extends Cinema("Brewery Arts Centre Kendal", "Brewery Arts Centre Kendal")
case object GaietyCinemaWhitehaven extends Cinema("Gaiety Cinema Whitehaven", "Gaiety Whitehaven")
case object KeswickAlhambra extends Cinema("Keswick Alhambra", "Keswick Alhambra")
case object LonsdaleAlhambraPenrith extends Cinema("Lonsdale Alhambra Penrith", "Lonsdale Alhambra Penrith")
case object ParkwayWorkington extends Cinema("Parkway Workington", "Parkway Workington")
case object ReelCinemaMorecambe extends Cinema("Reel Cinema Morecambe", "Reel Morecambe")
case object RoxyUlverston extends Cinema("Roxy Ulverston", "Roxy Ulverston")
case object RoyaltyBownessOnWindemere extends Cinema("Royalty Bowness-on-Windemere", "Royalty Bowness-on-Windemere")
case object TheRitzCinemaWorkington extends Cinema("The Ritz Cinema Workington", "The Ritz Workington")
case object VueCinemasBarrow extends Cinema("Vue Cinemas Barrow", "Vue Barrow")
case object VueCinemasCarlisle extends Cinema("Vue Cinemas Carlisle", "Vue Carlisle")
case object ZeffirellisCinemaAmbleside extends Cinema("Zeffirellis Cinema Ambleside", "Zeffirellis Ambleside")
// ── United Kingdom · Derbyshire (Flicks) ──
case object CineworldChesterfield extends Cinema("Cineworld Chesterfield", "Cineworld Chesterfield")
case object EliteCinemaAndTheatreAshbourne extends Cinema("Elite Cinema and Theatre Ashbourne", "Elite and Theatre Ashbourne")
case object NorthernLightWirksworth extends Cinema("Northern Light Wirksworth", "Northern Light Wirksworth")
case object OdeonCinemaSwadlincote extends Cinema("Odeon Cinema Swadlincote", "Odeon Swadlincote")
case object OdeonLuxeDerby extends Cinema("Odeon Luxe Derby", "Odeon Luxe Derby")
case object QuadDerby extends Cinema("Quad Derby", "Quad Derby")
case object RitzBelper extends Cinema("Ritz Belper", "Ritz Belper")
case object ShowcaseCinemaDeLuxDerby extends Cinema("Showcase Cinema de Lux Derby", "Showcase de Lux Derby")
// ── United Kingdom · Devon (Flicks) ──
case object AlexandraNewtonAbbot extends Cinema("Alexandra Newton Abbot", "Alexandra Newton Abbot")
case object BarnCinemaDartingtonArtCentre extends Cinema("Barn Cinema, Dartington Art Centre", "Barn , Dartington Art Centre")
case object CentralCinemaBarnstaple extends Cinema("Central Cinema Barnstaple", "Central Barnstaple")
case object EmbassyCinemaIlfracombe extends Cinema("Embassy Cinema Ilfracombe", "Embassy Ilfracombe")
case object EverymanCinemaPlymouth extends Cinema("Everyman Cinema Plymouth", "Everyman Plymouth")
case object KingsCinemaKingsbridge extends Cinema("Kings Cinema Kingsbridge", "Kings Kingsbridge")
case object LyntonCinema extends Cinema("Lynton Cinema", "Lynton")
case object NewCarltonOkehampton extends Cinema("New Carlton Okehampton", "New Carlton Okehampton")
case object NewCentralCinemaTorquay extends Cinema("New Central Cinema Torquay", "New Central Torquay")
case object OdeonCinemaExeter extends Cinema("Odeon Cinema Exeter", "Odeon Exeter")
case object PavilionsTeignmouth extends Cinema("Pavilions Teignmouth", "Pavilions Teignmouth")
case object PicturehouseExeter extends Cinema("Picturehouse Exeter", "Picturehouse Exeter")
case object PloughArtsCentreTorrington extends Cinema("Plough Arts Centre Torrington", "Plough Arts Centre Torrington")
case object RadwaySidmouth extends Cinema("Radway Sidmouth", "Radway Sidmouth")
case object SavoyScottCinemasExmouth extends Cinema("Savoy (Scott Cinemas) Exmouth", "Savoy (Scott ) Exmouth")
case object TheBeehiveHoniton extends Cinema("The Beehive Honiton", "The Beehive Honiton")
case object TheFlavel extends Cinema("The Flavel", "The Flavel")
case object TheWatermarkIvybridge extends Cinema("The Watermark Ivybridge", "The Watermark Ivybridge")
case object TivoliTiverton extends Cinema("Tivoli Tiverton", "Tivoli Tiverton")
case object TotnesCinema extends Cinema("Totnes Cinema", "Totnes")
case object VueCinemasExeter extends Cinema("Vue Cinemas Exeter", "Vue Exeter")
case object VueCinemasTorbayPaignton extends Cinema("Vue Cinemas Torbay (Paignton)", "Vue Torbay (Paignton)")
// ── United Kingdom · Dorset (Flicks) ──
case object ColosseumBournemouth extends Cinema("Colosseum Bournemouth", "Colosseum Bournemouth")
case object ElectricPalaceBridport extends Cinema("Electric Palace Bridport", "Electric Palace Bridport")
case object HilltopCinemaShaftesburyArtsCentre extends Cinema("Hilltop Cinema, Shaftesbury Arts Centre", "Hilltop , Shaftesbury Arts Centre")
case object LighthousePoole extends Cinema("Lighthouse Poole", "Lighthouse Poole")
case object MowlemTheatre extends Cinema("Mowlem Theatre", "Mowlem Theatre")
case object OdeonCinemaBournemouthBH2 extends Cinema("Odeon Cinema Bournemouth BH2", "Odeon Bournemouth BH2")
case object OdeonCinemaDorchester extends Cinema("Odeon Cinema Dorchester", "Odeon Dorchester")
case object PlazaCinemaDorchester extends Cinema("Plaza Cinema Dorchester", "Plaza Dorchester")
case object RegentChristchurch extends Cinema("Regent Christchurch", "Regent Christchurch")
case object TheNewVicTisburyVillageHall extends Cinema("The New Vic, Tisbury Village Hall", "The New Vic, Tisbury Village Hall")
case object TheRexCinemaWareham extends Cinema("The Rex Cinema Wareham", "The Rex Wareham")
case object TivoliTheatreWimborne extends Cinema("Tivoli Theatre Wimborne", "Tivoli Theatre Wimborne")
case object VueCinemasPoole extends Cinema("Vue Cinemas Poole", "Vue Poole")
// ── United Kingdom · Down (Flicks) ──
case object IMCNewtownardsMovieland extends Cinema("IMC Newtownards (Movieland)", "IMC Newtownards (Movieland)")
case object IveaghMovieStudioIMCBanbridge extends Cinema("Iveagh Movie Studio (IMC Banbridge)", "Iveagh Movie Studio (IMC Banbridge)")
case object OmniplexBanbridge extends Cinema("Omniplex Banbridge", "Omniplex Banbridge")
case object OmniplexBangor extends Cinema("Omniplex Bangor", "Omniplex Bangor")
case object OmniplexDownpatrick extends Cinema("Omniplex Downpatrick", "Omniplex Downpatrick")
case object OmniplexDundonald extends Cinema("Omniplex Dundonald", "Omniplex Dundonald")
case object OmniplexNewry extends Cinema("Omniplex Newry", "Omniplex Newry")
// ── United Kingdom · Dudley (Flicks) ──
case object OdeonCinemaDudley extends Cinema("Odeon Cinema Dudley", "Odeon Dudley")
case object ShowcaseCinemaDudley extends Cinema("Showcase Cinema Dudley", "Showcase Dudley")
// ── United Kingdom · Dumfries and Galloway (Flicks) ──
case object LonsdaleCityCinemaAnnan extends Cinema("Lonsdale City Cinema Annan", "Lonsdale City Annan")
case object RobertBurnsCentreFilmTheatre extends Cinema("Robert Burns Centre Film Theatre", "Robert Burns Centre Film Theatre")
case object TheCinemaNewtonStewart extends Cinema("The Cinema Newton Stewart", "The Newton Stewart")
case object TheFullartonCastleDouglas extends Cinema("The Fullarton Castle Douglas", "The Fullarton Castle Douglas")
// ── United Kingdom · Dunbartonshire and Argyll & Bute (Flicks) ──
case object CampbeltownPictureHouse extends Cinema("Campbeltown Picture House", "Campbeltown Picture House")
case object DiscoveryCentreCinemaRothesay extends Cinema("Discovery Centre Cinema Rothesay", "Discovery Centre Rothesay")
case object OmniplexClydebankFormerlyEmpire extends Cinema("Omniplex Clydebank (formerly Empire)", "Omniplex Clydebank (formerly Empire)")
case object StudioCinemaDunoon extends Cinema("Studio Cinema Dunoon", "Studio Dunoon")
// ── United Kingdom · Dyfed (Flicks) ──
case object AberystwythArtsCentre extends Cinema("Aberystwyth Arts Centre", "Aberystwyth Arts Centre")
case object CommodoreCinemaAberystwyth extends Cinema("Commodore Cinema Aberystwyth", "Commodore Aberystwyth")
case object CrossHandsHallCinema extends Cinema("Cross Hands Hall & Cinema", "Cross Hands Hall &")
case object Libanus1877 extends Cinema("Libanus 1877", "Libanus 1877")
case object MinersWelfareAndCommunityHallYstradgynlais extends Cinema("Miners Welfare and Community Hall Ystradgynlais", "Miners Welfare and Community Hall Ystradgynlais")
case object OdeonCinemaLlanelli extends Cinema("Odeon Cinema Llanelli", "Odeon Llanelli")
case object PalaceCinemaHaverfordwest extends Cinema("Palace Cinema Haverfordwest", "Palace Haverfordwest")
case object PublicHallBrynamman extends Cinema("Public Hall Brynamman", "Public Hall Brynamman")
case object TheatrGwaunFishguard extends Cinema("Theatr Gwaun Fishguard", "Theatr Gwaun Fishguard")
case object TheatrMwldanCardigan extends Cinema("Theatr Mwldan Cardigan", "Theatr Mwldan Cardigan")
case object TorchTheatreMilfordHaven extends Cinema("Torch Theatre Milford Haven", "Torch Theatre Milford Haven")
case object VueCinemasCarmarthen extends Cinema("Vue Cinemas Carmarthen", "Vue Carmarthen")
// ── United Kingdom · East Sussex (Flicks) ──
case object CineworldBrighton extends Cinema("Cineworld Brighton", "Cineworld Brighton")
case object CineworldEastbourne extends Cinema("Cineworld Eastbourne", "Cineworld Eastbourne")
case object DepotLewes extends Cinema("Depot Lewes", "Depot Lewes")
case object DukeOfYorkSPicturehouseBrighton extends Cinema("Duke of York's Picturehouse Brighton", "Duke of York's Picturehouse Brighton")
case object DukeSAtKomediaPicturehouse extends Cinema("Duke's at Komedia Picturehouse", "Duke's at Komedia Picturehouse")
case object ElectricPalaceHastings extends Cinema("Electric Palace Hastings", "Electric Palace Hastings")
case object KinoRye extends Cinema("Kino Rye", "Kino Rye")
case object KinoTeatr extends Cinema("Kino-Teatr", "Kino-Teatr")
case object OdeonCinemaBrighton extends Cinema("Odeon Cinema Brighton", "Odeon Brighton")
case object OdeonCinemaHastings extends Cinema("Odeon Cinema Hastings", "Odeon Hastings")
case object PavilionHailsham extends Cinema("Pavilion Hailsham", "Pavilion Hailsham")
case object PictureHouseUckfield extends Cinema("Picture House Uckfield", "Picture House Uckfield")
case object TownerEastbourneCinema extends Cinema("Towner Eastbourne Cinema", "Towner Eastbourne")
// ── United Kingdom · East Yorkshire (Flicks) ──
case object CineworldHull extends Cinema("Cineworld Hull", "Cineworld Hull")
case object ForumBridlington extends Cinema("Forum Bridlington", "Forum Bridlington")
case object OdeonLuxeHull extends Cinema("Odeon Luxe Hull", "Odeon Luxe Hull")
case object PalaceCinemaMalton extends Cinema("Palace Cinema Malton", "Palace Malton")
case object ParkwayBeverley extends Cinema("Parkway Beverley", "Parkway Beverley")
case object ReelCinemaHull extends Cinema("Reel Cinema Hull", "Reel Hull")
case object VueCinemasHull extends Cinema("Vue Cinemas Hull", "Vue Hull")
// ── United Kingdom · Edinburgh & Lothians (Flicks) ──
case object CineworldEdinburgh extends Cinema("Cineworld Edinburgh", "Cineworld Edinburgh")
case object DominionCinemaEdinburgh extends Cinema("Dominion Cinema Edinburgh", "Dominion Edinburgh")
case object EverymanCinemaEdinburgh extends Cinema("Everyman Cinema Edinburgh", "Everyman Edinburgh")
case object FilmhouseEdinburgh extends Cinema("Filmhouse Edinburgh", "Filmhouse Edinburgh")
case object OdeonEdinburghFortKinnaird extends Cinema("Odeon Edinburgh (Fort Kinnaird)", "Odeon Edinburgh (Fort Kinnaird)")
case object OdeonEdinburghLothianRoad extends Cinema("Odeon Edinburgh (Lothian Road)", "Odeon Edinburgh (Lothian Road)")
case object OdeonLuxeEdinburghEdinburghWest extends Cinema("Odeon Luxe Edinburgh (Edinburgh West)", "Odeon Luxe Edinburgh (Edinburgh West)")
case object ScotsmanPicturehouseEdinburgh extends Cinema("Scotsman Picturehouse Edinburgh", "Scotsman Picturehouse Edinburgh")
case object TheCameoPicturehouse extends Cinema("The Cameo, Picturehouse", "The Cameo, Picturehouse")
case object TheFraserCentreTranent extends Cinema("The Fraser Centre Tranent", "The Fraser Centre Tranent")
case object VueCinemasLivingston extends Cinema("Vue Cinemas Livingston", "Vue Livingston")
case object VueEdinburghOceanTerminal extends Cinema("Vue Edinburgh Ocean Terminal", "Vue Edinburgh Ocean Terminal")
case object VueEdinburghOmniCentre extends Cinema("Vue Edinburgh Omni Centre", "Vue Edinburgh Omni Centre")
// ── United Kingdom · Essex (Flicks) ──
case object CenturyCinemaClacton extends Cinema("Century Cinema Clacton", "Century Clacton")
case object CineworldBasildon extends Cinema("Cineworld Basildon", "Cineworld Basildon")
case object CineworldBraintree extends Cinema("Cineworld Braintree", "Cineworld Braintree")
case object CineworldHarlowHarveyCentre extends Cinema("Cineworld Harlow (Harvey Centre)", "Cineworld Harlow (Harvey Centre)")
case object CineworldHarlowQueensgate extends Cinema("Cineworld Harlow (Queensgate)", "Cineworld Harlow (Queensgate)")
case object CurzonCinemaColchester extends Cinema("Curzon Cinema Colchester", "Curzon Colchester")
case object ElectricPalaceHarwich extends Cinema("Electric Palace Harwich", "Electric Palace Harwich")
case object EmpireTheatreHalstead extends Cinema("Empire Theatre Halstead", "Empire Theatre Halstead")
case object EverymanCinemaChelmsford extends Cinema("Everyman Cinema Chelmsford", "Everyman Chelmsford")
case object MovieStarrCanveyIsland extends Cinema("Movie Starr Canvey Island", "Movie Starr Canvey Island")
case object OdeonCinemaChelmsford extends Cinema("Odeon Cinema Chelmsford", "Odeon Chelmsford")
case object OdeonCinemaColchester extends Cinema("Odeon Cinema Colchester", "Odeon Colchester")
case object OdeonCinemaSouthendOnSea extends Cinema("Odeon Cinema Southend-on-Sea", "Odeon Southend-on-Sea")
case object RioBurnhamOnCrouch extends Cinema("Rio Burnham-on-Crouch", "Rio Burnham-on-Crouch")
case object RoxyMoviesBishopSStortford extends Cinema("Roxy Movies Bishop’s Stortford", "Roxy Movies Bishop’s Stortford")
case object SaffronScreen extends Cinema("Saffron Screen", "Saffron Screen")
case object VueCinemasBasildon extends Cinema("Vue Cinemas Basildon", "Vue Basildon")
case object VueCinemasColchester extends Cinema("Vue Cinemas Colchester", "Vue Colchester")
case object VueCinemasWestThurrock extends Cinema("Vue Cinemas West Thurrock", "Vue West Thurrock")
// ── United Kingdom · Fermanagh (Flicks) ──
case object IMCCinemaEnniskillen extends Cinema("IMC Cinema Enniskillen", "IMC Enniskillen")
// ── United Kingdom · Fife (Flicks) ──
case object AdamSmithTheatreKirkcaldy extends Cinema("Adam Smith Theatre Kirkcaldy", "Adam Smith Theatre Kirkcaldy")
case object KinoGlenrothes extends Cinema("Kino Glenrothes", "Kino Glenrothes")
case object OdeonCinemaDunfermline extends Cinema("Odeon Cinema Dunfermline", "Odeon Dunfermline")
// ── United Kingdom · Glamorgan (Flicks) ──
case object ColiseumTheatreAberdare extends Cinema("Coliseum Theatre Aberdare", "Coliseum Theatre Aberdare")
case object GwynHallNeath extends Cinema("Gwyn Hall Neath", "Gwyn Hall Neath")
case object OdeonCinemaBridgend extends Cinema("Odeon Cinema Bridgend", "Odeon Bridgend")
case object OdeonCinemaSwansea extends Cinema("Odeon Cinema Swansea", "Odeon Swansea")
case object PontardaweArtsCentre extends Cinema("Pontardawe Arts Centre", "Pontardawe Arts Centre")
case object ReelCinemaPortTalbot extends Cinema("Reel Cinema Port Talbot", "Reel Port Talbot")
case object TaliesinArtsCentreSwansea extends Cinema("Taliesin Arts Centre Swansea", "Taliesin Arts Centre Swansea")
case object VueCinemasMerthyrTydfil extends Cinema("Vue Cinemas Merthyr Tydfil", "Vue Merthyr Tydfil")
case object VueCinemasSwansea extends Cinema("Vue Cinemas Swansea", "Vue Swansea")
// ── United Kingdom · Glasgow (Flicks) ──
case object CineworldSilverburnGlasgow extends Cinema("Cineworld (Silverburn) Glasgow", "Cineworld (Silverburn) Glasgow")
case object EverymanCinemaGlasgow extends Cinema("Everyman Cinema Glasgow", "Everyman Glasgow")
case object GlasgowFilmTheatre extends Cinema("Glasgow Film Theatre", "Glasgow Film Theatre")
case object GrosvenorCinemaGlasgow extends Cinema("Grosvenor Cinema Glasgow", "Grosvenor Glasgow")
case object IMAXAtGlasgowScienceCentre extends Cinema("IMAX at Glasgow Science Centre", "IMAX at Glasgow Science Centre")
case object LanternhouseCinema extends Cinema("Lanternhouse Cinema", "Lanternhouse")
case object OdeonLuxeGlasgow extends Cinema("Odeon Luxe Glasgow", "Odeon Luxe Glasgow")
case object VueCinemasGlasgowFort extends Cinema("Vue Cinemas Glasgow Fort", "Vue Glasgow Fort")
case object VueCinemasGlasgowStEnoch extends Cinema("Vue Cinemas Glasgow St. Enoch", "Vue Glasgow St. Enoch")
// ── United Kingdom · Gloucestershire (Flicks) ──
case object CineworldCheltenham extends Cinema("Cineworld Cheltenham", "Cineworld Cheltenham")
case object CineworldGloucesterQuays extends Cinema("Cineworld Gloucester Quays", "Cineworld Gloucester Quays")
case object ElectricPictureHouseWottonUnderEdge extends Cinema("Electric Picture House Wotton-Under-Edge", "Electric Picture House Wotton-Under-Edge")
case object EverymanCheltenham extends Cinema("Everyman Cheltenham", "Everyman Cheltenham")
case object GuildhallCinemaGloucester extends Cinema("Guildhall Cinema Gloucester", "Guildhall Gloucester")
case object MerlinStudioColeford extends Cinema("Merlin Studio Coleford", "Merlin Studio Coleford")
case object PalaceCinemaCinderford extends Cinema("Palace Cinema Cinderford", "Palace Cinderford")
case object RosesTheatreTewkesbury extends Cinema("Roses Theatre Tewkesbury", "Roses Theatre Tewkesbury")
case object SherborneCinemaGloucester extends Cinema("Sherborne Cinema Gloucester", "Sherborne Gloucester")
case object VueCinemasStroud extends Cinema("Vue Cinemas Stroud", "Vue Stroud")
// ── United Kingdom · Guernsey (Flicks) ──
case object BeauSejourLeisureCentreGuernsey extends Cinema("Beau Séjour Leisure Centre, ​Guernsey", "Beau Séjour Leisure Centre, ​Guernsey")
case object TheMallardCinemaGuernsey extends Cinema("The Mallard Cinema, Guernsey", "The Mallard , Guernsey")
// ── United Kingdom · Gwent (Flicks) ──
case object BakerStreetCinemaAbergavenny extends Cinema("Baker Street Cinema Abergavenny", "Baker Street Abergavenny")
case object CineworldSpyttyParkNewport extends Cinema("Cineworld (Spytty Park) Newport", "Cineworld (Spytty Park) Newport")
case object MarketHallCinemaBrynmawr extends Cinema("Market Hall Cinema Brynmawr", "Market Hall Brynmawr")
case object MaximeCinemaBlackwood extends Cinema("Maxime Cinema Blackwood", "Maxime Blackwood")
case object RiverfrontNewport extends Cinema("Riverfront Newport", "Riverfront Newport")
case object SavoyTheatreMonmouth extends Cinema("Savoy Theatre Monmouth", "Savoy Theatre Monmouth")
case object VueCinemasCwmbran extends Cinema("Vue Cinemas Cwmbran", "Vue Cwmbran")
// ── United Kingdom · Gwynedd (Flicks) ──
case object CellBBlaenauFfestiniog extends Cinema("Cell B Blaenau Ffestiniog", "Cell B Blaenau Ffestiniog")
case object EmpireCinemaHolyhead extends Cinema("Empire Cinema Holyhead", "Empire Holyhead")
case object GaleriCaenarfon extends Cinema("Galeri Caenarfon", "Galeri Caenarfon")
case object MagicLanternTywyn extends Cinema("Magic Lantern Tywyn", "Magic Lantern Tywyn")
case object NeuaddDwyforPwllheli extends Cinema("Neuadd Dwyfor Pwllheli", "Neuadd Dwyfor Pwllheli")
case object PontioArtsInnovationCentreBangor extends Cinema("Pontio Arts & Innovation Centre Bangor", "Pontio Arts & Innovation Centre Bangor")
case object TheatrDerekWilliams extends Cinema("Theatr Derek Williams", "Theatr Derek Williams")
// ── United Kingdom · Hampshire (Flicks) ──
case object ChichesterCinemaAtNewPark extends Cinema("Chichester Cinema at New Park", "Chichester at New Park")
case object CineworldChichester extends Cinema("Cineworld Chichester", "Cineworld Chichester")
case object CineworldWhiteley extends Cinema("Cineworld Whiteley", "Cineworld Whiteley")
case object EverymanCinemaWinchester extends Cinema("Everyman Cinema Winchester", "Everyman Winchester")
case object HarbourLightsPicturehouse extends Cinema("Harbour Lights Picturehouse", "Harbour Lights Picturehouse")
case object HytheMoviolaCinema extends Cinema("Hythe Moviola Cinema", "Hythe Moviola")
case object No6CinemaPortsmouth extends Cinema("No.6 Cinema Portsmouth", "No.6 Portsmouth")
case object OdeonCinemaBasingstoke extends Cinema("Odeon Cinema Basingstoke", "Odeon Basingstoke")
case object OdeonCinemaPortSolent extends Cinema("Odeon Cinema Port Solent", "Odeon Port Solent")
case object ReelCinemaFareham extends Cinema("Reel Cinema Fareham", "Reel Fareham")
case object ShowcaseDeLuxSouthampton extends Cinema("Showcase de Lux Southampton", "Showcase de Lux Southampton")
case object SouthseaCinemaArtsCentre extends Cinema("Southsea Cinema & Arts Centre", "Southsea & Arts Centre")
case object TheLivingRoomCinemaLiphook extends Cinema("The Living Room Cinema Liphook", "The Living Room Liphook")
case object TheMaltLymington extends Cinema("The Malt Lymington", "The Malt Lymington")
case object VueCinemasBasingstoke extends Cinema("Vue Cinemas Basingstoke", "Vue Basingstoke")
case object VueCinemasEastleigh extends Cinema("Vue Cinemas Eastleigh", "Vue Eastleigh")
case object VueCinemasPortsmouth extends Cinema("Vue Cinemas Portsmouth", "Vue Portsmouth")
// ── United Kingdom · Herefordshire (Flicks) ──
case object CourtyardHereford extends Cinema("Courtyard Hereford", "Courtyard Hereford")
case object GatewayCinemaRossOnWye extends Cinema("Gateway Cinema Ross-on-Wye", "Gateway Ross-on-Wye")
case object OdeonCinemaHereford extends Cinema("Odeon Cinema Hereford", "Odeon Hereford")
case object RichardBoothSBookshopHayOnWye extends Cinema("Richard Booth's Bookshop Hay-on-Wye", "Richard Booth's Bookshop Hay-on-Wye")
// ── United Kingdom · Hertfordshire (Flicks) ──
case object BaldockArtsHeritageCentre extends Cinema("Baldock Arts & Heritage Centre", "Baldock Arts & Heritage Centre")
case object BEAMHertfordTheatre extends Cinema("BEAM (Hertford Theatre)", "BEAM (Hertford Theatre)")
case object BroadwayLetchworth extends Cinema("Broadway Letchworth", "Broadway Letchworth")
case object CineworldHemelHempstead extends Cinema("Cineworld Hemel Hempstead", "Cineworld Hemel Hempstead")
case object CineworldStevenage extends Cinema("Cineworld Stevenage", "Cineworld Stevenage")
case object CineworldWatford extends Cinema("Cineworld Watford", "Cineworld Watford")
case object OdeonCinemaHatfield extends Cinema("Odeon Cinema Hatfield", "Odeon Hatfield")
case object ReelCinemaBorehamwood extends Cinema("Reel Cinema Borehamwood", "Reel Borehamwood")
case object TheCinemaCampusWest extends Cinema("The Cinema, Campus West", "The , Campus West")
case object TheOdysseyStAlbans extends Cinema("The Odyssey St Albans", "The Odyssey St Albans")
case object TheRexCinemaBerkhamsted extends Cinema("The Rex Cinema Berkhamsted", "The Rex Berkhamsted")
case object VueCinemasWatford extends Cinema("Vue Cinemas Watford", "Vue Watford")
case object Watersmeet extends Cinema("Watersmeet", "Watersmeet")
// ── United Kingdom · Highlands and Islands (Flicks) ──
case object AnLanntairArtsCentreStornowayIsleOfLewis extends Cinema("An Lanntair Arts Centre Stornoway, Isle of Lewis", "An Lanntair Arts Centre Stornoway, Isle of Lewis")
case object CromartyCinema extends Cinema("Cromarty Cinema", "Cromarty")
case object EdenCourtTheatreInverness extends Cinema("Eden Court Theatre Inverness", "Eden Court Theatre Inverness")
case object HighlandCinemaFortWilliam extends Cinema("Highland Cinema Fort William", "Highland Fort William")
case object LASPortRighArosCinemaPortree extends Cinema("LAS Port Righ (Aros Cinema Portree)", "LAS Port Righ (Aros Portree)")
case object MareelLerwickShetlandIslands extends Cinema("Mareel Lerwick, Shetland Islands", "Mareel Lerwick, Shetland Islands")
case object MerlinCinemaThurso extends Cinema("Merlin Cinema Thurso", "Merlin Thurso")
case object PhoenixKirkwallOkneyIslands extends Cinema("Phoenix Kirkwall, Okney Islands", "Phoenix Kirkwall, Okney Islands")
case object SpeyValleyCinemaAviemore extends Cinema("Spey Valley Cinema Aviemore", "Spey Valley Aviemore")
case object VueCinemasInverness extends Cinema("Vue Cinemas Inverness", "Vue Inverness")
case object WestSideCinemaStromness extends Cinema("West Side Cinema Stromness", "West Side Stromness")
// ── United Kingdom · Isle of Man (Flicks) ──
case object BroadwayCinemaVillaMarina extends Cinema("Broadway Cinema Villa Marina", "Broadway Villa Marina")
case object PalaceCinemasIsleOfMan extends Cinema("Palace Cinemas Isle of Man", "Palace Isle of Man")
// ── United Kingdom · Isle of Wight (Flicks) ──
case object CineworldNewportIsleOfWight extends Cinema("Cineworld Newport, Isle of Wight", "Cineworld Newport, Isle of Wight")
case object CommodoreRydeIsleOfWight extends Cinema("Commodore Ryde, Isle of Wight", "Commodore Ryde, Isle of Wight")
// ── United Kingdom · Jersey (Flicks) ──
case object CineworldStHelierJersey extends Cinema("Cineworld St Helier, Jersey", "Cineworld St Helier, Jersey")
// ── United Kingdom · Kent (Flicks) ──
case object CarltonCinemaWestgateOnSea extends Cinema("Carlton Cinema Westgate-on-Sea", "Carlton Westgate-on-Sea")
case object CinemarshTheMarshAcademy extends Cinema("Cinemarsh, The Marsh Academy", "Cinemarsh, The Marsh Academy")
case object CineworldAshford extends Cinema("Cineworld Ashford", "Cineworld Ashford")
case object CineworldDover extends Cinema("Cineworld Dover", "Cineworld Dover")
case object CineworldRochester extends Cinema("Cineworld Rochester", "Cineworld Rochester")
case object CurzonCanterburyRiverside extends Cinema("Curzon Canterbury Riverside", "Curzon Canterbury Riverside")
case object EmpireCinemaSandwich extends Cinema("Empire Cinema Sandwich", "Empire Sandwich")
case object GulbenkianTheatre extends Cinema("Gulbenkian Theatre", "Gulbenkian Theatre")
case object KavanaghCinemaHerneBay extends Cinema("Kavanagh Cinema Herne Bay", "Kavanagh Herne Bay")
case object KinoHawkhurst extends Cinema("Kino Hawkhurst", "Kino Hawkhurst")
case object OdeonCinemaChatham extends Cinema("Odeon Cinema Chatham", "Odeon Chatham")
case object OdeonCinemaMaidstone extends Cinema("Odeon Cinema Maidstone", "Odeon Maidstone")
case object OdeonCinemaTunbridgeWells extends Cinema("Odeon Cinema Tunbridge Wells", "Odeon Tunbridge Wells")
case object PalaceCinemaKent extends Cinema("Palace Cinema Kent", "Palace Kent")
case object RoyalCinemaFaversham extends Cinema("Royal Cinema Faversham", "Royal Faversham")
case object ShowcaseDeLuxBluewater extends Cinema("Showcase de Lux Bluewater", "Showcase de Lux Bluewater")
case object SilverScreenFolkestone extends Cinema("Silver Screen Folkestone", "Silver Screen Folkestone")
case object StagSevenoaks extends Cinema("Stag Sevenoaks", "Stag Sevenoaks")
case object TheAshfordCinemaFormerlyPicturehouse extends Cinema("The Ashford Cinema (formerly Picturehouse)", "The Ashford (formerly Picturehouse)")
case object TheLightSittingbourne extends Cinema("The Light Sittingbourne", "The Light Sittingbourne")
case object TheWoodvilleGravesend extends Cinema("The Woodville Gravesend", "The Woodville Gravesend")
case object VueCinemasThanetWestwoodCross extends Cinema("Vue Cinemas Thanet Westwood Cross", "Vue Thanet Westwood Cross")
// ── United Kingdom · Lanarkshire (Flicks) ──
case object OdeonLuxeEastKilbride extends Cinema("Odeon Luxe East Kilbride", "Odeon Luxe East Kilbride")
case object ShowcaseGlasgowCoatbridge extends Cinema("Showcase Glasgow Coatbridge", "Showcase Glasgow Coatbridge")
case object VueCinemasHamilton extends Cinema("Vue Cinemas Hamilton", "Vue Hamilton")
// ── United Kingdom · Lancashire (Flicks) ──
case object ArcCinemaBlackpool extends Cinema("Arc Cinema Blackpool", "Arc Blackpool")
case object ArcCinemaPreston extends Cinema("Arc Cinema Preston", "Arc Preston")
case object CineworldBolton extends Cinema("Cineworld Bolton", "Cineworld Bolton")
case object CineworldBroughton extends Cinema("Cineworld Broughton", "Cineworld Broughton")
case object EverymanCinemaClitheroe extends Cinema("Everyman Cinema Clitheroe", "Everyman Clitheroe")
case object FlowerBowlEntertainmentCentrePreston extends Cinema("Flower Bowl Entertainment Centre Preston", "Flower Bowl Entertainment Centre Preston")
case object LowtherPavilionLytham extends Cinema("Lowther Pavilion Lytham", "Lowther Pavilion Lytham")
case object OdeonCinemaPreston extends Cinema("Odeon Cinema Preston", "Odeon Preston")
case object OdeonCinemaRochdale extends Cinema("Odeon Cinema Rochdale", "Odeon Rochdale")
case object ReelCinemaBlackburn extends Cinema("Reel Cinema Blackburn", "Reel Blackburn")
case object ReelCinemaChorley extends Cinema("Reel Cinema Chorley", "Reel Chorley")
case object ReelCinemasBurnley extends Cinema("Reel Cinemas Burnley", "Reel Burnley")
case object RegentBlackpool extends Cinema("Regent Blackpool", "Regent Blackpool")
case object TheDukesLancaster extends Cinema("The Dukes Lancaster", "The Dukes Lancaster")
case object TheIslandLythamStAnnes extends Cinema("The Island, Lytham St. Annes", "The Island, Lytham St. Annes")
case object TheLightBolton extends Cinema("The Light Bolton", "The Light Bolton")
case object VueCinemasAccrington extends Cinema("Vue Cinemas Accrington", "Vue Accrington")
case object VueCinemasBlackburn extends Cinema("Vue Cinemas Blackburn", "Vue Blackburn")
case object VueCinemasBolton extends Cinema("Vue Cinemas Bolton", "Vue Bolton")
case object VueCinemasBury extends Cinema("Vue Cinemas Bury", "Vue Bury")
case object VueCinemasCleveleys extends Cinema("Vue Cinemas Cleveleys", "Vue Cleveleys")
case object VueCinemasLancaster extends Cinema("Vue Cinemas Lancaster", "Vue Lancaster")
case object VueCinemasPreston extends Cinema("Vue Cinemas Preston", "Vue Preston")
// ── United Kingdom · Leicestershire (Flicks) ──
case object CineworldHinckley extends Cinema("Cineworld Hinckley", "Cineworld Hinckley")
case object FlixStudentRunCinemaLoughborough extends Cinema("Flix Student-Run Cinema Loughborough", "Flix Student-Run Loughborough")
case object OdeonCinemaLoughborough extends Cinema("Odeon Cinema Loughborough", "Odeon Loughborough")
case object OdeonLuxeLeicester extends Cinema("Odeon Luxe Leicester", "Odeon Luxe Leicester")
case object PhoenixCinemaAndArtCentreLeicester extends Cinema("Phoenix Cinema and Art Centre Leicester", "Phoenix and Art Centre Leicester")
case object PiccadillyCinemaLeicester extends Cinema("Piccadilly Cinema Leicester", "Piccadilly Leicester")
case object RegalMeltonMowbray extends Cinema("Regal Melton Mowbray", "Regal Melton Mowbray")
case object ShowcaseDeLuxLeicester extends Cinema("Showcase de Lux Leicester", "Showcase de Lux Leicester")
case object VueCinemasLeicester extends Cinema("Vue Cinemas Leicester", "Vue Leicester")
// ── United Kingdom · Lincolnshire (Flicks) ──
case object ArtsCentreStamford extends Cinema("Arts Centre Stamford", "Arts Centre Stamford")
case object EverymanCinemaLincoln extends Cinema("Everyman Cinema Lincoln", "Everyman Lincoln")
case object JunctionGoole extends Cinema("Junction Goole", "Junction Goole")
case object KinemaInTheWoods extends Cinema("Kinema in the Woods", "Kinema in the Woods")
case object LoewenCinema extends Cinema("Loewen Cinema", "Loewen")
case object OdeonCinemaLincoln extends Cinema("Odeon Cinema Lincoln", "Odeon Lincoln")
case object ParkwayCinemaLouth extends Cinema("Parkway Cinema Louth", "Parkway Louth")
case object ParkwayCleethorpes extends Cinema("Parkway Cleethorpes", "Parkway Cleethorpes")
case object SavoyBoston extends Cinema("Savoy Boston", "Savoy Boston")
case object SavoyGrantham extends Cinema("Savoy Grantham", "Savoy Grantham")
case object SleafordPlayhouse extends Cinema("Sleaford Playhouse", "Sleaford Playhouse")
case object TowerCinemaSkegness extends Cinema("Tower Cinema Skegness", "Tower Skegness")
case object VueCinemasScunthorpe extends Cinema("Vue Cinemas Scunthorpe", "Vue Scunthorpe")
// ── United Kingdom · Londonderry (Flicks) ──
case object BrunswickMoviebowlLondonderry extends Cinema("Brunswick Moviebowl Londonderry", "Brunswick Moviebowl Londonderry")
case object MovieHouseColeraine extends Cinema("Movie House Coleraine", "Movie House Coleraine")
case object MovieHouseMaghera extends Cinema("Movie House Maghera", "Movie House Maghera")
case object NerveCentreLondonderry extends Cinema("Nerve Centre Londonderry", "Nerve Centre Londonderry")
case object OmniplexLondonderry extends Cinema("Omniplex Londonderry", "Omniplex Londonderry")
// ── United Kingdom · Liverpool (Flicks) ──
case object CineworldSpeke extends Cinema("Cineworld Speke", "Cineworld Speke")
case object CineworldStHelens extends Cinema("Cineworld St Helens", "Cineworld St Helens")
case object EverymanCinemaLiverpool extends Cinema("Everyman Cinema Liverpool", "Everyman Liverpool")
case object OdeonLiverpoolONE extends Cinema("Odeon Liverpool ONE", "Odeon Liverpool ONE")
case object OdeonLiverpoolSwitchIsland extends Cinema("Odeon Liverpool Switch Island", "Odeon Liverpool Switch Island")
case object OdeonLuxeBromborough extends Cinema("Odeon Luxe Bromborough", "Odeon Luxe Bromborough")
case object PicturehouseAtFACTLiverpool extends Cinema("Picturehouse at FACT Liverpool", "Picturehouse at FACT Liverpool")
case object PlazaCommunityCinemaLiverpool extends Cinema("Plaza Community Cinema Liverpool", "Plaza Community Liverpool")
case object ShowcaseDeLuxLiverpool extends Cinema("Showcase de Lux Liverpool", "Showcase de Lux Liverpool")
case object SouthportBijouCinema extends Cinema("Southport Bijou Cinema", "Southport Bijou")
case object TheLightNewBrighton extends Cinema("The Light New Brighton", "The Light New Brighton")
case object VueCinemasBirkenhead extends Cinema("Vue Cinemas Birkenhead", "Vue Birkenhead")
case object VueCinemasSouthport extends Cinema("Vue Cinemas Southport", "Vue Southport")
case object WooltonPictureHouse extends Cinema("Woolton Picture House", "Woolton Picture House")
// ── United Kingdom · North Yorkshire (Flicks) ──
case object CineworldYork extends Cinema("Cineworld York", "Cineworld York")
case object CityScreenPicturehouseYork extends Cinema("City Screen Picturehouse York", "City Screen Picturehouse York")
case object EverymanCinemaHarrogate extends Cinema("Everyman Cinema Harrogate", "Everyman Harrogate")
case object EverymanCinemaNorthallerton extends Cinema("Everyman Cinema Northallerton", "Everyman Northallerton")
case object EverymanCinemaYork extends Cinema("Everyman Cinema York", "Everyman York")
case object HollywoodPlazaScarborough extends Cinema("Hollywood Plaza Scarborough", "Hollywood Plaza Scarborough")
case object OdeonCinemaHarrogate extends Cinema("Odeon Cinema Harrogate", "Odeon Harrogate")
case object OdeonMiddlesbrough extends Cinema("Odeon Middlesbrough", "Odeon Middlesbrough")
case object PavilionCinemaWhitby extends Cinema("Pavilion Cinema Whitby", "Pavilion Whitby")
case object PocklingtonArtsCentre extends Cinema("Pocklington Arts Centre", "Pocklington Arts Centre")
case object RegentRedcar extends Cinema("Regent Redcar", "Regent Redcar")
case object RitzCinemaThirsk extends Cinema("Ritz Cinema Thirsk", "Ritz Thirsk")
case object RoxyMoviesMiddlesbrough extends Cinema("Roxy Movies Middlesbrough", "Roxy Movies Middlesbrough")
case object SavoyCinemaCatterickGarrison extends Cinema("Savoy Cinema Catterick Garrison", "Savoy Catterick Garrison")
case object StationCinemaRichmond extends Cinema("Station Cinema Richmond", "Station Richmond")
case object StephenJosephTheatreScarborough extends Cinema("Stephen Joseph Theatre Scarborough", "Stephen Joseph Theatre Scarborough")
case object TheForumNorthallerton extends Cinema("The Forum Northallerton", "The Forum Northallerton")
case object VueCinemasYork extends Cinema("Vue Cinemas York", "Vue York")
// ── United Kingdom · Northamptonshire (Flicks) ──
case object ArcCinemaDaventry extends Cinema("Arc Cinema Daventry", "Arc Daventry")
case object CineworldRushdenLakes extends Cinema("Cineworld Rushden Lakes", "Cineworld Rushden Lakes")
case object ForumCinemaNorthampton extends Cinema("Forum Cinema Northampton", "Forum Northampton")
case object NorthamptonFilmhouse extends Cinema("Northampton Filmhouse", "Northampton Filmhouse")
case object OdeonNorthampton extends Cinema("Odeon Northampton", "Odeon Northampton")
case object SavoyCinemaCorby extends Cinema("Savoy Cinema Corby", "Savoy Corby")
case object VueCinemasNorthampton extends Cinema("Vue Cinemas Northampton", "Vue Northampton")
// ── United Kingdom · Northumberland (Flicks) ──
case object ForumCinemaHexham extends Cinema("Forum Cinema Hexham", "Forum Hexham")
case object MarketPavilionCinemaBlyth extends Cinema("Market Pavilion Cinema Blyth", "Market Pavilion Blyth")
case object PhoenixCinemaBlyth extends Cinema("Phoenix Cinema Blyth", "Phoenix Blyth")
case object TheMaltingsBerwickUponTweed extends Cinema("The Maltings Berwick-Upon-Tweed", "The Maltings Berwick-Upon-Tweed")
case object VueCinemasCramlington extends Cinema("Vue Cinemas Cramlington", "Vue Cramlington")
// ── United Kingdom · Nottinghamshire (Flicks) ──
case object ArcCinemaAtTheByronHucknall extends Cinema("Arc Cinema at The Byron, Hucknall", "Arc at The Byron, Hucknall")
case object ArcCinemaBeeston extends Cinema("Arc Cinema Beeston", "Arc Beeston")
case object BroadwayCinemaNottingham extends Cinema("Broadway Cinema Nottingham", "Broadway Nottingham")
case object OdeonCinemaMansfield extends Cinema("Odeon Cinema Mansfield", "Odeon Mansfield")
case object OdeonCinemaNewark extends Cinema("Odeon Cinema Newark", "Odeon Newark")
case object ReelCinemaScalaIlkeston extends Cinema("Reel Cinema Scala Ilkeston", "Reel Scala Ilkeston")
case object SavoyCinemaNottingham extends Cinema("Savoy Cinema Nottingham", "Savoy Nottingham")
case object SavoyWorksop extends Cinema("Savoy Worksop", "Savoy Worksop")
case object ShowcaseDeLuxNottingham extends Cinema("Showcase de Lux Nottingham", "Showcase de Lux Nottingham")
case object VueCinemasNottingham extends Cinema("Vue Cinemas Nottingham", "Vue Nottingham")
// ── United Kingdom · Oxfordshire (Flicks) ──
case object AbbeyCinemaAbingdon extends Cinema("Abbey Cinema Abingdon", "Abbey Abingdon")
case object CineworldDidcot extends Cinema("Cineworld Didcot", "Cineworld Didcot")
case object CineworldWitney extends Cinema("Cineworld Witney", "Cineworld Witney")
case object CornExchangeCinemaWallingford extends Cinema("Corn Exchange Cinema Wallingford", "Corn Exchange Wallingford")
case object CurzonCinemaOxford extends Cinema("Curzon Cinema Oxford", "Curzon Oxford")
case object PhoenixPicturehouseOxford extends Cinema("Phoenix Picturehouse Oxford", "Phoenix Picturehouse Oxford")
case object RegalPicturehouseHenley extends Cinema("Regal Picturehouse Henley", "Regal Picturehouse Henley")
case object TheLightBanbury extends Cinema("The Light Banbury", "The Light Banbury")
case object TheLivingRoomCinemaChippingNorton extends Cinema("The Living Room Cinema Chipping Norton", "The Living Room Chipping Norton")
case object TheOxfordCinemaCafe extends Cinema("The Oxford Cinema & Cafe", "The Oxford & Cafe")
case object UltimatePicturePalaceOxford extends Cinema("Ultimate Picture Palace Oxford", "Ultimate Picture Palace Oxford")
case object VueCinemasBicester extends Cinema("Vue Cinemas Bicester", "Vue Bicester")
case object VueCinemasOxford extends Cinema("Vue Cinemas Oxford", "Vue Oxford")
// ── United Kingdom · Powys (Flicks) ──
case object ColiseumCinemaBrecon extends Cinema("Coliseum Cinema Brecon", "Coliseum Brecon")
case object OdeonCinemaWrexham extends Cinema("Odeon Cinema Wrexham", "Odeon Wrexham")
case object WyesideArtsCentreBuilthWells extends Cinema("Wyeside Arts Centre Builth Wells", "Wyeside Arts Centre Builth Wells")
// ── United Kingdom · Renfrewshire (Flicks) ──
case object OdeonCinemaBraehead extends Cinema("Odeon Cinema Braehead", "Odeon Braehead")
case object ShowcaseDeLuxPaisley extends Cinema("Showcase de Lux Paisley", "Showcase de Lux Paisley")
case object TheTowerDigitalArtsCenterHelensburgh extends Cinema("The Tower Digital Arts Center Helensburgh", "The Tower Digital Arts Center Helensburgh")
case object WaterfrontGreenock extends Cinema("Waterfront Greenock", "Waterfront Greenock")
// ── United Kingdom · Roxburgh, Ettrick and Lauderdale (Flicks) ──
case object PavilionCinemaGalashiels extends Cinema("Pavilion Cinema Galashiels", "Pavilion Galashiels")
case object TowerMillHeartOfHawick extends Cinema("Tower Mill (Heart of Hawick)", "Tower Mill (Heart of Hawick)")
// ── United Kingdom · Sandwell (Flicks) ──
case object OdeonCinemaWestBromwich extends Cinema("Odeon Cinema West Bromwich", "Odeon West Bromwich")
// ── United Kingdom · Shropshire (Flicks) ──
case object AssemblyRoomsLudlow extends Cinema("Assembly Rooms Ludlow", "Assembly Rooms Ludlow")
case object CineworldShrewsbury extends Cinema("Cineworld Shrewsbury", "Cineworld Shrewsbury")
case object CineworldTelford extends Cinema("Cineworld Telford", "Cineworld Telford")
case object FestivalDraytonCentre extends Cinema("Festival Drayton Centre", "Festival Drayton Centre")
case object MaonaCinemaOswestry extends Cinema("Maona Cinema Oswestry", "Maona Oswestry")
case object OdeonLuxeTelford extends Cinema("Odeon Luxe Telford", "Odeon Luxe Telford")
case object OldMarketHallShrewsbury extends Cinema("Old Market Hall Shrewsbury", "Old Market Hall Shrewsbury")
case object ReelCinemaBridgnorthMajestic extends Cinema("Reel Cinema Bridgnorth (Majestic)", "Reel Bridgnorth (Majestic)")
case object WellingtonOrbit extends Cinema("Wellington Orbit", "Wellington Orbit")
// ── United Kingdom · Somerset (Flicks) ──
case object CineworldWestonSuperMare extends Cinema("Cineworld Weston-super-Mare", "Cineworld Weston-super-Mare")
case object CineworldYeovil extends Cinema("Cineworld Yeovil", "Cineworld Yeovil")
case object CurzonCinemaClevedon extends Cinema("Curzon Cinema Clevedon", "Curzon Clevedon")
case object EverymanBath extends Cinema("Everyman Bath", "Everyman Bath")
case object LittleTheatrePicturehouse extends Cinema("Little Theatre Picturehouse", "Little Theatre Picturehouse")
case object MerlinWellesleyWellington extends Cinema("Merlin Wellesley Wellington", "Merlin Wellesley Wellington")
case object OdeonCinemaBath extends Cinema("Odeon Cinema Bath", "Odeon Bath")
case object OdeonCinemaTaunton extends Cinema("Odeon Cinema Taunton", "Odeon Taunton")
case object PlazaCinemaWestonSuperMare extends Cinema("Plaza Cinema Weston-super-Mare", "Plaza Weston-super-Mare")
case object RitzBurnhamOnSea extends Cinema("Ritz Burnham-on-Sea", "Ritz Burnham-on-Sea")
case object ScottCinemasBridgwater extends Cinema("Scott Cinemas Bridgwater", "Scott Bridgwater")
case object TauntonBrewhouse extends Cinema("Taunton Brewhouse", "Taunton Brewhouse")
case object TheAvenueCinemaMinehead extends Cinema("The Avenue Cinema Minehead", "The Avenue Minehead")
case object TheWellsFilmCentre extends Cinema("The Wells Film Centre", "The Wells Film Centre")
case object WestwayCinemaFrome extends Cinema("Westway Cinema Frome", "Westway Frome")
// ── United Kingdom · South Yorkshire (Flicks) ──
case object ArcCinemaRotherham extends Cinema("Arc Cinema Rotherham", "Arc Rotherham")
case object CineworldBarnsley extends Cinema("Cineworld Barnsley", "Cineworld Barnsley")
case object ParkwayBarnsley extends Cinema("Parkway Barnsley", "Parkway Barnsley")
case object SavoyDoncaster extends Cinema("Savoy Doncaster", "Savoy Doncaster")
case object VueCinemasDoncaster extends Cinema("Vue Cinemas Doncaster", "Vue Doncaster")
// ── United Kingdom · Staffordshire (Flicks) ──
case object CannockCinema extends Cinema("Cannock Cinema", "Cannock")
case object CinebowlUttoxeter extends Cinema("Cinebowl Uttoxeter", "Cinebowl Uttoxeter")
case object CineworldBurtonOnTrent extends Cinema("Cineworld Burton-on-Trent", "Cineworld Burton-on-Trent")
case object CineworldStokeOnTrent extends Cinema("Cineworld Stoke-on-Trent", "Cineworld Stoke-on-Trent")
case object CineworldWolverhampton extends Cinema("Cineworld Wolverhampton", "Cineworld Wolverhampton")
case object FilmTheatreStokeOnTrent extends Cinema("Film Theatre Stoke-on-Trent", "Film Theatre Stoke-on-Trent")
case object LichfieldGarrickTheatreStudio extends Cinema("Lichfield Garrick Theatre & Studio", "Lichfield Garrick Theatre & Studio")
case object LockworksCinemaWolverhampton extends Cinema("Lockworks Cinema Wolverhampton", "Lockworks Wolverhampton")
case object OdeonCinemaStokeOnTrent extends Cinema("Odeon Cinema Stoke-on-Trent", "Odeon Stoke-on-Trent")
case object OdeonCinemaTamworth extends Cinema("Odeon Cinema Tamworth", "Odeon Tamworth")
case object OdeonLuxeStafford extends Cinema("Odeon Luxe Stafford", "Odeon Luxe Stafford")
case object RedCarpetBartonMarina extends Cinema("Red Carpet Barton Marina", "Red Carpet Barton Marina")
case object TheLightWalsall extends Cinema("The Light Walsall", "The Light Walsall")
case object VueCinemasNewcastleUnderLyme extends Cinema("Vue Cinemas Newcastle-under-Lyme", "Vue Newcastle-under-Lyme")
// ── United Kingdom · Suffolk (Flicks) ──
case object AbbeygateBuryStEdmunds extends Cinema("Abbeygate Bury St Edmunds", "Abbeygate Bury St Edmunds")
case object AldeburghCinema extends Cinema("Aldeburgh Cinema", "Aldeburgh")
case object CineworldBuryStEdmunds extends Cinema("Cineworld Bury St Edmunds", "Cineworld Bury St Edmunds")
case object CineworldHaverhill extends Cinema("Cineworld Haverhill", "Cineworld Haverhill")
case object CineworldIpswich extends Cinema("Cineworld Ipswich", "Cineworld Ipswich")
case object ElectricPicturePalaceSouthwold extends Cinema("Electric Picture Palace Southwold", "Electric Picture Palace Southwold")
case object EverymanBuryStEdmunds extends Cinema("Everyman Bury St Edmunds", "Everyman Bury St Edmunds")
case object FilmTheatreLeiston extends Cinema("Film Theatre Leiston", "Film Theatre Leiston")
case object HaverhillArtsCentre extends Cinema("Haverhill Arts Centre", "Haverhill Arts Centre")
case object KingStreetCinema extends Cinema("King Street Cinema", "King Street")
case object KingsCinemaNewmarket extends Cinema("Kings Cinema Newmarket", "Kings Newmarket")
case object OmniplexIpswichFormerlyEmpire extends Cinema("Omniplex Ipswich (formerly Empire)", "Omniplex Ipswich (formerly Empire)")
case object PalaceCinemaFelixstowe extends Cinema("Palace Cinema Felixstowe", "Palace Felixstowe")
case object RegalStowmarket extends Cinema("Regal Stowmarket", "Regal Stowmarket")
case object RiversideTheatreWoodbridge extends Cinema("Riverside Theatre Woodbridge", "Riverside Theatre Woodbridge")
// ── United Kingdom · Surrey (Flicks) ──
case object ChiddingfoldVillageHallCinema extends Cinema("Chiddingfold Village Hall Cinema", "Chiddingfold Village Hall")
case object CineworldAldershot extends Cinema("Cineworld Aldershot", "Cineworld Aldershot")
case object EverymanCinemaOxted extends Cinema("Everyman Cinema Oxted", "Everyman Oxted")
case object EverymanCinemaReigate extends Cinema("Everyman Cinema Reigate", "Everyman Reigate")
case object HaslemereHallCinema extends Cinema("Haslemere Hall Cinema", "Haslemere Hall")
case object OdeonCinemaGuildford extends Cinema("Odeon Cinema Guildford", "Odeon Guildford")
case object ReelCinemasFarnham extends Cinema("Reel Cinemas Farnham", "Reel Farnham")
case object TheLightRedhill extends Cinema("The Light Redhill", "The Light Redhill")
case object VueCinemasCamberley extends Cinema("Vue Cinemas Camberley", "Vue Camberley")
case object VueCinemasFarnborough extends Cinema("Vue Cinemas Farnborough", "Vue Farnborough")
// ── United Kingdom · Tayside (Flicks) ──
case object BirksAberfeldy extends Cinema("Birks Aberfeldy", "Birks Aberfeldy")
case object ChalmersFilmhouseArbroath extends Cinema("Chalmers Filmhouse Arbroath", "Chalmers Filmhouse Arbroath")
case object CineworldDundee extends Cinema("Cineworld Dundee", "Cineworld Dundee")
case object DundeeContemporaryArtsDCA extends Cinema("Dundee Contemporary Arts (DCA)", "Dundee Contemporary Arts (DCA)")
case object NewPictureHouseStAndrews extends Cinema("New Picture House St Andrews", "New Picture House St Andrews")
case object OdeonLuxeDundee extends Cinema("Odeon Luxe Dundee", "Odeon Luxe Dundee")
case object PlayhouseCinemaPerth extends Cinema("Playhouse Cinema Perth", "Playhouse Perth")
case object TheMontrosePlayhouse extends Cinema("The Montrose Playhouse", "The Montrose Playhouse")
// ── United Kingdom · Tyne and Wear (Flicks) ──
case object CineworldBoldonTyneWear extends Cinema("Cineworld Boldon Tyne & Wear", "Cineworld Boldon Tyne & Wear")
case object CineworldNewcastle extends Cinema("Cineworld Newcastle", "Cineworld Newcastle")
case object CustomsHouseCinemaSouthShields extends Cinema("Customs House Cinema, South Shields", "Customs House , South Shields")
case object EverymanCinemaNewcastle extends Cinema("Everyman Cinema Newcastle", "Everyman Newcastle")
case object JamJarCinema extends Cinema("Jam Jar Cinema", "Jam Jar")
case object OdeonCinemaMetrocentre extends Cinema("Odeon Cinema Metrocentre", "Odeon Metrocentre")
case object OdeonCinemaSilverlink extends Cinema("Odeon Cinema Silverlink", "Odeon Silverlink")
case object OmniplexSunderlandFormerlyEmpire extends Cinema("Omniplex Sunderland (formerly Empire)", "Omniplex Sunderland (formerly Empire)")
case object StarAndShadowCinemaNewcastle extends Cinema("Star and Shadow Cinema Newcastle", "Star and Shadow Newcastle")
case object TynesideNewcastle extends Cinema("Tyneside Newcastle", "Tyneside Newcastle")
case object VueCinemasGateshead extends Cinema("Vue Cinemas Gateshead", "Vue Gateshead")
// ── United Kingdom · Tyrone (Flicks) ──
case object OmniplexDungannon extends Cinema("Omniplex Dungannon", "Omniplex Dungannon")
case object OmniplexOmagh extends Cinema("Omniplex Omagh", "Omniplex Omagh")
case object RitzMultiplexCookstown extends Cinema("Ritz Multiplex Cookstown", "Ritz Multiplex Cookstown")
// ── United Kingdom · Warwickshire (Flicks) ──
case object CineworldRugby extends Cinema("Cineworld Rugby", "Cineworld Rugby")
case object EverymanCinemaStratfordUponAvon extends Cinema("Everyman Cinema Stratford-upon-Avon", "Everyman Stratford-upon-Avon")
case object OdeonCinemaCoventry extends Cinema("Odeon Cinema Coventry", "Odeon Coventry")
case object OdeonLuxeNuneaton extends Cinema("Odeon Luxe Nuneaton", "Odeon Luxe Nuneaton")
case object RoyalSpaCentre extends Cinema("Royal Spa Centre", "Royal Spa Centre")
case object ShowcaseDeLuxCoventry extends Cinema("Showcase de Lux Coventry", "Showcase de Lux Coventry")
case object VueCinemasLeamingtonSpa extends Cinema("Vue Cinemas Leamington Spa", "Vue Leamington Spa")
case object WarwickArtsCentreCoventry extends Cinema("Warwick Arts Centre Coventry", "Warwick Arts Centre Coventry")
// ── United Kingdom · West Sussex (Flicks) ──
case object AtriumEastGrinstead extends Cinema("Atrium East Grinstead", "Atrium East Grinstead")
case object CapitolHorsham extends Cinema("Capitol Horsham", "Capitol Horsham")
case object CineworldCrawley extends Cinema("Cineworld Crawley", "Cineworld Crawley")
case object ConnaughtTheatreStudioWorthing extends Cinema("Connaught Theatre & Studio Worthing", "Connaught Theatre & Studio Worthing")
case object DomeWorthing extends Cinema("Dome Worthing", "Dome Worthing")
case object EverymanCinemaHorsham extends Cinema("Everyman Cinema Horsham", "Everyman Horsham")
case object OrionBurgessHill extends Cinema("Orion Burgess Hill", "Orion Burgess Hill")
case object PicturedromeBognorRegis extends Cinema("Picturedrome Bognor Regis", "Picturedrome Bognor Regis")
case object WindmillLittlehampton extends Cinema("Windmill Littlehampton", "Windmill Littlehampton")
// ── United Kingdom · West Yorkshire (Flicks) ──
case object CineworldBradford extends Cinema("Cineworld Bradford", "Cineworld Bradford")
case object CineworldLeeds extends Cinema("Cineworld Leeds", "Cineworld Leeds")
case object CineworldWakefield extends Cinema("Cineworld Wakefield", "Cineworld Wakefield")
case object CottageRoadCinemaLeeds extends Cinema("Cottage Road Cinema Leeds", "Cottage Road Leeds")
case object EverymanCinemaLeeds extends Cinema("Everyman Cinema Leeds", "Everyman Leeds")
case object HeartCentreHeadingley extends Cinema("Heart Centre Headingley", "Heart Centre Headingley")
case object HebdenBridgePicturehouse extends Cinema("Hebden Bridge Picturehouse", "Hebden Bridge Picturehouse")
case object HydeParkPictureHouse extends Cinema("Hyde Park Picture House", "Hyde Park Picture House")
case object IlkleyCinema extends Cinema("Ilkley Cinema", "Ilkley")
case object OdeonCinemaHuddersfield extends Cinema("Odeon Cinema Huddersfield", "Odeon Huddersfield")
case object OdeonLuxeLeedsThorpePark extends Cinema("Odeon Luxe Leeds (Thorpe Park)", "Odeon Luxe Leeds (Thorpe Park)")
case object OdeonLuxeLeedsBradford extends Cinema("Odeon Luxe Leeds-Bradford", "Odeon Luxe Leeds-Bradford")
case object PictureHouseKeighley extends Cinema("Picture House Keighley", "Picture House Keighley")
case object PicturevilleScienceAndMediaMuseumBradford extends Cinema("Pictureville (Science and Media Museum Bradford)", "Pictureville (Science and Media Museum Bradford)")
case object PlazaSkipton extends Cinema("Plaza Skipton", "Plaza Skipton")
case object ReelCinemaWakefield extends Cinema("Reel Cinema Wakefield", "Reel Wakefield")
case object RexElland extends Cinema("Rex Elland", "Rex Elland")
case object ShowcaseDeLuxLeeds extends Cinema("Showcase de Lux Leeds", "Showcase de Lux Leeds")
case object TheLightBradford extends Cinema("The Light Bradford", "The Light Bradford")
case object VueCinemasCastleford extends Cinema("Vue Cinemas Castleford", "Vue Castleford")
case object VueCinemasHalifax extends Cinema("Vue Cinemas Halifax", "Vue Halifax")
case object VueCinemasLeedsKirkstallRoad extends Cinema("Vue Cinemas Leeds (Kirkstall Road)", "Vue Leeds (Kirkstall Road)")
case object VueCinemasLeedsTheLight extends Cinema("Vue Cinemas Leeds (The Light)", "Vue Leeds (The Light)")
case object WetherbyFilmTheatre extends Cinema("Wetherby Film Theatre", "Wetherby Film Theatre")
// ── United Kingdom · Wiltshire (Flicks) ──
case object CineworldShawRidgeSwindon extends Cinema("Cineworld (Shaw Ridge) Swindon", "Cineworld (Shaw Ridge) Swindon")
case object EverymanCinemaSalisbury extends Cinema("Everyman Cinema Salisbury", "Everyman Salisbury")
case object OdeonCinemaAndover extends Cinema("Odeon Cinema Andover", "Odeon Andover")
case object OdeonCinemaSalisbury extends Cinema("Odeon Cinema Salisbury", "Odeon Salisbury")
case object OdeonCinemaTrowbridge extends Cinema("Odeon Cinema Trowbridge", "Odeon Trowbridge")
case object PalaceCinemaDevizes extends Cinema("Palace Cinema Devizes", "Palace Devizes")
case object ReelCinemaChippenhamAstoria extends Cinema("Reel Cinema Chippenham (Astoria)", "Reel Chippenham (Astoria)")
case object RegalCinemaFordingbridge extends Cinema("Regal Cinema Fordingbridge", "Regal Fordingbridge")
case object TheParadeCinemaMarlborough extends Cinema("The Parade Cinema Marlborough", "The Parade Marlborough")
case object VueCinemasSwindon extends Cinema("Vue Cinemas Swindon", "Vue Swindon")
// ── United Kingdom · Worcestershire (Flicks) ──
case object CastlemortonCinemaMortonMajestic extends Cinema("Castlemorton Cinema (Morton Majestic)", "Castlemorton (Morton Majestic)")
case object FuturistCinema extends Cinema("Futurist Cinema", "Futurist")
case object MalvernTheatres extends Cinema("Malvern Theatres", "Malvern Theatres")
case object Number8Pershore extends Cinema("Number 8 Pershore", "Number 8 Pershore")
case object OdeonCinemaWorcester extends Cinema("Odeon Cinema Worcester", "Odeon Worcester")
case object RegalCinemaEvesham extends Cinema("Regal Cinema Evesham", "Regal Evesham")
case object RegalCinemaTenburyWells extends Cinema("Regal Cinema Tenbury Wells", "Regal Tenbury Wells")
case object VueCinemasRedditch extends Cinema("Vue Cinemas Redditch", "Vue Redditch")
case object VueCinemasWorcester extends Cinema("Vue Cinemas Worcester", "Vue Worcester")
// ── United Kingdom · Yorkshire (Flicks) ──
case object CineworldSheffield extends Cinema("Cineworld Sheffield", "Cineworld Sheffield")
case object OdeonLuxeSheffield extends Cinema("Odeon Luxe Sheffield", "Odeon Luxe Sheffield")
case object ParamountPenistone extends Cinema("Paramount Penistone", "Paramount Penistone")
case object ShowroomSheffield extends Cinema("Showroom Sheffield", "Showroom Sheffield")
case object TheLightSheffield extends Cinema("The Light Sheffield", "The Light Sheffield")
case object VueCinemasSheffield extends Cinema("Vue Cinemas Sheffield", "Vue Sheffield")


// ── Germany (Filmstarts) — data-driven from the full roster ──────────────────
// `GermanRosterData` (generated from data/germany/regions.json) carries 158
// regions / 1,533 cinemas. Each venue is a `GermanCinema` instance built ONCE in
// `GermanRoster` and reused everywhere (the region's cinemas, `Cinema.byCity`, and
// the scrape catalog) — so the same `Source` instance is used throughout and
// identity equality holds like the hand-authored `case object` cinemas did.
final class GermanCinema(displayName: String, pillName: String) extends Cinema(displayName, pillName)

/** Materialises the generated German roster into `GermanRegion` cities +
 *  `GermanCinema` venues, once. Exposes the regions for `City.germanCities`, the
 *  by-display-name grouping for `Cinema.byCity`, and each cinema's Filmstarts
 *  `theaterId` for the scrape catalog. */
object GermanRoster {
  private val built: Seq[(GermanRegion, Seq[(GermanCinema, String)])] =
    GermanRosterData.regions.map { case (slug, name, lat, lon, cinemas) =>
      val venues = cinemas.map { case (disp, pill, tid) => (new GermanCinema(disp, pill), tid) }
      (new GermanRegion(slug, CityLabels(name, name, name), lat, lon, venues.map(_._1)), venues)
    }
  val regions: Seq[GermanRegion]             = built.map(_._1)
  val byCity:  Seq[(String, Seq[Cinema])]    = built.map { case (r, v) => r.labels.nominative -> v.map(_._1) }
  val theaterIdByCinema: Map[Cinema, String] = built.flatMap(_._2).toMap
}


object Cinema {
  /** Poznań venues — the original ten. Their display order doubles as the
   *  per-source merge priority (see `Source.all`), so Multikino stays in the
   *  list and existing rows keep their resolved precedence. */
  val poznan: Seq[Cinema] = Seq(
    KinoApollo,
    KinoBulgarska,
    CharlieMonroe,
    Helios,
    CinemaCityKinepolis,
    KinoMuza,
    Multikino,
    KinoPalacowe,
    CinemaCityPoznanPlaza,
    Rialto,
  )

  val wroclaw: Seq[Cinema] = Seq(CinemaCityWroclavia, CinemaCityKorona, MultikinoPasazGrunwaldzki, HeliosMagnolia, HeliosAlejaBielany, KinoNoweHoryzonty, DolnoslaskieCentrumFilmowe, KinoAstra, KinoDyskusyjnyKlubFilmowyPolitechnika)

  val warszawa: Seq[Cinema] = Seq(CinemaCityArkadia, CinemaCityBemowo, CinemaCityGaleriaPolnocna, CinemaCityJanki, CinemaCityMokotow, CinemaCityPromenada, CinemaCitySadyba, MultikinoZloteTarasy, MultikinoMlociny, MultikinoReduta, MultikinoTargowek, MultikinoWolaPark, HeliosBlueCity, KinoMuranow, KinoLuna, KinoElektronik, KinoIluzjon, KinoGram, KinoKultura, KinoAmondo, KinoNaBoku, KinoGlebocka66, Kinomuzeum, KinoSwit, KinoKepa, StacjaFalenica, SluzewskiDomKultury, KinoAtlantic, Kinoteka, Ujazdowski, KinoCytadela, KinoWisla, AdaKinoStudyjne, KinoMazowieckiTeatrMuzycznyImJanaKiepuryKinoPraha, KinoAlternatywy)

  /** Kraków venues. Cinema City has three multiplexes here (Bonarka,
   *  Kazimierz, Zakopianka — Zakopianka also houses the city's only IMAX);
   *  Multikino has one. No Helios in Kraków. The independents (Pod Baranami,
   *  Kijów, Mikro, …) are wired separately as bespoke scrapers. */
  val krakow: Seq[Cinema] = Seq(
    CinemaCityBonarka,
    CinemaCityKazimierz,
    CinemaCityZakopianka,
    MultikinoKrakow,
    KinoMikro,
    MikroBronowice,
    KinoSfinks,
    KinoPodBaranami,
    KinoKijow,
    KinoKika,
    KinoAgrafka,
    KinoParadox,
  )

  /** Łódź venues. Three national chains — Cinema City (Manufaktura), Multikino,
   *  and the city's single Helios (in Galeria Sukcesja) — plus Kino Charlie, the
   *  arthouse cinema on Piotrkowska, wired as a bespoke ld+json scraper. */
  val lodz: Seq[Cinema] = Seq(CinemaCityManufaktura, MultikinoLodz, HeliosLodz, KinoCharlie, KinematografLodz, Nckf, KinoTatry, KinoSpojnia, KinoStaryMlyn)

  /** Katowice venues. Cinema City has two locations (Punkt 44 and Silesia),
   *  Multikino one, Helios one. The three art-house venues run by Silesia Film —
   *  Kosmos, Światowid, and Kinoteatr Rialto — are all Bilety24-hosted. */
  val katowice: Seq[Cinema] = Seq(CinemaCityPunkt44, CinemaCitySilesia, MultikinoKatowice, HeliosKatowice, KinoKosmos, KinoSwiatowid, KinoteatrRialto, CinemaCity, KinoPatria)

  /** Szczecin venues. No Cinema City here; the two multiplexes are Helios (in
   *  CH Kupiec) and Multikino. The lone independent is Kino Pionier 1907 — the
   *  oldest continuously-operating cinema in the world — wired as a bespoke
   *  scraper. */
  val szczecin: Seq[Cinema] = Seq(HeliosSzczecin, MultikinoSzczecin, KinoPionier, HeliosOutletPark, KinoZamekSzczecin, KinoKawiarnia, KinoPDK, KinoSCK)

  /** Białystok venues. No Cinema City or Multikino in the city; Helios (HQ'd in
   *  Łódź, strong in eastern Poland) runs three multiplexes — Alfa, Biała and
   *  Jurowiecka. The independent is Kino Forum, the film screen of the
   *  Białostocki Ośrodek Kultury, wired as a bespoke scraper. */
  val bialystok: Seq[Cinema] = Seq(HeliosAlfa, HeliosBiala, HeliosJurowiecka, KinoForum, KinoSokolSokolka)

  /** Trójmiasto venues — the Tri-City of Gdańsk, Gdynia and Sopot treated as one
   *  repertoire. Two Helios in Gdańsk (Metropolia, Forum), one in Gdynia
   *  (Riviera), plus Multikino Gdańsk. Cinema City Krewetka closed in 2018 and
   *  Multikino Sopot in 2026, so neither is modelled. The independents (Cinema1,
   *  GCF, …) are wired separately as bespoke scrapers. */
  val trojmiasto: Seq[Cinema] = Seq(MultikinoGdansk, HeliosMetropolia, HeliosForum, HeliosRiviera, KinoSpektrum, KinoKameralne, KinoIkm, KinoMuzeumGdansk, KinoZak, KinoPort, Cinema1Gdansk, GdynskieCentrumFilmowe, KinoNaSzekspirowskim, MultikinoRumia)

  /** Bydgoszcz venues. All three national chains have one multiplex each
   *  (Cinema City in Focus Mall, Multikino on Focha, Helios on Fordońska);
   *  the city's one art-house screen is MCK's Kino Orzeł, ticketed through the
   *  national Bilety24 marketplace. */
  val bydgoszcz: Seq[Cinema] = Seq(CinemaCityBydgoszcz, MultikinoBydgoszcz, HeliosBydgoszcz, KinoOrzel, KinoKinomax, KinoRondo)

  /** Lublin venues. Two Cinema City multiplexes (Felicity, Lublin Plaza) and
   *  Multikino in Galeria Olimp; no Helios in Lublin. Kino Bajka — the Centrum
   *  Kultury art-house on Radziszewskiego — is the local studyjne screen, wired
   *  separately as a bespoke scraper. */
  val lublin: Seq[Cinema] = Seq(CinemaCityLublinFelicity, CinemaCityLublinPlaza, MultikinoLublin, KinoBajka, KinoCkLublin, KinoChatkaZaka, KinoLewart, KinoMetalowiec)

  val czestochowa: Seq[Cinema] = Seq(CinemaCityCzestochowaJurajska, CinemaCityCzestochowaWolnosc, OkfIluzja, KinoDKFRumcajs, KinoKarolinka, KinoMDK, KinoMOKCentrum, KinoZacisze)

  val radom: Seq[Cinema] = Seq(HeliosRadom, MultikinoRadom, McswElektrowniaCinema, HeliosStarachowice, KinoCentrumSkarzyskoKamienna, KinoGornik, KinoKozienickiDomKultury, KinoKuznica, KinoSwitZwolen)

  val sosnowiec: Seq[Cinema] = Seq(HeliosSosnowiec, CinemaCitySosnowiec)

  val torun: Seq[Cinema] = Seq(CinemaCityTorunCzerwonaDroga, CinemaCityTorunPlaza, KinoCentrumCsw, KinoMiejskieCentrumKultury, KinoZdroj)

  val kielce: Seq[Cinema] = Seq(HeliosKielce, MultikinoKielce, KinoFenomen, KinoMoskwa, KinoCK, KinoKoneckieCentrumKultury)

  val rzeszow: Seq[Cinema] = Seq(HeliosRzeszow, MultikinoRzeszow, KinoZorza, KinoZaRogiemCafe, HeliosKrosno, KinoArtKino, KinoJednosc, KinoMCK, KinoSniezka, KinoSokolBrzozow, KinoWarszawa)

  val gliwice: Seq[Cinema] = Seq(CinemaCityGliwice, KinoAmok, KinoScenaKultura)

  val zabrze: Seq[Cinema] = Seq(MultikinoZabrze, KinoRoma)

  // New chains-first cities. Each holds only its national-chain branches for now;
  // a local independent screen, where one exists, joins later as a bespoke scraper.
  val olsztyn: Seq[Cinema]      = Seq(HeliosOlsztyn, MultikinoOlsztyn, KinoAwangarda2, KinoCinemaLumiere, KinoIgnacy, KinoNarie)
  val bielskoBiala: Seq[Cinema] = Seq(HeliosBielskoBiala, CinemaCityBielskoBiala, KinoKreska, KinoJanosik, KinoPckulKino, KinoSwitCzechowiceDziedzice, KinoTeatrElektryczny, KinoWislaBrzeszcze, MultikinoCzechowiceDziedzice)
  val opole: Seq[Cinema]        = Seq(HeliosOpoleKarolinka, HeliosOpoleSolaris, KinoMeduza, HeliosKedzierzynKozle, KinoBajkaKluczbork, KinoChemik, KinoDiana, KinoKrapkowice, KinoStudio, KinoTwierdza)
  val rybnik: Seq[Cinema]       = Seq(MultikinoRybnik, CinemaCityRybnik, HeliosZory, KinoBaltyk, KinoCentrum, KinoNaStarowce, KinoPegaz, KinoTeatrZiemiRybnickiej)
  val gorzow: Seq[Cinema]       = Seq(HeliosGorzow, MultikinoGorzow, Kino60Krzesel)
  val elblag: Seq[Cinema]       = Seq(MultikinoElblag, CinemaCityElblag, HeliosTczew, KinoBaszta, KinoPowisle, KinoZulawskiOsrodekKultury)
  val koszalin: Seq[Cinema]     = Seq(HeliosKoszalin, MultikinoKoszalin, KinoKryterium, KinoBajkaDarlowo, KinoCentrumBialogard, KinoDK, KinoGOK, KinoGoplana, KinoWybrzeze)
  val kalisz: Seq[Cinema]       = Seq(HeliosKalisz, MultikinoKalisz, HeliosOstrowWlkp, KinoCentrum3D, KinoEcho, KinoPiastOstrzeszow, KinoPrzedwiosnieKrotoszyn)
  val zielonaGora: Seq[Cinema]  = Seq(CinemaCityZielonaGora, KinoEuropa, KinoMaxKino, KinoPionierZary, KinoSDKSwiebodzin)
  val tychy: Seq[Cinema]        = Seq(MultikinoTychy, KinoNaszeKino, KinoPlanetCinema)
  val walbrzych: Seq[Cinema]    = Seq(CinemaCityWalbrzych, KinoApolloWalbrzych, KinoMOKNowaRuda, KinoMOKiS, KinoSleza, KinoZbyszek, MultikinoKlodzko, MultikinoSwidnica)
  val tarnow: Seq[Cinema]       = Seq(MultikinoTarnow, KinoMillenium, KinoFarys, KinoGCK, KinoKolory, KinoPlaneta, KinoPromien, KinoRegis, KinoSokolDabrowaTarnowska)
  val wloclawek: Seq[Cinema]    = Seq(MultikinoWloclawek, KinoJutrzenka, KinoNawojka, KinoNoweKinoWarszawa, KinoZaRogiem)
  val legnica: Seq[Cinema]      = Seq(HeliosLegnica, KinoPiast, HeliosLubin, KinoAurum, KinoCyfroweKino, KinoForumBoleslawiec, KinoMuzaLubin, KinoPCA)
  val plock: Seq[Cinema]        = Seq(HeliosPlock, KinoPrzedwiosnie, KinoKDK, KinoKalejdoskop, KinoODEON)
  val bytom: Seq[Cinema]        = Seq(CinemaCityBytom)
  val dabrowaGornicza: Seq[Cinema] = Seq(HeliosDabrowaGornicza, KinoKadr)
  val nowySacz: Seq[Cinema]     = Seq(HeliosNowySacz, KinoSokol, KinoJaworzyna, KinoKlaps)
  val slupsk: Seq[Cinema]       = Seq(MultikinoSlupsk, KinoRejs, KinoFregata)
  val jeleniaGora: Seq[Cinema]  = Seq(HeliosJeleniaGora, KinoLot, KinoWawel)
  val przemysl: Seq[Cinema]     = Seq(HeliosPrzemysl, KinoCentrum3DPrzemysl, KinoIkar, KinoNaBiegunach, KinoSDK)
  val konin: Seq[Cinema]        = Seq(HeliosKonin, KinoOskard, KinoZacheta, KinoNadWarta, KinoHel, KinoSokolnia, KinoTur, KinoMok)

  // ── United Kingdom (Flicks) ──
  val london: Seq[Cinema] = Seq(ActOneActon, ArthouseCrouchEnd, BarbicanLondonCinema1, BfiLondonImax, BfiLondonSouthbank, CastleCinemaHackney, SidcupStoryteller, ChiswickCinema, CineworldGreenwich, CineworldBexleyheath, CineworldEnfield, CineworldFeltham, CineworldIlford, CineworldLeicesterSquare, CineworldLondonHounslow, CineworldSouthRuislip, CineworldWandsworth, CineworldWembley, CineworldWestIndiaQuay, CineworldWoodGreen, CineLumiereLondon, CloseUpFilmCentreShoreditch, CrouchEndPicturehouse, CurzonCinemaAldgate, CurzonCinemaBloomsbury, CurzonCinemaCamden, CurzonCinemaHoxton, CurzonCinemaKingston, CurzonCinemaMayfair, CurzonCinemaRichmond, CurzonCinemaSeaContainersMondrian, CurzonCinemaVictoria, CurzonSoho, CurzonWimbledon, DavidLeanCinemaCroydon, ElectricCinemaLondon, ElectricCinemaWhiteCity, EverymanAtTheWhiteleyLondon, EverymanBrentford, EverymanCinemaBakerStreet, EverymanCinemaBarnet, EverymanCinemaBelsizeParkHampstead, EverymanCinemaBoroughYards, EverymanCinemaBroadgate, EverymanCinemaCanaryWharf, EverymanCinemaChelsea, EverymanCinemaCrystalPalace, EverymanCinemaEgham, EverymanCinemaEsher, EverymanCinemaHampstead, EverymanCinemaKingSCross, EverymanCinemaMaidaVale, EverymanCinemaMuswellHill, EverymanCinemaStratfordInternational, EverymanCinemaWaltonOnThames, EverymanCinemaIslington, FinsburyParkPicturehouse, ForestCinemasWalthamstow, GenesisTowerHamlets, InstituteOfContemporaryArts, Jw3Hampstead, KilnKilburn, LeatherheadTheatreCinemaLeatherhead, LexiKensalRise, LumiereRomford, NovaCinemaWoking, OdeonCinemaActon, OdeonCinemaBeckenham, OdeonCinemaEpsom, OdeonCinemaGreenwich, OdeonCinemaHolloway, OdeonCinemaKingston, OdeonCinemaOrpington, OdeonCinemaRichmond, OdeonCinemaSouthWoodford, OdeonCinemaStreatham, OdeonCinemaTottenhamCourtRoad, OdeonCinemaUxbridge, OdeonCinemaWimbledon, OdeonCinemaLuxeHaymarket, OdeonLuxeIslington, OdeonLuxeLeeValley, OdeonCinemaLuxeLeicesterSquare, OdeonCinemaLuxePutney, OdeonLuxeSwissCottage, OdeonLuxeWestEnd, OlympicCinemaBarnes, EmpireCinemaSutton, Peckhamplex, PhoenixCinemaEastFinchley, PicturehouseCentralLondon, PicturehouseClapham, PicturehouseEalingFilmworks, PicturehouseEastDulwich, PicturehouseEpsomSquare, PicturehouseGreenwich, PicturehouseHackney, PicturehouseWestNorwood, PrinceCharlesLondon, RegentStreetCinemaLondon, RichMixBethnalGreen, RioDalston, RiversideStudiosHammersmith, RooftopFilmClubPeckhamBusseyBuilding, RooftopFilmClubStratfordRoofEast, ScienceMuseumLondonImax, ArchlightCinemas, TheArzner, TheCinemaAtSelfridges, TheCinemaInThePowerStation, TheGardenCinema, TheGatePicturehouseLondon, TheLightCinemasAddlestone, TheNickelLondon, TheRitzyPicturehouseBrixton, VueCinemasBromley, VueCinemasDagenham, VueCinemasEltham, VueCinemasFinchleyRoadSwissCottage, VueCinemasFulham, VueCinemasHarrow, VueCinemasIslington, VueCinemasFinchley, VueCinemasPiccadillyCircus, VueCinemasPurleyWayCroydon, VueCinemasRomford, VueCinemasStainesUponThames, VueCinemasStratford, VueCinemasWestEnd, VueCinemasWestfieldShepherdSBush, VueCinemasWoodGreen, WatermansArtCentreBrentford, WyllyottsTheatrePottersBar)
  private val londonAreaOf: Map[Cinema, CinemaArea] = {
    import CinemaArea.*
    Map(
      // ── Central ──
      BfiLondonImax -> Central, BfiLondonSouthbank -> Central, CineworldLeicesterSquare -> Central, CineLumiereLondon -> Central, CurzonCinemaBloomsbury -> Central, CurzonCinemaCamden -> Central, CurzonCinemaSeaContainersMondrian -> Central, CurzonCinemaVictoria -> Central, CurzonSoho -> Central, EverymanCinemaBakerStreet -> Central, EverymanCinemaBroadgate -> Central, EverymanCinemaChelsea -> Central, EverymanCinemaKingSCross -> Central, EverymanCinemaIslington -> Central, InstituteOfContemporaryArts -> Central, OdeonCinemaTottenhamCourtRoad -> Central, OdeonCinemaLuxeHaymarket -> Central, OdeonLuxeIslington -> Central, OdeonCinemaLuxeLeicesterSquare -> Central, OdeonLuxeWestEnd -> Central, PrinceCharlesLondon -> Central, RegentStreetCinemaLondon -> Central, ScienceMuseumLondonImax -> Central, TheCinemaAtSelfridges -> Central, TheGardenCinema -> Central, TheNickelLondon -> Central, VueCinemasIslington -> Central, VueCinemasPiccadillyCircus -> Central, VueCinemasWestEnd -> Central,
      // ── North ──
      ArthouseCrouchEnd -> North, CineworldEnfield -> North, CineworldIlford -> North, CineworldWoodGreen -> North, CrouchEndPicturehouse -> North, EverymanCinemaBarnet -> North, EverymanCinemaBelsizeParkHampstead -> North, EverymanCinemaHampstead -> North, EverymanCinemaMuswellHill -> North, FinsburyParkPicturehouse -> North, Jw3Hampstead -> North, KilnKilburn -> North, LexiKensalRise -> North, LumiereRomford -> North, OdeonCinemaHolloway -> North, OdeonLuxeLeeValley -> North, OdeonLuxeSwissCottage -> North, PhoenixCinemaEastFinchley -> North, VueCinemasDagenham -> North, VueCinemasFinchleyRoadSwissCottage -> North, VueCinemasFinchley -> North, VueCinemasRomford -> North, VueCinemasWoodGreen -> North, WyllyottsTheatrePottersBar -> North,
      // ── East ──
      BarbicanLondonCinema1 -> East, CastleCinemaHackney -> East, CineworldWestIndiaQuay -> East, CloseUpFilmCentreShoreditch -> East, CurzonCinemaAldgate -> East, CurzonCinemaHoxton -> East, EverymanCinemaCanaryWharf -> East, EverymanCinemaStratfordInternational -> East, ForestCinemasWalthamstow -> East, GenesisTowerHamlets -> East, OdeonCinemaSouthWoodford -> East, PicturehouseHackney -> East, RichMixBethnalGreen -> East, RioDalston -> East, RooftopFilmClubStratfordRoofEast -> East, VueCinemasStratford -> East,
      // ── South ──
      SidcupStoryteller -> South, CineworldGreenwich -> South, CineworldBexleyheath -> South, CineworldFeltham -> South, CineworldLondonHounslow -> South, CineworldWandsworth -> South, CurzonCinemaKingston -> South, CurzonCinemaRichmond -> South, CurzonWimbledon -> South, DavidLeanCinemaCroydon -> South, EverymanCinemaBoroughYards -> South, EverymanCinemaCrystalPalace -> South, EverymanCinemaEgham -> South, EverymanCinemaEsher -> South, EverymanCinemaWaltonOnThames -> South, LeatherheadTheatreCinemaLeatherhead -> South, NovaCinemaWoking -> South, OdeonCinemaBeckenham -> South, OdeonCinemaEpsom -> South, OdeonCinemaGreenwich -> South, OdeonCinemaKingston -> South, OdeonCinemaOrpington -> South, OdeonCinemaRichmond -> South, OdeonCinemaStreatham -> South, OdeonCinemaWimbledon -> South, OdeonCinemaLuxePutney -> South, OlympicCinemaBarnes -> South, EmpireCinemaSutton -> South, Peckhamplex -> South, PicturehouseClapham -> South, PicturehouseEastDulwich -> South, PicturehouseEpsomSquare -> South, PicturehouseGreenwich -> South, PicturehouseWestNorwood -> South, RooftopFilmClubPeckhamBusseyBuilding -> South, ArchlightCinemas -> South, TheArzner -> South, TheCinemaInThePowerStation -> South, TheLightCinemasAddlestone -> South, TheRitzyPicturehouseBrixton -> South, VueCinemasBromley -> South, VueCinemasEltham -> South, VueCinemasFulham -> South, VueCinemasPurleyWayCroydon -> South, VueCinemasStainesUponThames -> South,
      // ── West ──
      ActOneActon -> West, ChiswickCinema -> West, CineworldSouthRuislip -> West, CineworldWembley -> West, CurzonCinemaMayfair -> West, ElectricCinemaLondon -> West, ElectricCinemaWhiteCity -> West, EverymanAtTheWhiteleyLondon -> West, EverymanBrentford -> West, EverymanCinemaMaidaVale -> West, OdeonCinemaActon -> West, OdeonCinemaUxbridge -> West, PicturehouseCentralLondon -> West, PicturehouseEalingFilmworks -> West, RiversideStudiosHammersmith -> West, TheGatePicturehouseLondon -> West, VueCinemasHarrow -> West, VueCinemasWestfieldShepherdSBush -> West, WatermansArtCentreBrentford -> West,
    )
  }
  val londonAreas: Seq[CinemaAreaGroup] =
    CinemaArea.values.toSeq
      .map(area => CinemaAreaGroup(area, london.filter(londonAreaOf.get(_).contains(area))))
      .filter(_.cinemas.nonEmpty)
  val manchester: Seq[Cinema] = Seq(CineworldAshtonUnderLyne, CineworldManchester, CultplexManchester, EverymanManchesterStJohns, FlixTreehouseManchester, HomeManchester, LeighFilmFactory, NorthernLightSale, OdeonCinemaManchesterGreatNorthern, OdeonCinemaManchesterTraffordCentre, OdeonCinemaOldham, EmpireCinemaWigan, PlazaStockport, ReelCinemaRochdale, RegentMarple, SavoyHeatonMoor, TheLightCinemasStockport, VueCinemasManchesterPrintworks, VueCinemasManchesterQuayside)
  val norwich: Seq[Cinema] = Seq(ArcCinemaGreatYarmouth, CentralCinemaFakenham, CornExchangeCinemaKingSLynn, EastCoastCinemaLowestoft, LittleTheatreSheringham, MajesticKingSLynn, MarinaTheatreLowestoft, OdeonNorwich, OrionDereham, PalaceCinemaGorlestonOnSea, CinemaCityPicturehouseNorwich, RegalMovieplexCromer, TheLightThetford, VueCinemasNorwich)
  val aberdeenshire: Seq[Cinema] = Seq(ArcCinemaPeterhead, BelmontFilmhouse, CineworldQueensLinkAberdeen, CineworldUnionSquareAberdeen, MorayPlayhouse, Number30Huntly, TheBarnBanchory, VictoriaHallEllon)
  val antrim: Seq[Cinema] = Seq(IMCCinemaBallymena, MovieHouseGlengormley, OmniplexAntrim, OmniplexCarrickfergus, OmniplexLarne)
  val armagh: Seq[Cinema] = Seq(OmniplexCraigavon)
  val ayrshireAndArran: Seq[Cinema] = Seq(AstoriaCinemaAyr, CinemaSaltcoatsPremierLeisure, OdeonCinemaKilmarnock)
  val bedfordshire: Seq[Cinema] = Seq(CineworldLuton, VueCinemasBedford)
  val belfast: Seq[Cinema] = Seq(CineworldBelfast, MovieHouseCitySideBelfast, OdeonCinemaBelfast, OmniplexBelfast, OmniplexLisburn, QueenSFilmTheatreBelfast, StrandArtsCentreBelfast, TheAvenueCinemaBelfast)
  val berkshire: Seq[Cinema] = Seq(CineworldBracknell, CornExchangeNewburyScreenOne, EverymanCinemaWokingham, OdeonLuxeMaidenhead, ReadingBiscuitFactory, ShowcaseDeLuxReading, SouthHillParkArtsCentreBracknell, TheAssemblyAtHeckfieldPlace, TheOldCourtWindsor, VueCinemasNewbury, VueCinemasReading)
  val birmingham: Seq[Cinema] = Seq(ArtrixBromsgrove, CineworldBroadStreetBirmingham, CineworldNECBirmingham, CineworldSolihull, EverymanCinemaBirmingham, MidlandsArtsCentreBirmingham, MockingbirdCinemaKitchenBirmingham, OdeonBirminghamNewStreet, OdeonLuxeBirminghamBroadwayPlaza, OmniplexBirmingham, ReelCinemaQuinton, RoyalCinemasSuttonColdfield, VueCinemasBirmingham)
  val bristol: Seq[Cinema] = Seq(CubeCinemaBristol, EverymanCinemaBristol, OdeonCabotCircus, ScottCinemasBristolWestburyPark, ShowcaseBristolAvonmeads, VueCinemasBristolCribbsCauseway, VueCinemasBristolLongwellGreen, WatershedBristol)
  val buckinghamshire: Seq[Cinema] = Seq(CineworldHighWycombe, CineworldMiltonKeynes, EverymanCinemaGerrardsCross, EverymanCinemaMarlow, OdeonCinemaAylesbury, OdeonCinemaMiltonKeynes, OmniplexHighWycombeFormerlyEmpire, VillagePictureHouseCuddington)
  val cambridgeshire: Seq[Cinema] = Seq(ArtsCinemaJohnClareTheatrePeterborough, ArtsPicturehouseCambridge, CineworldEly, CineworldHuntingdon, CineworldStNeots, ElyCommunityCinema, EverymanCinemaCambridge, KeyTheatrePeterborough, LuxeWisbech, OdeonLuxePeterborough, ShowcaseDeLuxPeterborough, TheLightCambridge, TheLightWisbech)
  val cardiff: Seq[Cinema] = Seq(ChapterCardiff, CineworldCardiff, EverymanCinemaCardiff, OdeonCinemaCardiff, ShowcaseCinemaCardiff)
  val centralScotland: Seq[Cinema] = Seq(ChalmersAlloaCinema, CineworldFalkirk, HippodromeBoNess, MacrobertArtCentreStirling, VueCinemasStirling)
  val cheshire: Seq[Cinema] = Seq(BuxtonCinemaPavilionArtsCentre, CinemacMacclesfield, CineworldWarrington, CurzonCinemaKnutsford, EverymanCinemaAltrincham, OdeonCinemaCrewe, OdeonCinemaNorthwichBaronsQuay, OdeonLuxeWarrington, PicturehouseChester, ReelCinemaWidnes, RexWilmslow, StoryhouseChester, VueCinemasAltrincham, VueCinemasCheshireOaks)
  val clwyd: Seq[Cinema] = Seq(CineworldLlandudno, MerlinScalaPrestatyn, StrandCinemaRhyl, TheatrColwyn)
  val cornwall: Seq[Cinema] = Seq(CineworldPlymouth, FilmhouseNewlyn, FloraCinemaHelston, MerlinCapitolBodmin, MerlinRegalCinemaRedruth, MerlinSavoyPenzance, PhoenixCinemaFalmouth, PlymouthArtsCinema, RebelCinema, RoyalStIvesCinema, TheAstraCinemaStMawgan, ThePolyFalmouth, VueCinemasPlymouth, WTWLighthouseNewquay, WTWPlazaTruro, WTWRegalWadebridge, WTWWhiteRiverCinema)
  val countyDurham: Seq[Cinema] = Seq(ARCStocktonOnTees, CineworldDaltonParkMurtonCounty, EmpireTheatreConsett, EverymanCinemaDurham, FuseCommunityCinemaPrudhoe, GalaCinemaDurham, OdeonLuxeDurham, ShowcaseCinemaDeLuxTeesside, VueCinemasDarlington, VueCinemasHartlepool)
  val cumbria: Seq[Cinema] = Seq(BreweryArtsCentreKendal, GaietyCinemaWhitehaven, KeswickAlhambra, LonsdaleAlhambraPenrith, ParkwayWorkington, ReelCinemaMorecambe, RoxyUlverston, RoyaltyBownessOnWindemere, TheRitzCinemaWorkington, VueCinemasBarrow, VueCinemasCarlisle, ZeffirellisCinemaAmbleside)
  val derbyshire: Seq[Cinema] = Seq(CineworldChesterfield, EliteCinemaAndTheatreAshbourne, NorthernLightWirksworth, OdeonCinemaSwadlincote, OdeonLuxeDerby, QuadDerby, RitzBelper, ShowcaseCinemaDeLuxDerby)
  val devon: Seq[Cinema] = Seq(AlexandraNewtonAbbot, BarnCinemaDartingtonArtCentre, CentralCinemaBarnstaple, EmbassyCinemaIlfracombe, EverymanCinemaPlymouth, KingsCinemaKingsbridge, LyntonCinema, NewCarltonOkehampton, NewCentralCinemaTorquay, OdeonCinemaExeter, PavilionsTeignmouth, PicturehouseExeter, PloughArtsCentreTorrington, RadwaySidmouth, SavoyScottCinemasExmouth, TheBeehiveHoniton, TheFlavel, TheWatermarkIvybridge, TivoliTiverton, TotnesCinema, VueCinemasExeter, VueCinemasTorbayPaignton)
  val dorset: Seq[Cinema] = Seq(ColosseumBournemouth, ElectricPalaceBridport, HilltopCinemaShaftesburyArtsCentre, LighthousePoole, MowlemTheatre, OdeonCinemaBournemouthBH2, OdeonCinemaDorchester, PlazaCinemaDorchester, RegentChristchurch, TheNewVicTisburyVillageHall, TheRexCinemaWareham, TivoliTheatreWimborne, VueCinemasPoole)
  val down: Seq[Cinema] = Seq(IMCNewtownardsMovieland, IveaghMovieStudioIMCBanbridge, OmniplexBanbridge, OmniplexBangor, OmniplexDownpatrick, OmniplexDundonald, OmniplexNewry)
  val dudley: Seq[Cinema] = Seq(OdeonCinemaDudley, ShowcaseCinemaDudley)
  val dumfriesAndGalloway: Seq[Cinema] = Seq(LonsdaleCityCinemaAnnan, RobertBurnsCentreFilmTheatre, TheCinemaNewtonStewart, TheFullartonCastleDouglas)
  val dunbartonshireArgyllBute: Seq[Cinema] = Seq(CampbeltownPictureHouse, DiscoveryCentreCinemaRothesay, OmniplexClydebankFormerlyEmpire, StudioCinemaDunoon)
  val dyfed: Seq[Cinema] = Seq(AberystwythArtsCentre, CommodoreCinemaAberystwyth, CrossHandsHallCinema, Libanus1877, MinersWelfareAndCommunityHallYstradgynlais, OdeonCinemaLlanelli, PalaceCinemaHaverfordwest, PublicHallBrynamman, TheatrGwaunFishguard, TheatrMwldanCardigan, TorchTheatreMilfordHaven, VueCinemasCarmarthen)
  val eastSussex: Seq[Cinema] = Seq(CineworldBrighton, CineworldEastbourne, DepotLewes, DukeOfYorkSPicturehouseBrighton, DukeSAtKomediaPicturehouse, ElectricPalaceHastings, KinoRye, KinoTeatr, OdeonCinemaBrighton, OdeonCinemaHastings, PavilionHailsham, PictureHouseUckfield, TownerEastbourneCinema)
  val eastYorkshire: Seq[Cinema] = Seq(CineworldHull, ForumBridlington, OdeonLuxeHull, PalaceCinemaMalton, ParkwayBeverley, ReelCinemaHull, VueCinemasHull)
  val edinburghAndLothians: Seq[Cinema] = Seq(CineworldEdinburgh, DominionCinemaEdinburgh, EverymanCinemaEdinburgh, FilmhouseEdinburgh, OdeonEdinburghFortKinnaird, OdeonEdinburghLothianRoad, OdeonLuxeEdinburghEdinburghWest, ScotsmanPicturehouseEdinburgh, TheCameoPicturehouse, TheFraserCentreTranent, VueCinemasLivingston, VueEdinburghOceanTerminal, VueEdinburghOmniCentre)
  val essex: Seq[Cinema] = Seq(CenturyCinemaClacton, CineworldBasildon, CineworldBraintree, CineworldHarlowHarveyCentre, CineworldHarlowQueensgate, CurzonCinemaColchester, ElectricPalaceHarwich, EmpireTheatreHalstead, EverymanCinemaChelmsford, MovieStarrCanveyIsland, OdeonCinemaChelmsford, OdeonCinemaColchester, OdeonCinemaSouthendOnSea, RioBurnhamOnCrouch, RoxyMoviesBishopSStortford, SaffronScreen, VueCinemasBasildon, VueCinemasColchester, VueCinemasWestThurrock)
  val fermanagh: Seq[Cinema] = Seq(IMCCinemaEnniskillen)
  val fife: Seq[Cinema] = Seq(AdamSmithTheatreKirkcaldy, KinoGlenrothes, OdeonCinemaDunfermline)
  val glamorgan: Seq[Cinema] = Seq(ColiseumTheatreAberdare, GwynHallNeath, OdeonCinemaBridgend, OdeonCinemaSwansea, PontardaweArtsCentre, ReelCinemaPortTalbot, TaliesinArtsCentreSwansea, VueCinemasMerthyrTydfil, VueCinemasSwansea)
  val glasgow: Seq[Cinema] = Seq(CineworldSilverburnGlasgow, EverymanCinemaGlasgow, GlasgowFilmTheatre, GrosvenorCinemaGlasgow, IMAXAtGlasgowScienceCentre, LanternhouseCinema, OdeonLuxeGlasgow, VueCinemasGlasgowFort, VueCinemasGlasgowStEnoch)
  val gloucestershire: Seq[Cinema] = Seq(CineworldCheltenham, CineworldGloucesterQuays, ElectricPictureHouseWottonUnderEdge, EverymanCheltenham, GuildhallCinemaGloucester, MerlinStudioColeford, PalaceCinemaCinderford, RosesTheatreTewkesbury, SherborneCinemaGloucester, VueCinemasStroud)
  val guernsey: Seq[Cinema] = Seq(BeauSejourLeisureCentreGuernsey, TheMallardCinemaGuernsey)
  val gwent: Seq[Cinema] = Seq(BakerStreetCinemaAbergavenny, CineworldSpyttyParkNewport, MarketHallCinemaBrynmawr, MaximeCinemaBlackwood, RiverfrontNewport, SavoyTheatreMonmouth, VueCinemasCwmbran)
  val gwynedd: Seq[Cinema] = Seq(CellBBlaenauFfestiniog, EmpireCinemaHolyhead, GaleriCaenarfon, MagicLanternTywyn, NeuaddDwyforPwllheli, PontioArtsInnovationCentreBangor, TheatrDerekWilliams)
  val hampshire: Seq[Cinema] = Seq(ChichesterCinemaAtNewPark, CineworldChichester, CineworldWhiteley, EverymanCinemaWinchester, HarbourLightsPicturehouse, HytheMoviolaCinema, No6CinemaPortsmouth, OdeonCinemaBasingstoke, OdeonCinemaPortSolent, ReelCinemaFareham, ShowcaseDeLuxSouthampton, SouthseaCinemaArtsCentre, TheLivingRoomCinemaLiphook, TheMaltLymington, VueCinemasBasingstoke, VueCinemasEastleigh, VueCinemasPortsmouth)
  val herefordshire: Seq[Cinema] = Seq(CourtyardHereford, GatewayCinemaRossOnWye, OdeonCinemaHereford, RichardBoothSBookshopHayOnWye)
  val hertfordshire: Seq[Cinema] = Seq(BaldockArtsHeritageCentre, BEAMHertfordTheatre, BroadwayLetchworth, CineworldHemelHempstead, CineworldStevenage, CineworldWatford, OdeonCinemaHatfield, ReelCinemaBorehamwood, TheCinemaCampusWest, TheOdysseyStAlbans, TheRexCinemaBerkhamsted, VueCinemasWatford, Watersmeet)
  val highlandsAndIslands: Seq[Cinema] = Seq(AnLanntairArtsCentreStornowayIsleOfLewis, CromartyCinema, EdenCourtTheatreInverness, HighlandCinemaFortWilliam, LASPortRighArosCinemaPortree, MareelLerwickShetlandIslands, MerlinCinemaThurso, PhoenixKirkwallOkneyIslands, SpeyValleyCinemaAviemore, VueCinemasInverness, WestSideCinemaStromness)
  val isleOfMan: Seq[Cinema] = Seq(BroadwayCinemaVillaMarina, PalaceCinemasIsleOfMan)
  val isleOfWight: Seq[Cinema] = Seq(CineworldNewportIsleOfWight, CommodoreRydeIsleOfWight)
  val jersey: Seq[Cinema] = Seq(CineworldStHelierJersey)
  val kent: Seq[Cinema] = Seq(CarltonCinemaWestgateOnSea, CinemarshTheMarshAcademy, CineworldAshford, CineworldDover, CineworldRochester, CurzonCanterburyRiverside, EmpireCinemaSandwich, GulbenkianTheatre, KavanaghCinemaHerneBay, KinoHawkhurst, OdeonCinemaChatham, OdeonCinemaMaidstone, OdeonCinemaTunbridgeWells, PalaceCinemaKent, RoyalCinemaFaversham, ShowcaseDeLuxBluewater, SilverScreenFolkestone, StagSevenoaks, TheAshfordCinemaFormerlyPicturehouse, TheLightSittingbourne, TheWoodvilleGravesend, VueCinemasThanetWestwoodCross)
  val lanarkshire: Seq[Cinema] = Seq(OdeonLuxeEastKilbride, ShowcaseGlasgowCoatbridge, VueCinemasHamilton)
  val lancashire: Seq[Cinema] = Seq(ArcCinemaBlackpool, ArcCinemaPreston, CineworldBolton, CineworldBroughton, EverymanCinemaClitheroe, FlowerBowlEntertainmentCentrePreston, LowtherPavilionLytham, OdeonCinemaPreston, OdeonCinemaRochdale, ReelCinemaBlackburn, ReelCinemaChorley, ReelCinemasBurnley, RegentBlackpool, TheDukesLancaster, TheIslandLythamStAnnes, TheLightBolton, VueCinemasAccrington, VueCinemasBlackburn, VueCinemasBolton, VueCinemasBury, VueCinemasCleveleys, VueCinemasLancaster, VueCinemasPreston)
  val leicestershire: Seq[Cinema] = Seq(CineworldHinckley, FlixStudentRunCinemaLoughborough, OdeonCinemaLoughborough, OdeonLuxeLeicester, PhoenixCinemaAndArtCentreLeicester, PiccadillyCinemaLeicester, RegalMeltonMowbray, ShowcaseDeLuxLeicester, VueCinemasLeicester)
  val lincolnshire: Seq[Cinema] = Seq(ArtsCentreStamford, EverymanCinemaLincoln, JunctionGoole, KinemaInTheWoods, LoewenCinema, OdeonCinemaLincoln, ParkwayCinemaLouth, ParkwayCleethorpes, SavoyBoston, SavoyGrantham, SleafordPlayhouse, TowerCinemaSkegness, VueCinemasScunthorpe)
  val londonderry: Seq[Cinema] = Seq(BrunswickMoviebowlLondonderry, MovieHouseColeraine, MovieHouseMaghera, NerveCentreLondonderry, OmniplexLondonderry)
  val liverpool: Seq[Cinema] = Seq(CineworldSpeke, CineworldStHelens, EverymanCinemaLiverpool, OdeonLiverpoolONE, OdeonLiverpoolSwitchIsland, OdeonLuxeBromborough, PicturehouseAtFACTLiverpool, PlazaCommunityCinemaLiverpool, ShowcaseDeLuxLiverpool, SouthportBijouCinema, TheLightNewBrighton, VueCinemasBirkenhead, VueCinemasSouthport, WooltonPictureHouse)
  val northYorkshire: Seq[Cinema] = Seq(CineworldYork, CityScreenPicturehouseYork, EverymanCinemaHarrogate, EverymanCinemaNorthallerton, EverymanCinemaYork, HollywoodPlazaScarborough, OdeonCinemaHarrogate, OdeonMiddlesbrough, PavilionCinemaWhitby, PocklingtonArtsCentre, RegentRedcar, RitzCinemaThirsk, RoxyMoviesMiddlesbrough, SavoyCinemaCatterickGarrison, StationCinemaRichmond, StephenJosephTheatreScarborough, TheForumNorthallerton, VueCinemasYork)
  val northamptonshire: Seq[Cinema] = Seq(ArcCinemaDaventry, CineworldRushdenLakes, ForumCinemaNorthampton, NorthamptonFilmhouse, OdeonNorthampton, SavoyCinemaCorby, VueCinemasNorthampton)
  val northumberland: Seq[Cinema] = Seq(ForumCinemaHexham, MarketPavilionCinemaBlyth, PhoenixCinemaBlyth, TheMaltingsBerwickUponTweed, VueCinemasCramlington)
  val nottinghamshire: Seq[Cinema] = Seq(ArcCinemaAtTheByronHucknall, ArcCinemaBeeston, BroadwayCinemaNottingham, OdeonCinemaMansfield, OdeonCinemaNewark, ReelCinemaScalaIlkeston, SavoyCinemaNottingham, SavoyWorksop, ShowcaseDeLuxNottingham, VueCinemasNottingham)
  val oxfordshire: Seq[Cinema] = Seq(AbbeyCinemaAbingdon, CineworldDidcot, CineworldWitney, CornExchangeCinemaWallingford, CurzonCinemaOxford, PhoenixPicturehouseOxford, RegalPicturehouseHenley, TheLightBanbury, TheLivingRoomCinemaChippingNorton, TheOxfordCinemaCafe, UltimatePicturePalaceOxford, VueCinemasBicester, VueCinemasOxford)
  val powys: Seq[Cinema] = Seq(ColiseumCinemaBrecon, OdeonCinemaWrexham, WyesideArtsCentreBuilthWells)
  val renfrewshire: Seq[Cinema] = Seq(OdeonCinemaBraehead, ShowcaseDeLuxPaisley, TheTowerDigitalArtsCenterHelensburgh, WaterfrontGreenock)
  val roxburghEttrickAndLauderdale: Seq[Cinema] = Seq(PavilionCinemaGalashiels, TowerMillHeartOfHawick)
  val sandwell: Seq[Cinema] = Seq(OdeonCinemaWestBromwich)
  val shropshire: Seq[Cinema] = Seq(AssemblyRoomsLudlow, CineworldShrewsbury, CineworldTelford, FestivalDraytonCentre, MaonaCinemaOswestry, OdeonLuxeTelford, OldMarketHallShrewsbury, ReelCinemaBridgnorthMajestic, WellingtonOrbit)
  val somerset: Seq[Cinema] = Seq(CineworldWestonSuperMare, CineworldYeovil, CurzonCinemaClevedon, EverymanBath, LittleTheatrePicturehouse, MerlinWellesleyWellington, OdeonCinemaBath, OdeonCinemaTaunton, PlazaCinemaWestonSuperMare, RitzBurnhamOnSea, ScottCinemasBridgwater, TauntonBrewhouse, TheAvenueCinemaMinehead, TheWellsFilmCentre, WestwayCinemaFrome)
  val southYorkshire: Seq[Cinema] = Seq(ArcCinemaRotherham, CineworldBarnsley, ParkwayBarnsley, SavoyDoncaster, VueCinemasDoncaster)
  val staffordshire: Seq[Cinema] = Seq(CannockCinema, CinebowlUttoxeter, CineworldBurtonOnTrent, CineworldStokeOnTrent, CineworldWolverhampton, FilmTheatreStokeOnTrent, LichfieldGarrickTheatreStudio, LockworksCinemaWolverhampton, OdeonCinemaStokeOnTrent, OdeonCinemaTamworth, OdeonLuxeStafford, RedCarpetBartonMarina, TheLightWalsall, VueCinemasNewcastleUnderLyme)
  val suffolk: Seq[Cinema] = Seq(AbbeygateBuryStEdmunds, AldeburghCinema, CineworldBuryStEdmunds, CineworldHaverhill, CineworldIpswich, ElectricPicturePalaceSouthwold, EverymanBuryStEdmunds, FilmTheatreLeiston, HaverhillArtsCentre, KingStreetCinema, KingsCinemaNewmarket, OmniplexIpswichFormerlyEmpire, PalaceCinemaFelixstowe, RegalStowmarket, RiversideTheatreWoodbridge)
  val surrey: Seq[Cinema] = Seq(ChiddingfoldVillageHallCinema, CineworldAldershot, EverymanCinemaOxted, EverymanCinemaReigate, HaslemereHallCinema, OdeonCinemaGuildford, ReelCinemasFarnham, TheLightRedhill, VueCinemasCamberley, VueCinemasFarnborough)
  val tayside: Seq[Cinema] = Seq(BirksAberfeldy, ChalmersFilmhouseArbroath, CineworldDundee, DundeeContemporaryArtsDCA, NewPictureHouseStAndrews, OdeonLuxeDundee, PlayhouseCinemaPerth, TheMontrosePlayhouse)
  val tyneAndWear: Seq[Cinema] = Seq(CineworldBoldonTyneWear, CineworldNewcastle, CustomsHouseCinemaSouthShields, EverymanCinemaNewcastle, JamJarCinema, OdeonCinemaMetrocentre, OdeonCinemaSilverlink, OmniplexSunderlandFormerlyEmpire, StarAndShadowCinemaNewcastle, TynesideNewcastle, VueCinemasGateshead)
  val tyrone: Seq[Cinema] = Seq(OmniplexDungannon, OmniplexOmagh, RitzMultiplexCookstown)
  val warwickshire: Seq[Cinema] = Seq(CineworldRugby, EverymanCinemaStratfordUponAvon, OdeonCinemaCoventry, OdeonLuxeNuneaton, RoyalSpaCentre, ShowcaseDeLuxCoventry, VueCinemasLeamingtonSpa, WarwickArtsCentreCoventry)
  val westSussex: Seq[Cinema] = Seq(AtriumEastGrinstead, CapitolHorsham, CineworldCrawley, ConnaughtTheatreStudioWorthing, DomeWorthing, EverymanCinemaHorsham, OrionBurgessHill, PicturedromeBognorRegis, WindmillLittlehampton)
  val westYorkshire: Seq[Cinema] = Seq(CineworldBradford, CineworldLeeds, CineworldWakefield, CottageRoadCinemaLeeds, EverymanCinemaLeeds, HeartCentreHeadingley, HebdenBridgePicturehouse, HydeParkPictureHouse, IlkleyCinema, OdeonCinemaHuddersfield, OdeonLuxeLeedsThorpePark, OdeonLuxeLeedsBradford, PictureHouseKeighley, PicturevilleScienceAndMediaMuseumBradford, PlazaSkipton, ReelCinemaWakefield, RexElland, ShowcaseDeLuxLeeds, TheLightBradford, VueCinemasCastleford, VueCinemasHalifax, VueCinemasLeedsKirkstallRoad, VueCinemasLeedsTheLight, WetherbyFilmTheatre)
  val wiltshire: Seq[Cinema] = Seq(CineworldShawRidgeSwindon, EverymanCinemaSalisbury, OdeonCinemaAndover, OdeonCinemaSalisbury, OdeonCinemaTrowbridge, PalaceCinemaDevizes, ReelCinemaChippenhamAstoria, RegalCinemaFordingbridge, TheParadeCinemaMarlborough, VueCinemasSwindon)
  val worcestershire: Seq[Cinema] = Seq(CastlemortonCinemaMortonMajestic, FuturistCinema, MalvernTheatres, Number8Pershore, OdeonCinemaWorcester, RegalCinemaEvesham, RegalCinemaTenburyWells, VueCinemasRedditch, VueCinemasWorcester)
  val yorkshire: Seq[Cinema] = Seq(CineworldSheffield, OdeonLuxeSheffield, ParamountPenistone, ShowroomSheffield, TheLightSheffield, VueCinemasSheffield)

  // Germany's cinemas come from `GermanRoster.byCity` (appended to `byCity` below),
  // not per-city vals — the full 158-region roster is data-driven.

  /** Every city's venues in page order, paired with the city's display label.
   *  Single source of truth for `all` and for the uptime page's per-city
   *  grouping — add a city here and both pick it up. */
  val byCity: Seq[(String, Seq[Cinema])] = Seq(
    "Poznań"      -> poznan,
    "Wrocław"     -> wroclaw,
    "Warszawa"    -> warszawa,
    "Kraków"      -> krakow,
    "Łódź"        -> lodz,
    "Katowice"    -> katowice,
    "Szczecin"    -> szczecin,
    "Białystok"   -> bialystok,
    "Trójmiasto"  -> trojmiasto,
    "Bydgoszcz"   -> bydgoszcz,
    "Lublin"      -> lublin,
    "Częstochowa" -> czestochowa,
    "Radom"       -> radom,
    "Sosnowiec"   -> sosnowiec,
    "Toruń"       -> torun,
    "Kielce"      -> kielce,
    "Rzeszów"     -> rzeszow,
    "Gliwice"     -> gliwice,
    "Zabrze"      -> zabrze,
    "Olsztyn"     -> olsztyn,
    "Bielsko-Biała" -> bielskoBiala,
    "Opole"       -> opole,
    "Rybnik"      -> rybnik,
    "Gorzów Wielkopolski" -> gorzow,
    "Elbląg"      -> elblag,
    "Koszalin"    -> koszalin,
    "Kalisz"      -> kalisz,
    "Zielona Góra" -> zielonaGora,
    "Tychy"       -> tychy,
    "Wałbrzych"   -> walbrzych,
    "Tarnów"      -> tarnow,
    "Włocławek"   -> wloclawek,
    "Legnica"     -> legnica,
    "Płock"       -> plock,
    "Bytom"       -> bytom,
    "Dąbrowa Górnicza" -> dabrowaGornicza,
    "Nowy Sącz"   -> nowySacz,
    "Słupsk"      -> slupsk,
    "Jelenia Góra" -> jeleniaGora,
    "Przemyśl"    -> przemysl,
    "Konin"       -> konin,
    // United Kingdom
    "London" -> london,
    "Manchester" -> manchester,
    "Norwich" -> norwich,
    "Aberdeenshire" -> aberdeenshire,
    "Antrim" -> antrim,
    "Armagh" -> armagh,
    "Ayrshire and Arran" -> ayrshireAndArran,
    "Bedfordshire" -> bedfordshire,
    "Belfast" -> belfast,
    "Berkshire" -> berkshire,
    "Birmingham" -> birmingham,
    "Bristol" -> bristol,
    "Buckinghamshire" -> buckinghamshire,
    "Cambridgeshire" -> cambridgeshire,
    "Cardiff" -> cardiff,
    "Central Scotland" -> centralScotland,
    "Cheshire" -> cheshire,
    "Clwyd" -> clwyd,
    "Cornwall" -> cornwall,
    "County Durham" -> countyDurham,
    "Cumbria" -> cumbria,
    "Derbyshire" -> derbyshire,
    "Devon" -> devon,
    "Dorset" -> dorset,
    "Down" -> down,
    "Dudley" -> dudley,
    "Dumfries and Galloway" -> dumfriesAndGalloway,
    "Dunbartonshire and Argyll & Bute" -> dunbartonshireArgyllBute,
    "Dyfed" -> dyfed,
    "East Sussex" -> eastSussex,
    "East Yorkshire" -> eastYorkshire,
    "Edinburgh & Lothians" -> edinburghAndLothians,
    "Essex" -> essex,
    "Fermanagh" -> fermanagh,
    "Fife" -> fife,
    "Glamorgan" -> glamorgan,
    "Glasgow" -> glasgow,
    "Gloucestershire" -> gloucestershire,
    "Guernsey" -> guernsey,
    "Gwent" -> gwent,
    "Gwynedd" -> gwynedd,
    "Hampshire" -> hampshire,
    "Herefordshire" -> herefordshire,
    "Hertfordshire" -> hertfordshire,
    "Highlands and Islands" -> highlandsAndIslands,
    "Isle of Man" -> isleOfMan,
    "Isle of Wight" -> isleOfWight,
    "Jersey" -> jersey,
    "Kent" -> kent,
    "Lanarkshire" -> lanarkshire,
    "Lancashire" -> lancashire,
    "Leicestershire" -> leicestershire,
    "Lincolnshire" -> lincolnshire,
    "Londonderry" -> londonderry,
    "Liverpool" -> liverpool,
    "North Yorkshire" -> northYorkshire,
    "Northamptonshire" -> northamptonshire,
    "Northumberland" -> northumberland,
    "Nottinghamshire" -> nottinghamshire,
    "Oxfordshire" -> oxfordshire,
    "Powys" -> powys,
    "Renfrewshire" -> renfrewshire,
    "Roxburgh, Ettrick and Lauderdale" -> roxburghEttrickAndLauderdale,
    "Sandwell" -> sandwell,
    "Shropshire" -> shropshire,
    "Somerset" -> somerset,
    "South Yorkshire" -> southYorkshire,
    "Staffordshire" -> staffordshire,
    "Suffolk" -> suffolk,
    "Surrey" -> surrey,
    "Tayside" -> tayside,
    "Tyne and Wear" -> tyneAndWear,
    "Tyrone" -> tyrone,
    "Warwickshire" -> warwickshire,
    "West Sussex" -> westSussex,
    "West Yorkshire" -> westYorkshire,
    "Wiltshire" -> wiltshire,
    "Worcestershire" -> worcestershire,
    "Yorkshire" -> yorkshire,
  ) ++ GermanRoster.byCity  // Germany: the full 158-region roster (data-driven)

  val all: Seq[Cinema] = byCity.flatMap(_._2)

  val pillMap: Map[String, String] = all.map(c => c.displayName -> c.pillName).toMap

  /** Synthetic chain-detail sources → the venue cinemas whose per-film detail
   *  they hold. A chain (only Cinema City today) fetches a film's
   *  synopsis/cast/… ONCE network-wide into its [[CinemaCityChain]] slot rather
   *  than each venue's own slot, so that slot belongs to no single city. A
   *  city-scoped synopsis merge ([[MovieRecord.synopsisForCity]]) consults this
   *  map to decide which cities a chain blurb applies to — namely those where a
   *  member venue screens the film. Derived from the per-city venue lists by the
   *  stable "Cinema City <branch>" naming every Cinema City venue follows
   *  (`CinemaCityChain` itself is "Cinema City", no trailing branch, and isn't in
   *  `all`), so a newly-added branch is picked up automatically. */
  val chainDetailVenues: Map[Source, Set[Cinema]] = Map(
    CinemaCityChain -> all.filter(_.displayName.startsWith("Cinema City ")).toSet
  )

  /** City display label for each cinema — derived from `byCity`, the single
   *  source of truth. Used by the debug source-data view to disambiguate
   *  same-named chains (e.g. the many "Helios" venues across cities). */
  val cityByCinema: Map[Cinema, String] =
    byCity.flatMap { case (city, cinemas) => cinemas.map(_ -> city) }.toMap

  def cityOf(cinema: Cinema): Option[String] = cityByCinema.get(cinema)
}
