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
  )

  val all: Seq[Cinema] = byCity.flatMap(_._2)

  val pillMap: Map[String, String] = all.map(c => c.displayName -> c.pillName).toMap

  /** City display label for each cinema — derived from `byCity`, the single
   *  source of truth. Used by the debug source-data view to disambiguate
   *  same-named chains (e.g. the many "Helios" venues across cities). */
  val cityByCinema: Map[Cinema, String] =
    byCity.flatMap { case (city, cinemas) => cinemas.map(_ -> city) }.toMap

  def cityOf(cinema: Cinema): Option[String] = cityByCinema.get(cinema)
}
