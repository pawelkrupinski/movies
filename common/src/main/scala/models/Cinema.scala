package models

sealed abstract class Cinema(val displayName: String, val pillName: String) extends Source

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
case object KinoCskLublin        extends Cinema("Kino CSK Lublin", "CSK")

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

  val wroclaw: Seq[Cinema] = Seq(
    CinemaCityWroclavia,
    CinemaCityKorona,
    MultikinoPasazGrunwaldzki,
    HeliosMagnolia,
    HeliosAlejaBielany,
    KinoNoweHoryzonty,
    DolnoslaskieCentrumFilmowe,
  )

  val warszawa: Seq[Cinema] = Seq(
    CinemaCityArkadia,
    CinemaCityBemowo,
    CinemaCityGaleriaPolnocna,
    CinemaCityJanki,
    CinemaCityMokotow,
    CinemaCityPromenada,
    CinemaCitySadyba,
    MultikinoZloteTarasy,
    MultikinoMlociny,
    MultikinoReduta,
    MultikinoTargowek,
    MultikinoWolaPark,
    HeliosBlueCity,
    KinoMuranow,
    KinoLuna,
    KinoElektronik,
    KinoIluzjon,
    KinoGram,
    KinoKultura,
    KinoAmondo,
    KinoNaBoku,
    KinoGlebocka66,
    Kinomuzeum,
    KinoSwit,
    KinoKepa,
    StacjaFalenica,
    SluzewskiDomKultury,
    KinoAtlantic,
    Kinoteka,
    Ujazdowski,
    KinoCytadela,
    KinoWisla,
    AdaKinoStudyjne,
  )

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
  val lodz: Seq[Cinema] = Seq(
    CinemaCityManufaktura,
    MultikinoLodz,
    HeliosLodz,
    KinoCharlie,
    KinematografLodz,
    Nckf,
    KinoTatry,
  )

  /** Katowice venues. Cinema City has two locations (Punkt 44 and Silesia),
   *  Multikino one, Helios one. The three art-house venues run by Silesia Film —
   *  Kosmos, Światowid, and Kinoteatr Rialto — are all Bilety24-hosted. */
  val katowice: Seq[Cinema] = Seq(
    CinemaCityPunkt44,
    CinemaCitySilesia,
    MultikinoKatowice,
    HeliosKatowice,
    KinoKosmos,
    KinoSwiatowid,
    KinoteatrRialto,
  )

  /** Szczecin venues. No Cinema City here; the two multiplexes are Helios (in
   *  CH Kupiec) and Multikino. The lone independent is Kino Pionier 1907 — the
   *  oldest continuously-operating cinema in the world — wired as a bespoke
   *  scraper. */
  val szczecin: Seq[Cinema] = Seq(
    HeliosSzczecin,
    MultikinoSzczecin,
    KinoPionier,
    HeliosOutletPark,
    KinoZamekSzczecin,
  )

  /** Białystok venues. No Cinema City or Multikino in the city; Helios (HQ'd in
   *  Łódź, strong in eastern Poland) runs three multiplexes — Alfa, Biała and
   *  Jurowiecka. The independent is Kino Forum, the film screen of the
   *  Białostocki Ośrodek Kultury, wired as a bespoke scraper. */
  val bialystok: Seq[Cinema] = Seq(
    HeliosAlfa,
    HeliosBiala,
    HeliosJurowiecka,
    KinoForum,
  )

  /** Trójmiasto venues — the Tri-City of Gdańsk, Gdynia and Sopot treated as one
   *  repertoire. Two Helios in Gdańsk (Metropolia, Forum), one in Gdynia
   *  (Riviera), plus Multikino Gdańsk. Cinema City Krewetka closed in 2018 and
   *  Multikino Sopot in 2026, so neither is modelled. The independents (Cinema1,
   *  GCF, …) are wired separately as bespoke scrapers. */
  val trojmiasto: Seq[Cinema] = Seq(
    MultikinoGdansk,
    HeliosMetropolia,
    HeliosForum,
    HeliosRiviera,
    KinoSpektrum,
    KinoKameralne,
    KinoIkm,
    KinoMuzeumGdansk,
    KinoZak,
    KinoPort,
    Cinema1Gdansk,
    GdynskieCentrumFilmowe,
  )

  /** Bydgoszcz venues. All three national chains have one multiplex each
   *  (Cinema City in Focus Mall, Multikino on Focha, Helios on Fordońska);
   *  the city's one art-house screen is MCK's Kino Orzeł, ticketed through the
   *  national Bilety24 marketplace. */
  val bydgoszcz: Seq[Cinema] = Seq(
    CinemaCityBydgoszcz,
    MultikinoBydgoszcz,
    HeliosBydgoszcz,
    KinoOrzel,
  )

  /** Lublin venues. Two Cinema City multiplexes (Felicity, Lublin Plaza) and
   *  Multikino in Galeria Olimp; no Helios in Lublin. Kino Bajka — the Centrum
   *  Kultury art-house on Radziszewskiego — is the local studyjne screen, wired
   *  separately as a bespoke scraper. */
  val lublin: Seq[Cinema] = Seq(
    CinemaCityLublinFelicity,
    CinemaCityLublinPlaza,
    MultikinoLublin,
    KinoBajka,
    KinoCkLublin,
    KinoCskLublin,
    KinoChatkaZaka,
  )

  val czestochowa: Seq[Cinema] = Seq(CinemaCityCzestochowaJurajska, CinemaCityCzestochowaWolnosc, OkfIluzja)

  val radom: Seq[Cinema] = Seq(HeliosRadom, MultikinoRadom, McswElektrowniaCinema)

  val sosnowiec: Seq[Cinema] = Seq(HeliosSosnowiec, CinemaCitySosnowiec)

  val torun: Seq[Cinema] = Seq(CinemaCityTorunCzerwonaDroga, CinemaCityTorunPlaza, KinoCentrumCsw)

  val kielce: Seq[Cinema] = Seq(HeliosKielce, MultikinoKielce, KinoFenomen, KinoMoskwa)

  val rzeszow: Seq[Cinema] = Seq(HeliosRzeszow, MultikinoRzeszow, KinoZorza, KinoZaRogiemCafe)

  val gliwice: Seq[Cinema] = Seq(CinemaCityGliwice, KinoAmok)

  val zabrze: Seq[Cinema] = Seq(MultikinoZabrze, KinoRoma)

  // New chains-first cities. Each holds only its national-chain branches for now;
  // a local independent screen, where one exists, joins later as a bespoke scraper.
  val olsztyn: Seq[Cinema]      = Seq(HeliosOlsztyn, MultikinoOlsztyn, KinoAwangarda2)
  val bielskoBiala: Seq[Cinema] = Seq(HeliosBielskoBiala, CinemaCityBielskoBiala, KinoKreska)
  val opole: Seq[Cinema]        = Seq(HeliosOpoleKarolinka, HeliosOpoleSolaris, KinoMeduza)
  val rybnik: Seq[Cinema]       = Seq(MultikinoRybnik, CinemaCityRybnik)
  val gorzow: Seq[Cinema]       = Seq(HeliosGorzow, MultikinoGorzow, Kino60Krzesel)
  val elblag: Seq[Cinema]       = Seq(MultikinoElblag, CinemaCityElblag)
  val koszalin: Seq[Cinema]     = Seq(HeliosKoszalin, MultikinoKoszalin, KinoKryterium)
  val kalisz: Seq[Cinema]       = Seq(HeliosKalisz, MultikinoKalisz)
  val zielonaGora: Seq[Cinema]  = Seq(CinemaCityZielonaGora)
  val tychy: Seq[Cinema]        = Seq(MultikinoTychy)
  val walbrzych: Seq[Cinema]    = Seq(CinemaCityWalbrzych, KinoApolloWalbrzych)
  val tarnow: Seq[Cinema]       = Seq(MultikinoTarnow, KinoMillenium)
  val wloclawek: Seq[Cinema]    = Seq(MultikinoWloclawek)
  val legnica: Seq[Cinema]      = Seq(HeliosLegnica, KinoPiast)
  val plock: Seq[Cinema]        = Seq(HeliosPlock, KinoPrzedwiosnie)
  val bytom: Seq[Cinema]        = Seq(CinemaCityBytom)
  val dabrowaGornicza: Seq[Cinema] = Seq(HeliosDabrowaGornicza, KinoKadr)
  val nowySacz: Seq[Cinema]     = Seq(HeliosNowySacz, KinoSokol)
  val slupsk: Seq[Cinema]       = Seq(MultikinoSlupsk, KinoRejs)
  val jeleniaGora: Seq[Cinema]  = Seq(HeliosJeleniaGora, KinoLot)
  val przemysl: Seq[Cinema]     = Seq(HeliosPrzemysl)
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
}
