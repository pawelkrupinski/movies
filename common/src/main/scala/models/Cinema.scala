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
  )

  /** Katowice venues. Cinema City has two locations (Punkt 44 and Silesia),
   *  Multikino one, Helios one. The art-house trio run by Silesia Film —
   *  Kosmos and Światowid — are Bilety24-hosted independents wired as bespoke
   *  scrapers. (Kinoteatr Rialto, the third Silesia Film venue, currently
   *  programmes only concerts/theatre and no film repertoire, so it's omitted.) */
  val katowice: Seq[Cinema] = Seq(
    CinemaCityPunkt44,
    CinemaCitySilesia,
    MultikinoKatowice,
    HeliosKatowice,
    KinoKosmos,
    KinoSwiatowid,
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
  )

  val czestochowa: Seq[Cinema] = Seq(CinemaCityCzestochowaJurajska, CinemaCityCzestochowaWolnosc, OkfIluzja)

  val radom: Seq[Cinema] = Seq(HeliosRadom, MultikinoRadom, McswElektrowniaCinema)

  val sosnowiec: Seq[Cinema] = Seq(HeliosSosnowiec, CinemaCitySosnowiec)

  val torun: Seq[Cinema] = Seq(CinemaCityTorunCzerwonaDroga, CinemaCityTorunPlaza, KinoCentrumCsw)

  val kielce: Seq[Cinema] = Seq(HeliosKielce, MultikinoKielce, KinoFenomen, KinoMoskwa)

  val rzeszow: Seq[Cinema] = Seq(HeliosRzeszow, MultikinoRzeszow, KinoZorza, KinoZaRogiemCafe)

  val gliwice: Seq[Cinema] = Seq(CinemaCityGliwice, KinoAmok)

  val zabrze: Seq[Cinema] = Seq(MultikinoZabrze, KinoRoma)

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
  )

  val all: Seq[Cinema] = byCity.flatMap(_._2)

  val pillMap: Map[String, String] = all.map(c => c.displayName -> c.pillName).toMap
}
