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
  )

  val all: Seq[Cinema] = poznan ++ wroclaw ++ warszawa ++ krakow ++ trojmiasto

  val pillMap: Map[String, String] = all.map(c => c.displayName -> c.pillName).toMap
}
