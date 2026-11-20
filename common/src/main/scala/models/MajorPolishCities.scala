package models

/* The 41 major Polish cities ([[MajorPolishCity]]): each one's slug, hand-declined labels and centre.
 * `data/pl/scripts/build_pages.py` and `build_venue_towns.py` read these declarations. */

case object Poznan extends MajorPolishCity(
  slug   = "poznan",
  labels = CityLabels(nominative = "Poznań", genitivePlural = "poznańskich", locative = "Poznaniu"),
  lat    = 52.4064,
  lon    = 16.9252,
)

case object Wroclaw extends MajorPolishCity(
  slug   = "wroclaw",
  labels = CityLabels(nominative = "Wrocław", genitivePlural = "wrocławskich", locative = "Wrocławiu"),
  lat    = 51.1079,
  lon    = 17.0385,
)

case object Warszawa extends MajorPolishCity(
  slug   = "warszawa",
  labels = CityLabels(nominative = "Warszawa", genitivePlural = "warszawskich", locative = "Warszawie"),
  lat    = 52.2297,
  lon    = 21.0122,
)

case object Krakow extends MajorPolishCity(
  slug   = "krakow",
  labels = CityLabels(nominative = "Kraków", genitivePlural = "krakowskich", locative = "Krakowie"),
  lat    = 50.0647,
  lon    = 19.9450,
)

case object Lodz extends MajorPolishCity(
  slug   = "lodz",
  labels = CityLabels(nominative = "Łódź", genitivePlural = "łódzkich", locative = "Łodzi"),
  lat    = 51.7592,
  lon    = 19.4560,
)

case object Katowice extends MajorPolishCity(
  slug   = "katowice",
  labels = CityLabels(nominative = "Katowice", genitivePlural = "katowickich", locative = "Katowicach"),
  lat    = 50.2649,
  lon    = 19.0238,
)

case object Szczecin extends MajorPolishCity(
  slug   = "szczecin",
  labels = CityLabels(nominative = "Szczecin", genitivePlural = "szczecińskich", locative = "Szczecinie"),
  lat    = 53.4285,
  lon    = 14.5528,
)

case object Bialystok extends MajorPolishCity(
  slug   = "bialystok",
  labels = CityLabels(nominative = "Białystok", genitivePlural = "białostockich", locative = "Białymstoku"),
  lat    = 53.1325,
  lon    = 23.1688,
)

/** The Tri-City (Gdańsk · Gdynia · Sopot) as one repertoire scope. Centred on
 *  Sopot, the geographic middle of the three towns, so the nearest-city pick
 *  resolves anywhere in the conurbation. */
case object Trojmiasto extends MajorPolishCity(
  slug   = "trojmiasto",
  labels = CityLabels(nominative = "Trójmiasto", genitivePlural = "trójmiejskich", locative = "Trójmieście"),
  lat    = 54.4416,
  lon    = 18.5601,
) {
  /** The three towns the conurbation is, biggest first, then any other town the
   *  venue table files one of its venues under. Its satellite towns (Rumia,
   *  Wejherowo, Pruszcz Gdański) are pages of their own now. Needed at all
   *  because "Trójmiasto" is a name nobody searches a cinema by: only "Gdańsk"
   *  appeared on the page, inside `Multikino Gdańsk`. */
  override protected val extraPlaces: Seq[String] = (homeTowns ++ TownRanking.ofVenues(cinemas)).distinct
  /** No venue sits in "Trójmiasto": the unannotated ones are in its three towns. */
  override def homeTowns: Seq[String] = Seq("Gdańsk", "Gdynia", "Sopot")
}

case object Bydgoszcz extends MajorPolishCity(
  slug   = "bydgoszcz",
  labels = CityLabels(nominative = "Bydgoszcz", genitivePlural = "bydgoskich", locative = "Bydgoszczy"),
  lat    = 53.1235,
  lon    = 18.0084,
)

case object Lublin extends MajorPolishCity(
  slug   = "lublin",
  labels = CityLabels(nominative = "Lublin", genitivePlural = "lubelskich", locative = "Lublinie"),
  lat    = 51.2465,
  lon    = 22.5684,
)

case object Czestochowa extends MajorPolishCity(
  slug   = "czestochowa",
  labels = CityLabels(nominative = "Częstochowa", genitivePlural = "częstochowskich", locative = "Częstochowie"),
  lat    = 50.8118,
  lon    = 19.1203,
)

case object Radom extends MajorPolishCity(
  slug   = "radom",
  labels = CityLabels(nominative = "Radom", genitivePlural = "radomskich", locative = "Radomiu"),
  lat    = 51.4027,
  lon    = 21.1471,
)

case object Sosnowiec extends MajorPolishCity(
  slug   = "sosnowiec",
  labels = CityLabels(nominative = "Sosnowiec", genitivePlural = "sosnowieckich", locative = "Sosnowcu"),
  lat    = 50.2863,
  lon    = 19.1041,
)

case object Torun extends MajorPolishCity(
  slug   = "torun",
  labels = CityLabels(nominative = "Toruń", genitivePlural = "toruńskich", locative = "Toruniu"),
  lat    = 53.0138,
  lon    = 18.5984,
)

case object Kielce extends MajorPolishCity(
  slug   = "kielce",
  labels = CityLabels(nominative = "Kielce", genitivePlural = "kieleckich", locative = "Kielcach"),
  lat    = 50.8661,
  lon    = 20.6286,
)

case object Rzeszow extends MajorPolishCity(
  slug   = "rzeszow",
  labels = CityLabels(nominative = "Rzeszów", genitivePlural = "rzeszowskich", locative = "Rzeszowie"),
  lat    = 50.0413,
  lon    = 21.9990,
)

case object Gliwice extends MajorPolishCity(
  slug   = "gliwice",
  labels = CityLabels(nominative = "Gliwice", genitivePlural = "gliwickich", locative = "Gliwicach"),
  lat    = 50.2945,
  lon    = 18.6714,
)

case object Zabrze extends MajorPolishCity(
  slug   = "zabrze",
  labels = CityLabels(nominative = "Zabrze", genitivePlural = "zabrzańskich", locative = "Zabrzu"),
  lat    = 50.3249,
  lon    = 18.7857,
)

// ── New mid-size cities (national-chain coverage) ─────────────────────────────

case object Olsztyn extends MajorPolishCity(
  slug   = "olsztyn",
  labels = CityLabels(nominative = "Olsztyn", genitivePlural = "olsztyńskich", locative = "Olsztynie"),
  lat    = 53.7784,
  lon    = 20.4801,
)

case object BielskoBiala extends MajorPolishCity(
  slug   = "bielsko-biala",
  labels = CityLabels(nominative = "Bielsko-Biała", genitivePlural = "bielskich", locative = "Bielsku-Białej"),
  lat    = 49.8224,
  lon    = 19.0584,
)

case object Opole extends MajorPolishCity(
  slug   = "opole",
  labels = CityLabels(nominative = "Opole", genitivePlural = "opolskich", locative = "Opolu"),
  lat    = 50.6751,
  lon    = 17.9213,
)

case object Rybnik extends MajorPolishCity(
  slug   = "rybnik",
  labels = CityLabels(nominative = "Rybnik", genitivePlural = "rybnickich", locative = "Rybniku"),
  lat    = 50.0971,
  lon    = 18.5416,
)

case object GorzowWielkopolski extends MajorPolishCity(
  slug   = "gorzow-wielkopolski",
  labels = CityLabels(nominative = "Gorzów Wielkopolski", genitivePlural = "gorzowskich", locative = "Gorzowie Wielkopolskim"),
  lat    = 52.7368,
  lon    = 15.2288,
)

case object Elblag extends MajorPolishCity(
  slug   = "elblag",
  labels = CityLabels(nominative = "Elbląg", genitivePlural = "elbląskich", locative = "Elblągu"),
  lat    = 54.1522,
  lon    = 19.4088,
)

case object Koszalin extends MajorPolishCity(
  slug   = "koszalin",
  labels = CityLabels(nominative = "Koszalin", genitivePlural = "koszalińskich", locative = "Koszalinie"),
  lat    = 54.1943,
  lon    = 16.1722,
)

case object Kalisz extends MajorPolishCity(
  slug   = "kalisz",
  labels = CityLabels(nominative = "Kalisz", genitivePlural = "kaliskich", locative = "Kaliszu"),
  lat    = 51.7611,
  lon    = 18.0911,
)

case object ZielonaGora extends MajorPolishCity(
  slug   = "zielona-gora",
  labels = CityLabels(nominative = "Zielona Góra", genitivePlural = "zielonogórskich", locative = "Zielonej Górze"),
  lat    = 51.9356,
  lon    = 15.5062,
)

case object Tychy extends MajorPolishCity(
  slug   = "tychy",
  labels = CityLabels(nominative = "Tychy", genitivePlural = "tyskich", locative = "Tychach"),
  lat    = 50.1357,
  lon    = 18.9985,
)

case object Walbrzych extends MajorPolishCity(
  slug   = "walbrzych",
  labels = CityLabels(nominative = "Wałbrzych", genitivePlural = "wałbrzyskich", locative = "Wałbrzychu"),
  lat    = 50.7714,
  lon    = 16.2845,
)

case object Tarnow extends MajorPolishCity(
  slug   = "tarnow",
  labels = CityLabels(nominative = "Tarnów", genitivePlural = "tarnowskich", locative = "Tarnowie"),
  lat    = 50.0121,
  lon    = 20.9858,
)

case object Wloclawek extends MajorPolishCity(
  slug   = "wloclawek",
  labels = CityLabels(nominative = "Włocławek", genitivePlural = "włocławskich", locative = "Włocławku"),
  lat    = 52.6483,
  lon    = 19.0677,
)

case object Legnica extends MajorPolishCity(
  slug   = "legnica",
  labels = CityLabels(nominative = "Legnica", genitivePlural = "legnickich", locative = "Legnicy"),
  lat    = 51.2070,
  lon    = 16.1619,
)

case object Plock extends MajorPolishCity(
  slug   = "plock",
  labels = CityLabels(nominative = "Płock", genitivePlural = "płockich", locative = "Płocku"),
  lat    = 52.5468,
  lon    = 19.7064,
)

case object Bytom extends MajorPolishCity(
  slug   = "bytom",
  labels = CityLabels(nominative = "Bytom", genitivePlural = "bytomskich", locative = "Bytomiu"),
  lat    = 50.3483,
  lon    = 18.9157,
)

case object DabrowaGornicza extends MajorPolishCity(
  slug   = "dabrowa-gornicza",
  labels = CityLabels(nominative = "Dąbrowa Górnicza", genitivePlural = "dąbrowskich", locative = "Dąbrowie Górniczej"),
  lat    = 50.3219,
  lon    = 19.1876,
)

case object NowySacz extends MajorPolishCity(
  slug   = "nowy-sacz",
  labels = CityLabels(nominative = "Nowy Sącz", genitivePlural = "nowosądeckich", locative = "Nowym Sączu"),
  lat    = 49.6175,
  lon    = 20.7154,
)

case object Slupsk extends MajorPolishCity(
  slug   = "slupsk",
  labels = CityLabels(nominative = "Słupsk", genitivePlural = "słupskich", locative = "Słupsku"),
  lat    = 54.4641,
  lon    = 17.0287,
)

case object JeleniaGora extends MajorPolishCity(
  slug   = "jelenia-gora",
  labels = CityLabels(nominative = "Jelenia Góra", genitivePlural = "jeleniogórskich", locative = "Jeleniej Górze"),
  lat    = 50.9044,
  lon    = 15.7197,
)

case object Przemysl extends MajorPolishCity(
  slug   = "przemysl",
  labels = CityLabels(nominative = "Przemyśl", genitivePlural = "przemyskich", locative = "Przemyślu"),
  lat    = 49.7838,
  lon    = 22.7677,
)

case object Konin extends MajorPolishCity(
  slug   = "konin",
  labels = CityLabels(nominative = "Konin", genitivePlural = "konińskich", locative = "Koninie"),
  lat    = 52.2230,
  lon    = 18.2511,
)
