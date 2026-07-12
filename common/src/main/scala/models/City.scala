package models

import java.time.ZoneId
import java.util.Locale

/** The Polish name of a city in the grammatical forms the templates need:
 *  nominative ("Poznań"), genitive plural for "…skich kin" ("poznańskich"),
 *  and locative for "w …" ("Poznaniu"). Kept as data so no template hardcodes
 *  a city name. */
final case class CityLabels(nominative: String, genitivePlural: String, locative: String)

/** Per-language rendering of a city's [[CityLabels]] into the grammatical
 *  phrases the UI needs. Polish declines the name (locative "w Poznaniu" /
 *  "we Wrocławiu", genitive-plural adjective "poznańskich"); languages that
 *  don't decline (English, and the default fallback) read off the plain
 *  nominative ("in London", "London"). Selected by the city's country language
 *  so a per-country deployment renders naturally without any name hardcoded. */
private[models] sealed trait CityGrammar {
  def locativePhrase(labels: CityLabels): String
  def genitivePluralLabel(labels: CityLabels): String
}

private[models] object CityGrammar {

  /** Polish: the declined forms from [[CityLabels]], byte-identical to what the
   *  templates/OG-card generator emitted before i18n. "we" replaces "w" before a
   *  word starting with W/F + consonant (the awkward "w w-" / "w f-" cluster). */
  private object Polish extends CityGrammar {
    def locativePhrase(labels: CityLabels): String = {
      val loc    = labels.locative
      val vowels = "aeiouyąęó"
      val we     = loc.length >= 2 && (loc(0) == 'W' || loc(0) == 'F') &&
                   !vowels.contains(loc(1).toLower)
      s"${if (we) "we" else "w"} $loc"
    }
    def genitivePluralLabel(labels: CityLabels): String = labels.genitivePlural
  }

  /** Non-declining languages (English, default): the nominative, with the
   *  English preposition for the locative slot ("in London"). */
  private object Nominative extends CityGrammar {
    def locativePhrase(labels: CityLabels): String    = s"in ${labels.nominative}"
    def genitivePluralLabel(labels: CityLabels): String = labels.nominative
  }

  def of(locale: Locale): CityGrammar =
    if (locale.getLanguage == "pl") Polish else Nominative
}

/**
 * A city of cinema repertoire. A city is simply a **named subset of
 * globally-unique [[Cinema]] objects** plus its label inflections and
 * geographic centre (for client-side nearest-city selection). The enrichment
 * cache, Mongo, and merge rules stay global and city-agnostic — "city" is a
 * read-path scope filter (show only films/slots whose cinema ∈ this city) and
 * a URL path prefix `/{slug}/`. See `MovieControllerService` for the scoping.
 */
sealed abstract class City(
  val slug:   String,
  val labels: CityLabels,
  val lat:    Double,
  val lon:    Double,
  val zoneId: ZoneId,
) {
  def cinemas: Seq[Cinema]
  /** This city's cinema sub-regions, or empty when the city is **flat** (the
   *  default — most cities). When non-empty the areas PARTITION `cinemas`: their
   *  union is exactly `cinemas`, with no overlap and nothing left out (enforced
   *  by `CinemaAreaSpec`). A city opts into splitting by overriding this; clients
   *  then render one collapsible, individually-(de)selectable group per area. */
  def areas: Seq[CinemaAreaGroup] = Nil
  /** Whether this city is split into [[areas]] (vs. a flat cinema list). */
  def isSplit: Boolean = areas.nonEmpty
  /** "Repertuar kin …" locative phrase, in this city's country language.
   *  Polish declines ("w Poznaniu", "we Wrocławiu"); English (and any other
   *  non-declining language) reads "in London". Delegated to [[CityGrammar]] so
   *  the grammar lives in one place and PL output stays byte-identical. Used by
   *  the per-city share-card generator (`tools.OgCardGenerator`) + `StructuredData`. */
  def locativePhrase: String = CityGrammar.of(country.language).locativePhrase(labels)
  /** The city label used in the "…skich kin" ("<city>'s cinemas") genitive-plural
   *  slot: the declined Polish adjective ("poznańskich"), or — for a language
   *  that doesn't decline — the plain nominative ("London"). */
  def genitivePluralLabel: String = CityGrammar.of(country.language).genitivePluralLabel(labels)
  /** The country this city belongs to (reverse lookup over [[Country.all]]). */
  def country: Country                         = Country.of(this)
  lazy val cinemaSet: Set[Cinema]              = cinemas.toSet
  def cinemaDisplayNames: Seq[String]          = cinemas.map(_.displayName)
  /** Display-name → pill-name for this city's cinemas — the per-city
   *  counterpart of `Cinema.pillMap`, for `_sharedJsConfig`. */
  def cinemaPillMap: Map[String, String]       = cinemas.map(c => c.displayName -> c.pillName).toMap
}

case object Poznan extends City(
  slug   = "poznan",
  labels = CityLabels(nominative = "Poznań", genitivePlural = "poznańskich", locative = "Poznaniu"),
  lat    = 52.4064,
  lon    = 16.9252,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.poznan
}

case object Wroclaw extends City(
  slug   = "wroclaw",
  labels = CityLabels(nominative = "Wrocław", genitivePlural = "wrocławskich", locative = "Wrocławiu"),
  lat    = 51.1079,
  lon    = 17.0385,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.wroclaw
}

case object Warszawa extends City(
  slug   = "warszawa",
  labels = CityLabels(nominative = "Warszawa", genitivePlural = "warszawskich", locative = "Warszawie"),
  lat    = 52.2297,
  lon    = 21.0122,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.warszawa
}

case object Krakow extends City(
  slug   = "krakow",
  labels = CityLabels(nominative = "Kraków", genitivePlural = "krakowskich", locative = "Krakowie"),
  lat    = 50.0647,
  lon    = 19.9450,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.krakow
}

case object Lodz extends City(
  slug   = "lodz",
  labels = CityLabels(nominative = "Łódź", genitivePlural = "łódzkich", locative = "Łodzi"),
  lat    = 51.7592,
  lon    = 19.4560,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.lodz
}

case object Katowice extends City(
  slug   = "katowice",
  labels = CityLabels(nominative = "Katowice", genitivePlural = "katowickich", locative = "Katowicach"),
  lat    = 50.2649,
  lon    = 19.0238,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.katowice
}

case object Szczecin extends City(
  slug   = "szczecin",
  labels = CityLabels(nominative = "Szczecin", genitivePlural = "szczecińskich", locative = "Szczecinie"),
  lat    = 53.4285,
  lon    = 14.5528,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.szczecin
}

case object Bialystok extends City(
  slug   = "bialystok",
  labels = CityLabels(nominative = "Białystok", genitivePlural = "białostockich", locative = "Białymstoku"),
  lat    = 53.1325,
  lon    = 23.1688,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.bialystok
}

/** The Tri-City (Gdańsk · Gdynia · Sopot) as one repertoire scope. Centred on
 *  Sopot, the geographic middle of the three towns, so the nearest-city pick
 *  resolves anywhere in the conurbation. */
case object Trojmiasto extends City(
  slug   = "trojmiasto",
  labels = CityLabels(nominative = "Trójmiasto", genitivePlural = "trójmiejskich", locative = "Trójmieście"),
  lat    = 54.4416,
  lon    = 18.5601,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.trojmiasto
}

case object Bydgoszcz extends City(
  slug   = "bydgoszcz",
  labels = CityLabels(nominative = "Bydgoszcz", genitivePlural = "bydgoskich", locative = "Bydgoszczy"),
  lat    = 53.1235,
  lon    = 18.0084,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.bydgoszcz
}

case object Lublin extends City(
  slug   = "lublin",
  labels = CityLabels(nominative = "Lublin", genitivePlural = "lubelskich", locative = "Lublinie"),
  lat    = 51.2465,
  lon    = 22.5684,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.lublin
}

case object Czestochowa extends City(
  slug   = "czestochowa",
  labels = CityLabels(nominative = "Częstochowa", genitivePlural = "częstochowskich", locative = "Częstochowie"),
  lat    = 50.8118,
  lon    = 19.1203,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.czestochowa
}

case object Radom extends City(
  slug   = "radom",
  labels = CityLabels(nominative = "Radom", genitivePlural = "radomskich", locative = "Radomiu"),
  lat    = 51.4027,
  lon    = 21.1471,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.radom
}

case object Sosnowiec extends City(
  slug   = "sosnowiec",
  labels = CityLabels(nominative = "Sosnowiec", genitivePlural = "sosnowieckich", locative = "Sosnowcu"),
  lat    = 50.2863,
  lon    = 19.1041,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.sosnowiec
}

case object Torun extends City(
  slug   = "torun",
  labels = CityLabels(nominative = "Toruń", genitivePlural = "toruńskich", locative = "Toruniu"),
  lat    = 53.0138,
  lon    = 18.5984,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.torun
}

case object Kielce extends City(
  slug   = "kielce",
  labels = CityLabels(nominative = "Kielce", genitivePlural = "kieleckich", locative = "Kielcach"),
  lat    = 50.8661,
  lon    = 20.6286,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.kielce
}

case object Rzeszow extends City(
  slug   = "rzeszow",
  labels = CityLabels(nominative = "Rzeszów", genitivePlural = "rzeszowskich", locative = "Rzeszowie"),
  lat    = 50.0413,
  lon    = 21.9990,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.rzeszow
}

case object Gliwice extends City(
  slug   = "gliwice",
  labels = CityLabels(nominative = "Gliwice", genitivePlural = "gliwickich", locative = "Gliwicach"),
  lat    = 50.2945,
  lon    = 18.6714,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.gliwice
}

case object Zabrze extends City(
  slug   = "zabrze",
  labels = CityLabels(nominative = "Zabrze", genitivePlural = "zabrzańskich", locative = "Zabrzu"),
  lat    = 50.3249,
  lon    = 18.7857,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.zabrze
}

// ── New mid-size cities (national-chain coverage) ─────────────────────────────

case object Olsztyn extends City(
  slug   = "olsztyn",
  labels = CityLabels(nominative = "Olsztyn", genitivePlural = "olsztyńskich", locative = "Olsztynie"),
  lat    = 53.7784,
  lon    = 20.4801,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.olsztyn
}

case object BielskoBiala extends City(
  slug   = "bielsko-biala",
  labels = CityLabels(nominative = "Bielsko-Biała", genitivePlural = "bielskich", locative = "Bielsku-Białej"),
  lat    = 49.8224,
  lon    = 19.0584,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.bielskoBiala
}

case object Opole extends City(
  slug   = "opole",
  labels = CityLabels(nominative = "Opole", genitivePlural = "opolskich", locative = "Opolu"),
  lat    = 50.6751,
  lon    = 17.9213,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.opole
}

case object Rybnik extends City(
  slug   = "rybnik",
  labels = CityLabels(nominative = "Rybnik", genitivePlural = "rybnickich", locative = "Rybniku"),
  lat    = 50.0971,
  lon    = 18.5416,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.rybnik
}

case object GorzowWielkopolski extends City(
  slug   = "gorzow-wielkopolski",
  labels = CityLabels(nominative = "Gorzów Wielkopolski", genitivePlural = "gorzowskich", locative = "Gorzowie Wielkopolskim"),
  lat    = 52.7368,
  lon    = 15.2288,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.gorzow
}

case object Elblag extends City(
  slug   = "elblag",
  labels = CityLabels(nominative = "Elbląg", genitivePlural = "elbląskich", locative = "Elblągu"),
  lat    = 54.1522,
  lon    = 19.4088,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.elblag
}

case object Koszalin extends City(
  slug   = "koszalin",
  labels = CityLabels(nominative = "Koszalin", genitivePlural = "koszalińskich", locative = "Koszalinie"),
  lat    = 54.1943,
  lon    = 16.1722,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.koszalin
}

case object Kalisz extends City(
  slug   = "kalisz",
  labels = CityLabels(nominative = "Kalisz", genitivePlural = "kaliskich", locative = "Kaliszu"),
  lat    = 51.7611,
  lon    = 18.0911,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.kalisz
}

case object ZielonaGora extends City(
  slug   = "zielona-gora",
  labels = CityLabels(nominative = "Zielona Góra", genitivePlural = "zielonogórskich", locative = "Zielonej Górze"),
  lat    = 51.9356,
  lon    = 15.5062,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.zielonaGora
}

case object Tychy extends City(
  slug   = "tychy",
  labels = CityLabels(nominative = "Tychy", genitivePlural = "tyskich", locative = "Tychach"),
  lat    = 50.1357,
  lon    = 18.9985,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.tychy
}

case object Walbrzych extends City(
  slug   = "walbrzych",
  labels = CityLabels(nominative = "Wałbrzych", genitivePlural = "wałbrzyskich", locative = "Wałbrzychu"),
  lat    = 50.7714,
  lon    = 16.2845,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.walbrzych
}

case object Tarnow extends City(
  slug   = "tarnow",
  labels = CityLabels(nominative = "Tarnów", genitivePlural = "tarnowskich", locative = "Tarnowie"),
  lat    = 50.0121,
  lon    = 20.9858,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.tarnow
}

case object Wloclawek extends City(
  slug   = "wloclawek",
  labels = CityLabels(nominative = "Włocławek", genitivePlural = "włocławskich", locative = "Włocławku"),
  lat    = 52.6483,
  lon    = 19.0677,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.wloclawek
}

case object Legnica extends City(
  slug   = "legnica",
  labels = CityLabels(nominative = "Legnica", genitivePlural = "legnickich", locative = "Legnicy"),
  lat    = 51.2070,
  lon    = 16.1619,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.legnica
}

case object Plock extends City(
  slug   = "plock",
  labels = CityLabels(nominative = "Płock", genitivePlural = "płockich", locative = "Płocku"),
  lat    = 52.5468,
  lon    = 19.7064,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.plock
}

case object Bytom extends City(
  slug   = "bytom",
  labels = CityLabels(nominative = "Bytom", genitivePlural = "bytomskich", locative = "Bytomiu"),
  lat    = 50.3483,
  lon    = 18.9157,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.bytom
}

case object DabrowaGornicza extends City(
  slug   = "dabrowa-gornicza",
  labels = CityLabels(nominative = "Dąbrowa Górnicza", genitivePlural = "dąbrowskich", locative = "Dąbrowie Górniczej"),
  lat    = 50.3219,
  lon    = 19.1876,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.dabrowaGornicza
}

case object NowySacz extends City(
  slug   = "nowy-sacz",
  labels = CityLabels(nominative = "Nowy Sącz", genitivePlural = "nowosądeckich", locative = "Nowym Sączu"),
  lat    = 49.6175,
  lon    = 20.7154,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.nowySacz
}

case object Slupsk extends City(
  slug   = "slupsk",
  labels = CityLabels(nominative = "Słupsk", genitivePlural = "słupskich", locative = "Słupsku"),
  lat    = 54.4641,
  lon    = 17.0287,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.slupsk
}

case object JeleniaGora extends City(
  slug   = "jelenia-gora",
  labels = CityLabels(nominative = "Jelenia Góra", genitivePlural = "jeleniogórskich", locative = "Jeleniej Górze"),
  lat    = 50.9044,
  lon    = 15.7197,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.jeleniaGora
}

case object Przemysl extends City(
  slug   = "przemysl",
  labels = CityLabels(nominative = "Przemyśl", genitivePlural = "przemyskich", locative = "Przemyślu"),
  lat    = 49.7838,
  lon    = 22.7677,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.przemysl
}

case object Konin extends City(
  slug   = "konin",
  labels = CityLabels(nominative = "Konin", genitivePlural = "konińskich", locative = "Koninie"),
  lat    = 52.2230,
  lon    = 18.2511,
  zoneId = ZoneId.of("Europe/Warsaw"),
) {
  val cinemas: Seq[Cinema] = Cinema.konin
}

// ── United Kingdom (English — non-declining, so the three label slots all carry
//    the plain nominative; `CityGrammar.Nominative` reads only that). ──────────

case object London extends City("london",
  CityLabels("London", "London", "London"), 51.5074, -0.1278, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.london
  // London is the first split city: ~130 venues across Greater London, grouped
  // by compass area so the filter is navigable. See `Cinema.londonAreas`.
  override val areas: Seq[CinemaAreaGroup] = Cinema.londonAreas
}
case object Manchester extends City("manchester",
  CityLabels("Manchester", "Manchester", "Manchester"), 53.4808, -2.2426, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.manchester
}
case object Norwich extends City("norwich",
  CityLabels("Norwich", "Norwich", "Norwich"), 52.6309, 1.2974, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.norwich
}

// ── Germany (also non-declining for our purposes). ───────────────────────────

case object Berlin extends City("berlin",
  CityLabels("Berlin", "Berlin", "Berlin"), 52.5200, 13.4050, ZoneId.of("Europe/Berlin")) {
  val cinemas: Seq[Cinema] = Cinema.berlin
}
case object Munich extends City("munich",
  CityLabels("München", "München", "München"), 48.1351, 11.5820, ZoneId.of("Europe/Berlin")) {
  val cinemas: Seq[Cinema] = Cinema.munich
}
case object Wurzburg extends City("wurzburg",
  CityLabels("Würzburg", "Würzburg", "Würzburg"), 49.7913, 9.9534, ZoneId.of("Europe/Berlin")) {
  val cinemas: Seq[Cinema] = Cinema.wurzburg
}

object City {
  /** Poland's cities — the authoritative list for [[Country.Poland]]. [[all]] is
   *  the union across every [[Country]], so a new country contributes its own
   *  list (e.g. `ukCities`) here and [[all]] picks it up automatically. */
  private[models] val polishCities: Seq[City] = Seq(
    Poznan, Wroclaw, Warszawa, Krakow, Lodz, Katowice, Szczecin, Bialystok, Trojmiasto, Bydgoszcz, Lublin,
    Czestochowa, Radom, Sosnowiec, Torun, Kielce, Rzeszow, Gliwice, Zabrze,
    Olsztyn, BielskoBiala, Opole, Rybnik, GorzowWielkopolski, Elblag, Koszalin, Kalisz, ZielonaGora, Tychy,
    Walbrzych, Tarnow, Wloclawek, Legnica, Plock, Bytom, DabrowaGornicza, NowySacz, Slupsk, JeleniaGora,
    Przemysl, Konin,
  )

  /** The United Kingdom's cities — the authoritative list for
   *  [[Country.UnitedKingdom]]. */
  private[models] val ukCities: Seq[City] = Seq(London, Manchester, Norwich)

  /** Germany's cities — the authoritative list for [[Country.Germany]]. */
  private[models] val germanCities: Seq[City] = Seq(Berlin, Munich, Wurzburg)

  /** Every modelled city, across all countries — the global view used by the
   *  worker (which scrapes every country) and by country-agnostic reverse
   *  lookups. A single-country web deployment scopes to `country.cities`.
   *
   *  Built directly from the per-country lists that live HERE (`polishCities`,
   *  and future `ukCities`, …), NOT via `Country.all` — `Country` depends on
   *  `City` (its `cities` read `City.polishCities`), so a back-reference would
   *  make the two objects' static initialisers wait on each other and deadlock
   *  when loaded on parallel threads. Keep the dependency one-directional:
   *  `Country → City`. A new country adds its list to this concatenation. */
  val all: Seq[City] = polishCities ++ ukCities ++ germanCities

  def bySlug(slug: String): Option[City] = all.find(_.slug == slug)

  /** Reverse lookup: which city a cinema belongs to. Each cinema appears in
   *  exactly one city's `cinemas` list, so the map is unambiguous. Used by
   *  `MovieRecord.cities` to deep-link the (global) debug corpus into a city
   *  whose cinemas actually screen the film. */
  private lazy val cinemaToCity: Map[Cinema, City] =
    all.flatMap(c => c.cinemas.map(_ -> c)).toMap

  def forCinema(cinema: Cinema): Option[City] = cinemaToCity.get(cinema)

  /** [[all]] ordered alphabetically by display name under Polish collation, so
   *  the UI city pickers read A→Z with `Ł` after `L`, `Ó` after `O`, etc.
   *  rather than dumping the diacritic letters at the end (code-point order).
   *  This is the list every *UI* picker iterates; [[all]] keeps its hand-tuned
   *  order for `default`/`allJson`/nearest-city use, where order is semantic. */
  val allSorted: Seq[City] = CityListing.sorted(all, Locale.forLanguageTag("pl-PL"))

  /** Compact JSON array of every city for the client (web `ALL_CITIES`,
   *  consumed by the geolocation/nearest-city picker + the filter switch).
   *  Hand-built (no play-json dependency in models); city names carry no
   *  characters needing JSON escaping. */
  def allJson: String = CityListing.json(all)
}
