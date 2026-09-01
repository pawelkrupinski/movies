package models

import java.time.ZoneId
import java.util.Locale

import tools.Slugify

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

  /** Non-declining languages: the nominative, with the language's own
   *  preposition in front of it for the locative slot ("in London", "en Madrid").
   *  The preposition is a parameter rather than a literal because it is the only
   *  thing that differs — German's is "in" like English's, Spanish's is not, and
   *  a hardcoded "in" would have put "in Madrid" in every Spanish share card and
   *  every piece of Spanish structured data. */
  private final class Nominative(preposition: String) extends CityGrammar {
    def locativePhrase(labels: CityLabels): String      = s"$preposition ${labels.nominative}"
    def genitivePluralLabel(labels: CityLabels): String = labels.nominative
  }

  private val In = new Nominative("in")   // English, German
  private val En = new Nominative("en")   // Spanish

  def of(locale: Locale): CityGrammar = locale.getLanguage match {
    case "pl" => Polish
    case "es" => En
    case _    => In
  }
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
  /** Resolve one of this city's [[areas]] by its stable slug — the key clients
   *  persist a chosen area under (`'areasChosen:' + city`). `None` for an
   *  unknown slug and for every flat city. */
  def areaBySlug(slug: String): Option[CinemaAreaGroup] = areas.find(_.area.slug == slug)
  /** "Repertuar kin …" locative phrase, in this city's country language.
   *  Polish declines ("w Poznaniu", "we Wrocławiu"); English (and any other
   *  non-declining language) reads "in London". Delegated to [[CityGrammar]] so
   *  the grammar lives in one place and PL output stays byte-identical. Used by
   *  the per-city share-card generator (`tools.OgCardGenerator`) + `StructuredData`. */
  def locativePhrase: String = CityGrammar.of(country.language).locativePhrase(labels)
  /** Filename (under `assets/img/`) of this city's share-preview card — the
   *  repertoire montage `tools.OgCardGenerator` writes and every `/{slug}/` page
   *  names as its `og:image`. The city half of [[Country.homeOgImage]], and here
   *  rather than spelled out at each use for the same reason: the extension and
   *  the naming scheme have both changed under it, and a second spelling is a
   *  second thing to miss. */
  def shareImage: String = s"og-$slug.jpg"
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
case object Aberdeenshire extends City("aberdeenshire",
  CityLabels("Aberdeenshire", "Aberdeenshire", "Aberdeenshire"), 57.308, -2.3393, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.aberdeenshire
}
case object Antrim extends City("antrim",
  CityLabels("Antrim", "Antrim", "Antrim"), 54.762, -6.0127, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.antrim
}
case object Armagh extends City("armagh",
  CityLabels("Armagh", "Armagh", "Armagh"), 54.4492, -6.398, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.armagh
}
case object AyrshireAndArran extends City("ayrshire-and-arran",
  CityLabels("Ayrshire and Arran", "Ayrshire and Arran", "Ayrshire and Arran"), 55.5093, -4.581, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.ayrshireAndArran
}
case object Bedfordshire extends City("bedfordshire",
  CityLabels("Bedfordshire", "Bedfordshire", "Bedfordshire"), 52.0082, -0.4435, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.bedfordshire
}
case object Belfast extends City("belfast",
  CityLabels("Belfast", "Belfast", "Belfast"), 54.5857, -5.9428, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.belfast
}
case object Berkshire extends City("berkshire",
  CityLabels("Berkshire", "Berkshire", "Berkshire"), 51.4268, -0.9169, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.berkshire
}
case object Birmingham extends City("birmingham",
  CityLabels("Birmingham", "Birmingham", "Birmingham"), 52.4581, -1.9041, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.birmingham
}
case object Bristol extends City("bristol",
  CityLabels("Bristol", "Bristol", "Bristol"), 51.4659, -2.5805, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.bristol
}
case object Buckinghamshire extends City("buckinghamshire",
  CityLabels("Buckinghamshire", "Buckinghamshire", "Buckinghamshire"), 51.7582, -0.7609, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.buckinghamshire
}
case object Cambridgeshire extends City("cambridgeshire",
  CityLabels("Cambridgeshire", "Cambridgeshire", "Cambridgeshire"), 52.4301, -0.0137, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.cambridgeshire
}
case object Cardiff extends City("cardiff",
  CityLabels("Cardiff", "Cardiff", "Cardiff"), 51.4892, -3.1939, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.cardiff
}
case object CentralScotland extends City("central-scotland",
  CityLabels("Central Scotland", "Central Scotland", "Central Scotland"), 56.08, -3.8066, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.centralScotland
}
case object Cheshire extends City("cheshire",
  CityLabels("Cheshire", "Cheshire", "Cheshire"), 53.2917, -2.4966, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.cheshire
}
case object Clwyd extends City("clwyd",
  CityLabels("Clwyd", "Clwyd", "Clwyd"), 53.3083, -3.6072, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.clwyd
}
case object Cornwall extends City("cornwall",
  CityLabels("Cornwall", "Cornwall", "Cornwall"), 50.317, -4.9211, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.cornwall
}
case object CountyDurham extends City("county-durham",
  CityLabels("County Durham", "County Durham", "County Durham"), 54.7289, -1.5139, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.countyDurham
}
case object Cumbria extends City("cumbria",
  CityLabels("Cumbria", "Cumbria", "Cumbria"), 54.4593, -3.1119, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.cumbria
}
case object Derbyshire extends City("derbyshire",
  CityLabels("Derbyshire", "Derbyshire", "Derbyshire"), 52.9886, -1.5219, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.derbyshire
}
case object Devon extends City("devon",
  CityLabels("Devon", "Devon", "Devon"), 50.6651, -3.687, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.devon
}
case object Dorset extends City("dorset",
  CityLabels("Dorset", "Dorset", "Dorset"), 50.7664, -2.1122, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.dorset
}
case object Down extends City("down",
  CityLabels("Down", "Down", "Down"), 54.4293, -5.9704, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.down
}
case object Dudley extends City("dudley",
  CityLabels("Dudley", "Dudley", "Dudley"), 52.497, -2.0918, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.dudley
}
case object DumfriesAndGalloway extends City("dumfries-and-galloway",
  CityLabels("Dumfries and Galloway", "Dumfries and Galloway", "Dumfries and Galloway"), 54.9881, -3.8232, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.dumfriesAndGalloway
}
case object DunbartonshireArgyllBute extends City("dunbartonshire-argyll-bute",
  CityLabels("Dunbartonshire and Argyll & Bute", "Dunbartonshire and Argyll & Bute", "Dunbartonshire and Argyll & Bute"), 55.7795, -4.9973, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.dunbartonshireArgyllBute
}
case object Dyfed extends City("dyfed",
  CityLabels("Dyfed", "Dyfed", "Dyfed"), 51.9892, -4.3329, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.dyfed
}
case object EastSussex extends City("east-sussex",
  CityLabels("East Sussex", "East Sussex", "East Sussex"), 50.8499, 0.2215, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.eastSussex
}
case object EastYorkshire extends City("east-yorkshire",
  CityLabels("East Yorkshire", "East Yorkshire", "East Yorkshire"), 53.8685, -0.3985, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.eastYorkshire
}
case object EdinburghAndLothians extends City("edinburgh-and-lothians",
  CityLabels("Edinburgh & Lothians", "Edinburgh & Lothians", "Edinburgh & Lothians"), 55.9404, -3.2039, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.edinburghAndLothians
}
case object Essex extends City("essex",
  CityLabels("Essex", "Essex", "Essex"), 51.7621, 0.5901, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.essex
}
case object Fermanagh extends City("fermanagh",
  CityLabels("Fermanagh", "Fermanagh", "Fermanagh"), 54.3499, -7.6316, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.fermanagh
}
case object Fife extends City("fife",
  CityLabels("Fife", "Fife", "Fife"), 56.1287, -3.2424, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.fife
}
case object Glamorgan extends City("glamorgan",
  CityLabels("Glamorgan", "Glamorgan", "Glamorgan"), 51.6388, -3.7535, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.glamorgan
}
case object Glasgow extends City("glasgow",
  CityLabels("Glasgow", "Glasgow", "Glasgow"), 55.8682, -4.2316, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.glasgow
}
case object Gloucestershire extends City("gloucestershire",
  CityLabels("Gloucestershire", "Gloucestershire", "Gloucestershire"), 51.8387, -2.2712, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.gloucestershire
}
case object Guernsey extends City("guernsey",
  CityLabels("Guernsey", "Guernsey", "Guernsey"), 49.4446, -2.5695, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.guernsey
}
case object Gwent extends City("gwent",
  CityLabels("Gwent", "Gwent", "Gwent"), 51.6882, -3.0066, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.gwent
}
case object Gwynedd extends City("gwynedd",
  CityLabels("Gwynedd", "Gwynedd", "Gwynedd"), 53.0098, -4.153, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.gwynedd
}
case object Hampshire extends City("hampshire",
  CityLabels("Hampshire", "Hampshire", "Hampshire"), 50.9234, -1.165, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.hampshire
}
case object Herefordshire extends City("herefordshire",
  CityLabels("Herefordshire", "Herefordshire", "Herefordshire"), 52.031, -2.7825, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.herefordshire
}
case object Hertfordshire extends City("hertfordshire",
  CityLabels("Hertfordshire", "Hertfordshire", "Hertfordshire"), 51.7791, -0.3102, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.hertfordshire
}
case object HighlandsAndIslands extends City("highlands-and-islands",
  CityLabels("Highlands and Islands", "Highlands and Islands", "Highlands and Islands"), 58.086, -4.0855, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.highlandsAndIslands
}
case object IsleOfMan extends City("isle-of-man",
  CityLabels("Isle of Man", "Isle of Man", "Isle of Man"), 54.1578, -4.4775, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.isleOfMan
}
case object IsleOfWight extends City("isle-of-wight",
  CityLabels("Isle of Wight", "Isle of Wight", "Isle of Wight"), 50.7118, -1.2248, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.isleOfWight
}
case object Jersey extends City("jersey",
  CityLabels("Jersey", "Jersey", "Jersey"), 49.1839, -2.1144, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.jersey
}
case object Kent extends City("kent",
  CityLabels("Kent", "Kent", "Kent"), 51.2682, 0.8631, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.kent
}
case object Lanarkshire extends City("lanarkshire",
  CityLabels("Lanarkshire", "Lanarkshire", "Lanarkshire"), 55.7953, -4.0904, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.lanarkshire
}
case object Lancashire extends City("lancashire",
  CityLabels("Lancashire", "Lancashire", "Lancashire"), 53.7367, -2.6625, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.lancashire
}
case object Leicestershire extends City("leicestershire",
  CityLabels("Leicestershire", "Leicestershire", "Leicestershire"), 52.6656, -1.1514, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.leicestershire
}
case object Lincolnshire extends City("lincolnshire",
  CityLabels("Lincolnshire", "Lincolnshire", "Lincolnshire"), 53.2194, -0.2916, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.lincolnshire
}
case object Londonderry extends City("londonderry",
  CityLabels("Londonderry", "Londonderry", "Londonderry"), 54.9949, -7.0636, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.londonderry
}
case object Liverpool extends City("liverpool",
  CityLabels("Liverpool", "Liverpool", "Liverpool"), 53.4084, -2.9916, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.liverpool
}
case object NorthYorkshire extends City("north-yorkshire",
  CityLabels("North Yorkshire", "North Yorkshire", "North Yorkshire"), 54.2402, -1.156, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.northYorkshire
}
case object Northamptonshire extends City("northamptonshire",
  CityLabels("Northamptonshire", "Northamptonshire", "Northamptonshire"), 52.288, -0.8653, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.northamptonshire
}
case object Northumberland extends City("northumberland",
  CityLabels("Northumberland", "Northumberland", "Northumberland"), 55.2158, -1.7422, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.northumberland
}
case object Nottinghamshire extends City("nottinghamshire",
  CityLabels("Nottinghamshire", "Nottinghamshire", "Nottinghamshire"), 53.0236, -1.15, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.nottinghamshire
}
case object Oxfordshire extends City("oxfordshire",
  CityLabels("Oxfordshire", "Oxfordshire", "Oxfordshire"), 51.7572, -1.2545, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.oxfordshire
}
case object Powys extends City("powys",
  CityLabels("Powys", "Powys", "Powys"), 52.3806, -3.26, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.powys
}
case object Renfrewshire extends City("renfrewshire",
  CityLabels("Renfrewshire", "Renfrewshire", "Renfrewshire"), 55.9204, -4.5838, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.renfrewshire
}
case object RoxburghEttrickAndLauderdale extends City("roxburgh-ettrick-and-lauderdale",
  CityLabels("Roxburgh, Ettrick and Lauderdale", "Roxburgh, Ettrick and Lauderdale", "Roxburgh, Ettrick and Lauderdale"), 55.5183, -2.7969, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.roxburghEttrickAndLauderdale
}
case object Sandwell extends City("sandwell",
  CityLabels("Sandwell", "Sandwell", "Sandwell"), 52.5175, -1.9932, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.sandwell
}
case object Shropshire extends City("shropshire",
  CityLabels("Shropshire", "Shropshire", "Shropshire"), 52.6813, -2.6215, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.shropshire
}
case object Somerset extends City("somerset",
  CityLabels("Somerset", "Somerset", "Somerset"), 51.2159, -2.824, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.somerset
}
case object SouthYorkshire extends City("south-yorkshire",
  CityLabels("South Yorkshire", "South Yorkshire", "South Yorkshire"), 53.5141, -1.3109, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.southYorkshire
}
case object Staffordshire extends City("staffordshire",
  CityLabels("Staffordshire", "Staffordshire", "Staffordshire"), 52.7942, -1.9887, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.staffordshire
}
case object Suffolk extends City("suffolk",
  CityLabels("Suffolk", "Suffolk", "Suffolk"), 52.1492, 1.0262, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.suffolk
}
case object Surrey extends City("surrey",
  CityLabels("Surrey", "Surrey", "Surrey"), 51.2269, -0.5354, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.surrey
}
case object Tayside extends City("tayside",
  CityLabels("Tayside", "Tayside", "Tayside"), 56.5061, -3.0128, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.tayside
}
case object TyneAndWear extends City("tyne-and-wear",
  CityLabels("Tyne and Wear", "Tyne and Wear", "Tyne and Wear"), 54.9749, -1.5397, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.tyneAndWear
}
case object Tyrone extends City("tyrone",
  CityLabels("Tyrone", "Tyrone", "Tyrone"), 54.5255, -6.8664, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.tyrone
}
case object Warwickshire extends City("warwickshire",
  CityLabels("Warwickshire", "Warwickshire", "Warwickshire"), 52.3602, -1.5034, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.warwickshire
}
case object WestSussex extends City("west-sussex",
  CityLabels("West Sussex", "West Sussex", "West Sussex"), 50.9492, -0.3262, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.westSussex
}
case object WestYorkshire extends City("west-yorkshire",
  CityLabels("West Yorkshire", "West Yorkshire", "West Yorkshire"), 53.7878, -1.665, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.westYorkshire
}
case object Wiltshire extends City("wiltshire",
  CityLabels("Wiltshire", "Wiltshire", "Wiltshire"), 51.2955, -1.8505, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.wiltshire
}
case object Worcestershire extends City("worcestershire",
  CityLabels("Worcestershire", "Worcestershire", "Worcestershire"), 52.1923, -2.2079, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.worcestershire
}
case object Yorkshire extends City("yorkshire",
  CityLabels("Yorkshire", "Yorkshire", "Yorkshire"), 53.4082, -1.4756, ZoneId.of("Europe/London")) {
  val cinemas: Seq[Cinema] = Cinema.yorkshire
}

// ── Germany (also non-declining for our purposes). ───────────────────────────

/** A German region — the data-driven `City` subtype. The full roster (158 regions
 *  / 1,529 cinemas) is generated into `GermanRosterData` and materialised by
 *  [[GermanRoster]]; each region groups the cinemas within ~35 km of a hub city
 *  (see `data/germany/`). Instances are built ONCE (in `GermanRoster`), so
 *  identity equality holds just like the hand-authored `case object` cities —
 *  every lookup uses those singletons. */
final class GermanRegion(slug: String, labels: CityLabels, lat: Double, lon: Double, cinemas0: Seq[Cinema])
  extends City(slug, labels, lat, lon, ZoneId.of("Europe/Berlin")) {
  val cinemas: Seq[Cinema] = cinemas0
}

/** A US METRO — the data-driven `City` subtype, and the place a US visitor
 *  actually names. "Films in Los Angeles" is a screen somebody wants; "films in
 *  California" is not, which is why the state is a [[CityGroup]] here and not a
 *  city. The nine states and territories too small to have metros (Alaska,
 *  Hawaii, DC, …) have nothing to split into, so each is one of these in its own
 *  right, keeping the slug it already had.
 *
 *  The roster (5,031 cinemas over 448 metros + those nine) is generated into
 *  `UsRosterData` and materialised by [[UsRoster]]; instances are built ONCE (in
 *  `City.usCities`), so identity equality holds just like the hand-authored
 *  `case object` cities.
 *
 *  Unlike [[GermanRegion]] the zone is a CONSTRUCTOR parameter rather than a
 *  constant: the US spans six of them, so `Europe/Berlin`'s one-size answer has
 *  no US equivalent. Each place carries its state's predominant zone (see
 *  `data/us/scripts/states.py` for the handful of states that straddle two). */
final class UsCity(slug: String, labels: CityLabels, lat: Double, lon: Double,
                   zoneId: ZoneId, cinemas0: Seq[Cinema], areas0: Seq[CinemaAreaGroup])
  extends City(slug, labels, lat, lon, zoneId) {
  val cinemas: Seq[Cinema] = cinemas0
  /** The DISTRICTS of a metro too big to browse as one list — Manhattan,
   *  Brooklyn, Santa Monica — and `Nil` for every other place. The level below
   *  the metro, exactly as London's compass areas sit below London; see
   *  [[UsMetroSubAreas]]. */
  override val areas: Seq[CinemaAreaGroup] = areas0
}

/** A Spanish PROVINCE — the data-driven `City` subtype, and the unit both the
 *  source and a Spanish visitor use. SensaCine enumerates its own catalogue by
 *  province (`/cines/provincias-<id>/`), and 52 of them is a list a picker stays
 *  readable at, so unlike the US there is nothing to group them under: Spain's
 *  picker is flat, like Germany's.
 *
 *  The roster (595 cinemas over 52 provinces) is generated into
 *  `SpanishRosterData` and materialised by [[SpanishRoster]]; instances are built
 *  ONCE (in `City.spanishCities`), so identity equality holds just like the
 *  hand-authored `case object` cities.
 *
 *  Like [[UsCity]] and unlike [[GermanRegion]] the zone is a CONSTRUCTOR
 *  parameter: the Canary provinces run an hour behind the peninsula, and a
 *  single national zone would move their day boundary. */
final class SpanishProvince(slug: String, labels: CityLabels, lat: Double, lon: Double,
                            zoneId: ZoneId, cinemas0: Seq[Cinema])
  extends City(slug, labels, lat, lon, zoneId) {
  val cinemas: Seq[Cinema] = cinemas0
}

/** A named group of [[City]]s in a picker — a US STATE over its metros
 *  ("California → Los Angeles", the way a visitor finds one). The state is not
 *  a place you can open: `/california/` is nothing, and its films live at
 *  `/los-angeles/`. It is how the list of them is arranged, and nothing else.
 *
 *  Every other country's picker is one flat list, so `Country.cityGroups` is
 *  empty there rather than one group per city. */
final case class CityGroup(label: String, slug: String, cities: Seq[City])

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

  /** The United Kingdom's full modelled roster — every Flicks region we know how
   *  to scrape (79). Retained in full even while only a subset is live so that
   *  bringing a city back online is a one-line edit to [[activeUkCities]], never
   *  a re-declaration. Do NOT trim this list to disable cities — narrow
   *  [[activeUkCities]] instead. */
  private[models] val allUkCities: Seq[City] = Seq(
    London, Manchester, Norwich, Aberdeenshire, Antrim, Armagh, AyrshireAndArran, Bedfordshire, Belfast, Berkshire, Birmingham, Bristol, Buckinghamshire, Cambridgeshire, Cardiff, CentralScotland, Cheshire, Clwyd, Cornwall, CountyDurham, Cumbria, Derbyshire, Devon, Dorset, Down, Dudley, DumfriesAndGalloway, DunbartonshireArgyllBute, Dyfed, EastSussex, EastYorkshire, EdinburghAndLothians, Essex, Fermanagh, Fife, Glamorgan, Glasgow, Gloucestershire, Guernsey, Gwent, Gwynedd, Hampshire, Herefordshire, Hertfordshire, HighlandsAndIslands, IsleOfMan, IsleOfWight, Jersey, Kent, Lanarkshire, Lancashire, Leicestershire, Lincolnshire, Londonderry, Liverpool, NorthYorkshire, Northamptonshire, Northumberland, Nottinghamshire, Oxfordshire, Powys, Renfrewshire, RoxburghEttrickAndLauderdale, Sandwell, Shropshire, Somerset, SouthYorkshire, Staffordshire, Suffolk, Surrey, Tayside, TyneAndWear, Tyrone, Warwickshire, WestSussex, WestYorkshire, Wiltshire, Worcestershire, Yorkshire,
  )

  /** The UK cities currently live — the ones web serves and the worker scrapes.
   *  Currently the FULL modelled roster: every Flicks region is live. To scope
   *  the live set down again, narrow this to a subset of [[allUkCities]] (e.g.
   *  `Set(London, Manchester, …)`); the disabled cities stay fully modelled
   *  above, so re-enabling is always a one-line edit here — never a
   *  re-declaration. */
  private[models] val activeUkCities: Set[City] = allUkCities.toSet

  /** The authoritative UK list for [[Country.UnitedKingdom]] — the live subset
   *  of [[allUkCities]], kept in that list's declared order. */
  private[models] val ukCities: Seq[City] = allUkCities.filter(activeUkCities)

  /** Germany's cities — the authoritative list for [[Country.Germany]]. The full
   *  158-region roster, materialised data-driven from `GermanRosterData` (see
   *  [[GermanRegion]] / `GermanRoster`), rather than hand-authored case objects. */
  private[models] val germanCities: Seq[City] = GermanRoster.regions

  /** The United States' cities — the authoritative list for
   *  [[Country.UnitedStates]]. One city per METRO (448 of them, distance-clustered
   *  from the venues' own coordinates — see `data/us/scripts/cluster_metros.py`),
   *  plus the nine states and territories with too few venues to be worth
   *  splitting, which are cities in their own right. The state itself is a
   *  [[CityGroup]], not a city: nobody wants a screen of every cinema in Texas.
   *
   *  The slug is assigned HERE rather than in [[UsRoster]] because it is the one
   *  thing about a place that depends on every OTHER country: `City.bySlug` is a
   *  single global namespace, and a metro's own name is not unique in it —
   *  "Birmingham" is a UK city, and clusters never cross a state line so
   *  "Philadelphia" is a metro in two states at once. See [[usSlugs]]. */
  private[models] val usCities: Seq[City] = {
    val slugs = usSlugs(UsRoster.places)
    UsRoster.places.zip(slugs).map { case (p, slug) =>
      new UsCity(slug, CityLabels(p.label, p.label, p.label), p.lat, p.lon, p.zoneId, p.cinemas, p.districts)
    }
  }

  /** The URL slug of each [[UsRoster.places]] entry, in that order.
   *
   *  A place keeps its own folded label wherever that is free, and is qualified
   *  with its state where it is not (`birmingham-alabama`,
   *  `philadelphia-new-jersey`). Two different things can take the bare slug
   *  away, and they answer differently:
   *
   *  - ANOTHER COUNTRY already serves it. `/birmingham/` is a live UK page and
   *    `/glasgow/` a live Scottish one; a US metro may not move either. The US
   *    place qualifies, always.
   *  - ANOTHER US PLACE wants it, because clusters never cross a state line, so
   *    one travel-shed arrives as one metro per state it reaches: New York in
   *    both NY and NJ, Philadelphia in both PA and NJ, Kansas City in both KS
   *    and MO — 31 names claimed twice or more. Here the BIGGEST keeps the bare slug (ties on the state's name):
   *    `/new-york/` is the 102-venue New York side, which is what a visitor
   *    typing it means, and the New Jersey commuter belt is
   *    `/new-york-new-jersey/`.
   *
   *  Qualifying only the SLUG: the label stays "Philadelphia", because the
   *  picker shows it under its state's heading, where it is not ambiguous. */
  private def usSlugs(places: Seq[UsPlace]): Seq[String] = {
    val foreign = (polishCities ++ allUkCities ++ germanCities).map(_.slug).toSet
    val biggest = places.groupBy(_.preferredSlug)
      .map { case (slug, claimants) => slug -> claimants.maxBy(p => (p.cinemas.size, p.stateName)) }
    def keepsBareSlug(p: UsPlace): Boolean =
      !foreign.contains(p.preferredSlug) && biggest(p.preferredSlug).eq(p)
    places.map(p => if (keepsBareSlug(p)) p.preferredSlug else Slugify.stable(s"${p.label} ${p.stateName}"))
  }

  /** Slugs a city USED to answer at, and the one it answers at now.
   *
   *  A city slug is published — the sitemap, whatever a search engine indexed,
   *  the `city` cookie of everyone who has visited, the share card named after
   *  it — so renaming one is a redirect, not an edit. `modules.RenamedCityRedirectFilter`
   *  301s every path under the old slug to the same path under the new one.
   *
   *  An entry stays FOREVER: the cost is one map lookup per request, and the
   *  links it serves are the ones nobody can go back and fix.
   *
   *  - `san-francisco` → `san-francisco-bay-area` (2026-08-30). The metro is
   *    the whole Bay and its own regions include a San Francisco that is the
   *    city proper, so the old name both shadowed a region and filed San Jose
   *    and Oakland under a city they are not in. See `UsRoster.MetroDisplayNames`.
   */
  val renamedSlugs: Map[String, String] = Map(
    "san-francisco" -> "san-francisco-bay-area",
  )

  /** The slugs a city used to answer at, given the one it answers at now.
   *
   *  The read model is PROJECTED under the slug — a `CityScreening`'s `_id` is
   *  `filmId|city|cinema` — so the instant a slug changes, every already-projected
   *  row for that city is filed under a name nothing asks for, and stays that way
   *  until each film is projected again (up to a whole scrape cadence: 14h in the
   *  US). `WebReadModel` reads the former slugs alongside the current one so the
   *  city keeps serving across that window. */
  def formerSlugs(current: String): Seq[String] = formerSlugsByCurrent.getOrElse(current, Nil)

  private lazy val formerSlugsByCurrent: Map[String, Seq[String]] =
    renamedSlugs.groupMap(_._2)(_._1).view.mapValues(_.toSeq).toMap

  /** The US picker's grouping: one entry per state or territory, in roster
   *  order, holding the metros cut out of it (or the state itself, where it is
   *  small enough to be one place). */
  private[models] val usStates: Seq[CityGroup] =
    UsRoster.places.zip(usCities).foldLeft(Vector.empty[CityGroup]) {
      case (groups :+ last, (place, city)) if last.slug == place.stateSlug =>
        groups :+ last.copy(cities = last.cities :+ city)
      case (groups, (place, city)) =>
        groups :+ CityGroup(place.stateName, place.stateSlug, Seq(city))
    }

  /** Spain's cities — the authoritative list for [[Country.Spain]]. One city per
   *  PROVINCE (52 of them), materialised data-driven from `SpanishRosterData`
   *  (see [[SpanishProvince]] / [[SpanishRoster]]) rather than hand-authored case
   *  objects, the same shape Germany uses.
   *
   *  The slug is assigned HERE rather than in [[SpanishRoster]] for the reason
   *  [[usSlugs]] spells out: `City.bySlug` is a single global namespace, and a
   *  province name is not unique in it. */
  private[models] val spanishCities: Seq[City] = {
    val slugs = spanishSlugs(SpanishRoster.places)
    SpanishRoster.places.zip(slugs).map { case (p, slug) =>
      new SpanishProvince(slug, CityLabels(p.name, p.name, p.name), p.lat, p.lon, p.zoneId, p.cinemas)
    }
  }

  /** The URL slug of each [[SpanishRoster.places]] entry, in that order.
   *
   *  A province keeps its own folded name wherever that is free, and is
   *  qualified with its autonomous community where it is not
   *  (`toledo-castilla-la-mancha`) — the same rule, and the same reason, as a US
   *  metro qualifying with its state.
   *
   *  ONE province needs it today: Toledo, whose name the US roster already
   *  serves as the Ohio metro at `/toledo/`. Spain qualifies rather than the US,
   *  because `/toledo/` is a live published URL with a sitemap entry, an
   *  indexed page and a `city` cookie behind it, and the newcomer is the one
   *  that can still choose. Unlike the US case there is no "biggest claimant
   *  keeps it" tie-break to make: two provinces never share a name.
   *
   *  Qualifying only the SLUG — the label stays "Toledo", which is what the
   *  Spanish picker shows and what is unambiguous inside Spain. */
  private def spanishSlugs(places: Seq[SpanishPlace]): Seq[String] = {
    val foreign = (polishCities ++ allUkCities ++ germanCities ++ usCities).map(_.slug).toSet
    places.map { p =>
      if (foreign.contains(p.preferredSlug)) Slugify.stable(s"${p.name} ${p.community}")
      else p.preferredSlug
    }
  }

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
  val all: Seq[City] = polishCities ++ ukCities ++ germanCities ++ usCities ++ spanishCities

  /** Every modelled city across all countries — including cities that are
   *  declared in code but currently disabled, so absent from the live [[all]]
   *  (e.g. the UK cities filtered out by [[activeUkCities]]). [[all]] is the
   *  LIVE roster that web serves and the worker scrapes; this is the FULL roster
   *  used only by coverage/partition checks that must also see disabled cities. */
  val allModelled: Seq[City] = polishCities ++ allUkCities ++ germanCities ++ usCities ++ spanishCities

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
