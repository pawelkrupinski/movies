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
  /** The city's geographic centre, as a point. */
  final def centre: GeoPoint = GeoPoint(lat, lon)
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
  /** The real-world towns this listing covers, city first — the place names a
   *  visitor (and a search engine) would look the page up by. For most of the
   *  roster that is just the city's own name: one town, one page; a page of
   *  several towns names them through [[extraPlaces]] (a cluster's towns,
   *  [[Trojmiasto]]'s three). London's compass areas are not places and are not
   *  named.
   *
   *  Read by `controllers.StructuredData.cityPage` (as schema.org
   *  `containsPlace`), `controllers.FilterDescription.defaultDescription` and
   *  the page's `<h1>`. Before those, a covered town appeared nowhere a crawler
   *  could read it: "Sopot" was absent from `/trojmiasto/` entirely — title,
   *  description, structured data, and every one of its cinemas' names. */
  final def coveredPlaces: Seq[String] = (labels.nominative +: extraPlaces).distinct
  /** The places this city covers besides its own name. Ordered
   *  most venues first, so the consumers that can only name a few name the ones
   *  worth naming.
   *
   *  The default is the venue table, which is the answer for the two countries
   *  whose rosters are hand-written and therefore have nowhere to put a town —
   *  the UK and Poland. The generated rosters (Germany, Spain, the US) carry
   *  theirs in the tuple and override this; [[Trojmiasto]] is hand-written
   *  because its towns predate the table and are more complete than it. */
  protected def extraPlaces: Seq[String] = VenueTowns.of(cinemas)
  /** The covered towns OTHER than the city itself — empty for the ordinary
   *  one-town city, and the whole reason a multi-town page needs extra words. */
  final def otherCoveredPlaces: Seq[String] = coveredPlaces.filterNot(_ == labels.nominative)
  /** The town(s) one of this city's venues may sit in: the venue table's town
   *  when it names one, else the city's own [[homeTowns]]. Meaningful for the two
   *  HAND-WRITTEN rosters (Poland, the UK), whose out-of-town venues the table
   *  annotates; the generated rosters keep their towns in the roster tuple.
   *  Read by the roster audits, which compare it with the town a venue's source
   *  publishes. */
  final def townsOf(cinema: Cinema): Seq[String] =
    VenueTowns.byDisplayName.get(cinema.displayName).fold(homeTowns)(Seq(_))
  /** The towns an unannotated venue of this city sits in — the city itself,
   *  unless the city is a conurbation under a name no venue gives as its town
   *  (see [[Trojmiasto]]). */
  def homeTowns: Seq[String] = Seq(labels.nominative)
  /** "Repertuar kin …" locative phrase, in this city's country language.
   *  Polish declines ("w Poznaniu", "we Wrocławiu"); English (and any other
   *  non-declining language) reads "in London". Delegated to [[CityGrammar]] so
   *  the grammar lives in one place and PL output stays byte-identical. Used by
   *  the per-city share-card generator (`tools.OgCardGenerator`) + `StructuredData`. */
  def locativePhrase: String = CityGrammar.of(country.language).locativePhrase(labels)
  /** "Repertuar kin w Poznaniu" / "Cinema listings in London" — the city-scoped heading, in the
   *  city's country language: the default page title's lead, the listing's structured-data name and
   *  the line the worker draws on the city's share card. Here, in `common`, because the web and the
   *  worker both read it. */
  def heading: String = {
    val caption = country.language.getLanguage match {
      case "pl" => "Repertuar kin"
      case "es" => "Cartelera de cine"
      case "de" => "Kinoprogramm"
      case _    => "Cinema listings"
    }
    s"$caption $locativePhrase"
  }
  /** File name of this city's share-preview card in its country's share-card directory — the card
   *  the worker renders (`services.sharecards.CityCardService`), Caddy serves at
   *  `/share-cards/<cc>/<this>` and every `/{slug}/` page names as its `og:image`. One spelling
   *  for the writer and the reader. The `city-` prefix keeps it apart from the film cards beside
   *  it, whose names never hold a `-`. */
  def shareImage: String = s"${City.ShareImagePrefix}$slug.jpg"
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
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.poznan
}

case object Wroclaw extends City(
  slug   = "wroclaw",
  labels = CityLabels(nominative = "Wrocław", genitivePlural = "wrocławskich", locative = "Wrocławiu"),
  lat    = 51.1079,
  lon    = 17.0385,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.wroclaw
}

case object Warszawa extends City(
  slug   = "warszawa",
  labels = CityLabels(nominative = "Warszawa", genitivePlural = "warszawskich", locative = "Warszawie"),
  lat    = 52.2297,
  lon    = 21.0122,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.warszawa
}

case object Krakow extends City(
  slug   = "krakow",
  labels = CityLabels(nominative = "Kraków", genitivePlural = "krakowskich", locative = "Krakowie"),
  lat    = 50.0647,
  lon    = 19.9450,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.krakow
}

case object Lodz extends City(
  slug   = "lodz",
  labels = CityLabels(nominative = "Łódź", genitivePlural = "łódzkich", locative = "Łodzi"),
  lat    = 51.7592,
  lon    = 19.4560,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.lodz
}

case object Katowice extends City(
  slug   = "katowice",
  labels = CityLabels(nominative = "Katowice", genitivePlural = "katowickich", locative = "Katowicach"),
  lat    = 50.2649,
  lon    = 19.0238,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.katowice
}

case object Szczecin extends City(
  slug   = "szczecin",
  labels = CityLabels(nominative = "Szczecin", genitivePlural = "szczecińskich", locative = "Szczecinie"),
  lat    = 53.4285,
  lon    = 14.5528,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.szczecin
}

case object Bialystok extends City(
  slug   = "bialystok",
  labels = CityLabels(nominative = "Białystok", genitivePlural = "białostockich", locative = "Białymstoku"),
  lat    = 53.1325,
  lon    = 23.1688,
  zoneId = TimeZones.Poland,
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
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.trojmiasto
  /** The three towns the conurbation is, biggest first, then any other town the
   *  venue table files one of its venues under. Its satellite towns (Rumia,
   *  Wejherowo, Pruszcz Gdański) are pages of their own now. Needed at all
   *  because "Trójmiasto" is a name nobody searches a cinema by: only "Gdańsk"
   *  appeared on the page, inside `Multikino Gdańsk`. */
  override protected val extraPlaces: Seq[String] = (homeTowns ++ VenueTowns.of(cinemas)).distinct
  /** No venue sits in "Trójmiasto": the unannotated ones are in its three towns. */
  override def homeTowns: Seq[String] = Seq("Gdańsk", "Gdynia", "Sopot")
}

case object Bydgoszcz extends City(
  slug   = "bydgoszcz",
  labels = CityLabels(nominative = "Bydgoszcz", genitivePlural = "bydgoskich", locative = "Bydgoszczy"),
  lat    = 53.1235,
  lon    = 18.0084,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.bydgoszcz
}

case object Lublin extends City(
  slug   = "lublin",
  labels = CityLabels(nominative = "Lublin", genitivePlural = "lubelskich", locative = "Lublinie"),
  lat    = 51.2465,
  lon    = 22.5684,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.lublin
}

case object Czestochowa extends City(
  slug   = "czestochowa",
  labels = CityLabels(nominative = "Częstochowa", genitivePlural = "częstochowskich", locative = "Częstochowie"),
  lat    = 50.8118,
  lon    = 19.1203,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.czestochowa
}

case object Radom extends City(
  slug   = "radom",
  labels = CityLabels(nominative = "Radom", genitivePlural = "radomskich", locative = "Radomiu"),
  lat    = 51.4027,
  lon    = 21.1471,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.radom
}

case object Sosnowiec extends City(
  slug   = "sosnowiec",
  labels = CityLabels(nominative = "Sosnowiec", genitivePlural = "sosnowieckich", locative = "Sosnowcu"),
  lat    = 50.2863,
  lon    = 19.1041,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.sosnowiec
}

case object Torun extends City(
  slug   = "torun",
  labels = CityLabels(nominative = "Toruń", genitivePlural = "toruńskich", locative = "Toruniu"),
  lat    = 53.0138,
  lon    = 18.5984,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.torun
}

case object Kielce extends City(
  slug   = "kielce",
  labels = CityLabels(nominative = "Kielce", genitivePlural = "kieleckich", locative = "Kielcach"),
  lat    = 50.8661,
  lon    = 20.6286,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.kielce
}

case object Rzeszow extends City(
  slug   = "rzeszow",
  labels = CityLabels(nominative = "Rzeszów", genitivePlural = "rzeszowskich", locative = "Rzeszowie"),
  lat    = 50.0413,
  lon    = 21.9990,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.rzeszow
}

case object Gliwice extends City(
  slug   = "gliwice",
  labels = CityLabels(nominative = "Gliwice", genitivePlural = "gliwickich", locative = "Gliwicach"),
  lat    = 50.2945,
  lon    = 18.6714,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.gliwice
}

case object Zabrze extends City(
  slug   = "zabrze",
  labels = CityLabels(nominative = "Zabrze", genitivePlural = "zabrzańskich", locative = "Zabrzu"),
  lat    = 50.3249,
  lon    = 18.7857,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.zabrze
}

// ── New mid-size cities (national-chain coverage) ─────────────────────────────

case object Olsztyn extends City(
  slug   = "olsztyn",
  labels = CityLabels(nominative = "Olsztyn", genitivePlural = "olsztyńskich", locative = "Olsztynie"),
  lat    = 53.7784,
  lon    = 20.4801,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.olsztyn
}

case object BielskoBiala extends City(
  slug   = "bielsko-biala",
  labels = CityLabels(nominative = "Bielsko-Biała", genitivePlural = "bielskich", locative = "Bielsku-Białej"),
  lat    = 49.8224,
  lon    = 19.0584,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.bielskoBiala
}

case object Opole extends City(
  slug   = "opole",
  labels = CityLabels(nominative = "Opole", genitivePlural = "opolskich", locative = "Opolu"),
  lat    = 50.6751,
  lon    = 17.9213,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.opole
}

case object Rybnik extends City(
  slug   = "rybnik",
  labels = CityLabels(nominative = "Rybnik", genitivePlural = "rybnickich", locative = "Rybniku"),
  lat    = 50.0971,
  lon    = 18.5416,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.rybnik
}

case object GorzowWielkopolski extends City(
  slug   = "gorzow-wielkopolski",
  labels = CityLabels(nominative = "Gorzów Wielkopolski", genitivePlural = "gorzowskich", locative = "Gorzowie Wielkopolskim"),
  lat    = 52.7368,
  lon    = 15.2288,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.gorzow
}

case object Elblag extends City(
  slug   = "elblag",
  labels = CityLabels(nominative = "Elbląg", genitivePlural = "elbląskich", locative = "Elblągu"),
  lat    = 54.1522,
  lon    = 19.4088,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.elblag
}

case object Koszalin extends City(
  slug   = "koszalin",
  labels = CityLabels(nominative = "Koszalin", genitivePlural = "koszalińskich", locative = "Koszalinie"),
  lat    = 54.1943,
  lon    = 16.1722,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.koszalin
}

case object Kalisz extends City(
  slug   = "kalisz",
  labels = CityLabels(nominative = "Kalisz", genitivePlural = "kaliskich", locative = "Kaliszu"),
  lat    = 51.7611,
  lon    = 18.0911,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.kalisz
}

case object ZielonaGora extends City(
  slug   = "zielona-gora",
  labels = CityLabels(nominative = "Zielona Góra", genitivePlural = "zielonogórskich", locative = "Zielonej Górze"),
  lat    = 51.9356,
  lon    = 15.5062,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.zielonaGora
}

case object Tychy extends City(
  slug   = "tychy",
  labels = CityLabels(nominative = "Tychy", genitivePlural = "tyskich", locative = "Tychach"),
  lat    = 50.1357,
  lon    = 18.9985,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.tychy
}

case object Walbrzych extends City(
  slug   = "walbrzych",
  labels = CityLabels(nominative = "Wałbrzych", genitivePlural = "wałbrzyskich", locative = "Wałbrzychu"),
  lat    = 50.7714,
  lon    = 16.2845,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.walbrzych
}

case object Tarnow extends City(
  slug   = "tarnow",
  labels = CityLabels(nominative = "Tarnów", genitivePlural = "tarnowskich", locative = "Tarnowie"),
  lat    = 50.0121,
  lon    = 20.9858,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.tarnow
}

case object Wloclawek extends City(
  slug   = "wloclawek",
  labels = CityLabels(nominative = "Włocławek", genitivePlural = "włocławskich", locative = "Włocławku"),
  lat    = 52.6483,
  lon    = 19.0677,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.wloclawek
}

case object Legnica extends City(
  slug   = "legnica",
  labels = CityLabels(nominative = "Legnica", genitivePlural = "legnickich", locative = "Legnicy"),
  lat    = 51.2070,
  lon    = 16.1619,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.legnica
}

case object Plock extends City(
  slug   = "plock",
  labels = CityLabels(nominative = "Płock", genitivePlural = "płockich", locative = "Płocku"),
  lat    = 52.5468,
  lon    = 19.7064,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.plock
}

case object Bytom extends City(
  slug   = "bytom",
  labels = CityLabels(nominative = "Bytom", genitivePlural = "bytomskich", locative = "Bytomiu"),
  lat    = 50.3483,
  lon    = 18.9157,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.bytom
}

case object DabrowaGornicza extends City(
  slug   = "dabrowa-gornicza",
  labels = CityLabels(nominative = "Dąbrowa Górnicza", genitivePlural = "dąbrowskich", locative = "Dąbrowie Górniczej"),
  lat    = 50.3219,
  lon    = 19.1876,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.dabrowaGornicza
}

case object NowySacz extends City(
  slug   = "nowy-sacz",
  labels = CityLabels(nominative = "Nowy Sącz", genitivePlural = "nowosądeckich", locative = "Nowym Sączu"),
  lat    = 49.6175,
  lon    = 20.7154,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.nowySacz
}

case object Slupsk extends City(
  slug   = "slupsk",
  labels = CityLabels(nominative = "Słupsk", genitivePlural = "słupskich", locative = "Słupsku"),
  lat    = 54.4641,
  lon    = 17.0287,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.slupsk
}

case object JeleniaGora extends City(
  slug   = "jelenia-gora",
  labels = CityLabels(nominative = "Jelenia Góra", genitivePlural = "jeleniogórskich", locative = "Jeleniej Górze"),
  lat    = 50.9044,
  lon    = 15.7197,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.jeleniaGora
}

case object Przemysl extends City(
  slug   = "przemysl",
  labels = CityLabels(nominative = "Przemyśl", genitivePlural = "przemyskich", locative = "Przemyślu"),
  lat    = 49.7838,
  lon    = 22.7677,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.przemysl
}

case object Konin extends City(
  slug   = "konin",
  labels = CityLabels(nominative = "Konin", genitivePlural = "konińskich", locative = "Koninie"),
  lat    = 52.2230,
  lon    = 18.2511,
  zoneId = TimeZones.Poland,
) {
  val cinemas: Seq[Cinema] = Cinema.konin
}

/** A Polish page that is not one of the 41 major cities: a town with a few
 *  venues of its own, or a cluster of small towns a short drive apart, named
 *  after the biggest ("Turek i okolice"). Generated — see [[PolishPages]] and
 *  `data/pl/scripts/build_pages.py` for which venues each one holds.
 *
 *  Its labels carry no genitive-plural adjective: the major cities' hand-declined
 *  "poznańskich" has no dependable equivalent for a village or a cluster, so the
 *  places that would use one speak of "kin w Turku i okolicach" instead (see
 *  [[genitivePluralLabel]] and `FilterDescription`). */
final class PolishPage private[models] (row: PolishPages.Row)
  extends City(row.slug, CityLabels(row.nominative, genitivePlural = "", row.locative), row.lat, row.lon,
               TimeZones.Poland) {
  val cinemas: Seq[Cinema] = row.cinemas
  /** The town the page is named after first, then the rest of its towns — the
   *  venue table alone would rank a cluster's one-venue towns alphabetically and
   *  could put Koło ahead of the Turek the page is named for. */
  override protected def extraPlaces: Seq[String] = (row.name +: VenueTowns.of(cinemas)).distinct
  /** "w Turku i okolicach" for a cluster, plain "w Słubicach" for one town. */
  override def locativePhrase: String = super.locativePhrase + (if (row.multiTown) " i okolicach" else "")
  def voivodeship: String = row.voivodeship
}

// ── United Kingdom (English — non-declining, so the three label slots all carry
//    the plain nominative; `CityGrammar.Nominative` reads only that). ──────────

/** A UK city or region — the shape most of the UK roster is: a COUNTY or a
 *  travel-shed rather than one town. `/aberdeenshire/` covers Aberdeen,
 *  Peterhead, Banchory, Huntly and Ellon; `/cheshire/` covers Chester,
 *  Warrington and Crewe. That is why they share a class at all: it is the one
 *  country whose venues are hand-written `case object`s, so its towns cannot
 *  ride along in a generated roster tuple the way Germany's, Spain's and the
 *  US's do — they come from [[VenueTowns]] instead, which every city reads by
 *  default, so this class does not have to say so.
 *
 *  What it is for now is the zone the 76 of them were each repeating.
 *
 *  Which venues each one lists is held to two distance rules by
 *  `UkPageGeographySpec` (see `data/uk/README.md`): a venue far from its own
 *  hub and close to another's is re-filed there, and two pages whose hubs are
 *  one urban area are merged — Dudley and Sandwell into [[Birmingham]],
 *  Lanarkshire into [[Glasgow]] ([[ukMergedPages]]). */
sealed abstract class UkCity(slug: String, labels: CityLabels, lat: Double, lon: Double)
  extends City(slug, labels, lat, lon, TimeZones.UnitedKingdom)

case object London extends UkCity("london",
  CityLabels("London", "London", "London"), 51.5074, -0.1278) {
  val cinemas: Seq[Cinema] = Cinema.london
  override val areas: Seq[CinemaAreaGroup] = Cinema.londonAreas
}
case object Manchester extends UkCity("manchester",
  CityLabels("Manchester", "Manchester", "Manchester"), 53.4808, -2.2426) {
  val cinemas: Seq[Cinema] = Cinema.manchester
}
case object Norwich extends UkCity("norwich",
  CityLabels("Norwich", "Norwich", "Norwich"), 52.6309, 1.2974) {
  val cinemas: Seq[Cinema] = Cinema.norwich
}
case object Aberdeenshire extends UkCity("aberdeenshire",
  CityLabels("Aberdeenshire", "Aberdeenshire", "Aberdeenshire"), 57.308, -2.3393) {
  val cinemas: Seq[Cinema] = Cinema.aberdeenshire
}
case object Antrim extends UkCity("antrim",
  CityLabels("Antrim", "Antrim", "Antrim"), 54.762, -6.0127) {
  val cinemas: Seq[Cinema] = Cinema.antrim
}
case object Armagh extends UkCity("armagh",
  CityLabels("Armagh", "Armagh", "Armagh"), 54.4492, -6.398) {
  val cinemas: Seq[Cinema] = Cinema.armagh
}
case object AyrshireAndArran extends UkCity("ayrshire-and-arran",
  CityLabels("Ayrshire and Arran", "Ayrshire and Arran", "Ayrshire and Arran"), 55.5093, -4.581) {
  val cinemas: Seq[Cinema] = Cinema.ayrshireAndArran
}
case object Bedfordshire extends UkCity("bedfordshire",
  CityLabels("Bedfordshire", "Bedfordshire", "Bedfordshire"), 52.0082, -0.4435) {
  val cinemas: Seq[Cinema] = Cinema.bedfordshire
}
case object Belfast extends UkCity("belfast",
  CityLabels("Belfast", "Belfast", "Belfast"), 54.5857, -5.9428) {
  val cinemas: Seq[Cinema] = Cinema.belfast
}
case object Berkshire extends UkCity("berkshire",
  CityLabels("Berkshire", "Berkshire", "Berkshire"), 51.4268, -0.9169) {
  val cinemas: Seq[Cinema] = Cinema.berkshire
}
case object Birmingham extends UkCity("birmingham",
  CityLabels("Birmingham", "Birmingham", "Birmingham"), 52.4581, -1.9041) {
  val cinemas: Seq[Cinema] = Cinema.birmingham
}
case object Bristol extends UkCity("bristol",
  CityLabels("Bristol", "Bristol", "Bristol"), 51.4659, -2.5805) {
  val cinemas: Seq[Cinema] = Cinema.bristol
}
case object Buckinghamshire extends UkCity("buckinghamshire",
  CityLabels("Buckinghamshire", "Buckinghamshire", "Buckinghamshire"), 51.7582, -0.7609) {
  val cinemas: Seq[Cinema] = Cinema.buckinghamshire
}
case object Cambridgeshire extends UkCity("cambridgeshire",
  CityLabels("Cambridgeshire", "Cambridgeshire", "Cambridgeshire"), 52.4301, -0.0137) {
  val cinemas: Seq[Cinema] = Cinema.cambridgeshire
}
case object Cardiff extends UkCity("cardiff",
  CityLabels("Cardiff", "Cardiff", "Cardiff"), 51.4892, -3.1939) {
  val cinemas: Seq[Cinema] = Cinema.cardiff
}
case object CentralScotland extends UkCity("central-scotland",
  CityLabels("Central Scotland", "Central Scotland", "Central Scotland"), 56.08, -3.8066) {
  val cinemas: Seq[Cinema] = Cinema.centralScotland
}
case object Cheshire extends UkCity("cheshire",
  CityLabels("Cheshire", "Cheshire", "Cheshire"), 53.2917, -2.4966) {
  val cinemas: Seq[Cinema] = Cinema.cheshire
}
case object Clwyd extends UkCity("clwyd",
  CityLabels("Clwyd", "Clwyd", "Clwyd"), 53.3083, -3.6072) {
  val cinemas: Seq[Cinema] = Cinema.clwyd
}
case object Cornwall extends UkCity("cornwall",
  CityLabels("Cornwall", "Cornwall", "Cornwall"), 50.317, -4.9211) {
  val cinemas: Seq[Cinema] = Cinema.cornwall
}
case object CountyDurham extends UkCity("county-durham",
  CityLabels("County Durham", "County Durham", "County Durham"), 54.7289, -1.5139) {
  val cinemas: Seq[Cinema] = Cinema.countyDurham
}
case object Cumbria extends UkCity("cumbria",
  CityLabels("Cumbria", "Cumbria", "Cumbria"), 54.4593, -3.1119) {
  val cinemas: Seq[Cinema] = Cinema.cumbria
}
case object Derbyshire extends UkCity("derbyshire",
  CityLabels("Derbyshire", "Derbyshire", "Derbyshire"), 52.9886, -1.5219) {
  val cinemas: Seq[Cinema] = Cinema.derbyshire
}
case object Devon extends UkCity("devon",
  CityLabels("Devon", "Devon", "Devon"), 50.6651, -3.687) {
  val cinemas: Seq[Cinema] = Cinema.devon
}
case object Dorset extends UkCity("dorset",
  CityLabels("Dorset", "Dorset", "Dorset"), 50.7664, -2.1122) {
  val cinemas: Seq[Cinema] = Cinema.dorset
}
case object Down extends UkCity("down",
  CityLabels("Down", "Down", "Down"), 54.4293, -5.9704) {
  val cinemas: Seq[Cinema] = Cinema.down
}
case object DumfriesAndGalloway extends UkCity("dumfries-and-galloway",
  CityLabels("Dumfries and Galloway", "Dumfries and Galloway", "Dumfries and Galloway"), 54.9881, -3.8232) {
  val cinemas: Seq[Cinema] = Cinema.dumfriesAndGalloway
}
case object DunbartonshireArgyllBute extends UkCity("dunbartonshire-argyll-bute",
  CityLabels("Dunbartonshire and Argyll & Bute", "Dunbartonshire and Argyll & Bute", "Dunbartonshire and Argyll & Bute"), 55.7795, -4.9973) {
  val cinemas: Seq[Cinema] = Cinema.dunbartonshireArgyllBute
}
case object Dyfed extends UkCity("dyfed",
  CityLabels("Dyfed", "Dyfed", "Dyfed"), 51.9892, -4.3329) {
  val cinemas: Seq[Cinema] = Cinema.dyfed
}
case object EastSussex extends UkCity("east-sussex",
  CityLabels("East Sussex", "East Sussex", "East Sussex"), 50.8499, 0.2215) {
  val cinemas: Seq[Cinema] = Cinema.eastSussex
}
case object EastYorkshire extends UkCity("east-yorkshire",
  CityLabels("East Yorkshire", "East Yorkshire", "East Yorkshire"), 53.8685, -0.3985) {
  val cinemas: Seq[Cinema] = Cinema.eastYorkshire
}
case object EdinburghAndLothians extends UkCity("edinburgh-and-lothians",
  CityLabels("Edinburgh & Lothians", "Edinburgh & Lothians", "Edinburgh & Lothians"), 55.9404, -3.2039) {
  val cinemas: Seq[Cinema] = Cinema.edinburghAndLothians
}
case object Essex extends UkCity("essex",
  CityLabels("Essex", "Essex", "Essex"), 51.7621, 0.5901) {
  val cinemas: Seq[Cinema] = Cinema.essex
}
case object Fermanagh extends UkCity("fermanagh",
  CityLabels("Fermanagh", "Fermanagh", "Fermanagh"), 54.3499, -7.6316) {
  val cinemas: Seq[Cinema] = Cinema.fermanagh
}
case object Fife extends UkCity("fife",
  CityLabels("Fife", "Fife", "Fife"), 56.1287, -3.2424) {
  val cinemas: Seq[Cinema] = Cinema.fife
}
case object Glamorgan extends UkCity("glamorgan",
  CityLabels("Glamorgan", "Glamorgan", "Glamorgan"), 51.6388, -3.7535) {
  val cinemas: Seq[Cinema] = Cinema.glamorgan
}
case object Glasgow extends UkCity("glasgow",
  CityLabels("Glasgow", "Glasgow", "Glasgow"), 55.8682, -4.2316) {
  val cinemas: Seq[Cinema] = Cinema.glasgow
}
case object Gloucestershire extends UkCity("gloucestershire",
  CityLabels("Gloucestershire", "Gloucestershire", "Gloucestershire"), 51.8387, -2.2712) {
  val cinemas: Seq[Cinema] = Cinema.gloucestershire
}
case object Guernsey extends UkCity("guernsey",
  CityLabels("Guernsey", "Guernsey", "Guernsey"), 49.4446, -2.5695) {
  val cinemas: Seq[Cinema] = Cinema.guernsey
}
case object Gwent extends UkCity("gwent",
  CityLabels("Gwent", "Gwent", "Gwent"), 51.6882, -3.0066) {
  val cinemas: Seq[Cinema] = Cinema.gwent
}
case object Gwynedd extends UkCity("gwynedd",
  CityLabels("Gwynedd", "Gwynedd", "Gwynedd"), 53.0098, -4.153) {
  val cinemas: Seq[Cinema] = Cinema.gwynedd
}
case object Hampshire extends UkCity("hampshire",
  CityLabels("Hampshire", "Hampshire", "Hampshire"), 50.9234, -1.165) {
  val cinemas: Seq[Cinema] = Cinema.hampshire
}
case object Herefordshire extends UkCity("herefordshire",
  CityLabels("Herefordshire", "Herefordshire", "Herefordshire"), 52.031, -2.7825) {
  val cinemas: Seq[Cinema] = Cinema.herefordshire
}
case object Hertfordshire extends UkCity("hertfordshire",
  CityLabels("Hertfordshire", "Hertfordshire", "Hertfordshire"), 51.7791, -0.3102) {
  val cinemas: Seq[Cinema] = Cinema.hertfordshire
}
case object HighlandsAndIslands extends UkCity("highlands-and-islands",
  CityLabels("Highlands and Islands", "Highlands and Islands", "Highlands and Islands"), 58.086, -4.0855) {
  val cinemas: Seq[Cinema] = Cinema.highlandsAndIslands
}
case object IsleOfMan extends UkCity("isle-of-man",
  CityLabels("Isle of Man", "Isle of Man", "Isle of Man"), 54.1578, -4.4775) {
  val cinemas: Seq[Cinema] = Cinema.isleOfMan
}
case object IsleOfWight extends UkCity("isle-of-wight",
  CityLabels("Isle of Wight", "Isle of Wight", "Isle of Wight"), 50.7118, -1.2248) {
  val cinemas: Seq[Cinema] = Cinema.isleOfWight
}
case object Jersey extends UkCity("jersey",
  CityLabels("Jersey", "Jersey", "Jersey"), 49.1839, -2.1144) {
  val cinemas: Seq[Cinema] = Cinema.jersey
}
case object Kent extends UkCity("kent",
  CityLabels("Kent", "Kent", "Kent"), 51.2682, 0.8631) {
  val cinemas: Seq[Cinema] = Cinema.kent
}
case object Lancashire extends UkCity("lancashire",
  CityLabels("Lancashire", "Lancashire", "Lancashire"), 53.7367, -2.6625) {
  val cinemas: Seq[Cinema] = Cinema.lancashire
}
case object Leicestershire extends UkCity("leicestershire",
  CityLabels("Leicestershire", "Leicestershire", "Leicestershire"), 52.6656, -1.1514) {
  val cinemas: Seq[Cinema] = Cinema.leicestershire
}
case object Lincolnshire extends UkCity("lincolnshire",
  CityLabels("Lincolnshire", "Lincolnshire", "Lincolnshire"), 53.2194, -0.2916) {
  val cinemas: Seq[Cinema] = Cinema.lincolnshire
}
case object Londonderry extends UkCity("londonderry",
  CityLabels("Londonderry", "Londonderry", "Londonderry"), 54.9949, -7.0636) {
  val cinemas: Seq[Cinema] = Cinema.londonderry
}
case object Liverpool extends UkCity("liverpool",
  CityLabels("Liverpool", "Liverpool", "Liverpool"), 53.4084, -2.9916) {
  val cinemas: Seq[Cinema] = Cinema.liverpool
}
case object NorthYorkshire extends UkCity("north-yorkshire",
  CityLabels("North Yorkshire", "North Yorkshire", "North Yorkshire"), 54.2402, -1.156) {
  val cinemas: Seq[Cinema] = Cinema.northYorkshire
}
case object Northamptonshire extends UkCity("northamptonshire",
  CityLabels("Northamptonshire", "Northamptonshire", "Northamptonshire"), 52.288, -0.8653) {
  val cinemas: Seq[Cinema] = Cinema.northamptonshire
}
case object Northumberland extends UkCity("northumberland",
  CityLabels("Northumberland", "Northumberland", "Northumberland"), 55.2158, -1.7422) {
  val cinemas: Seq[Cinema] = Cinema.northumberland
}
case object Nottinghamshire extends UkCity("nottinghamshire",
  CityLabels("Nottinghamshire", "Nottinghamshire", "Nottinghamshire"), 53.0236, -1.15) {
  val cinemas: Seq[Cinema] = Cinema.nottinghamshire
}
case object Oxfordshire extends UkCity("oxfordshire",
  CityLabels("Oxfordshire", "Oxfordshire", "Oxfordshire"), 51.7572, -1.2545) {
  val cinemas: Seq[Cinema] = Cinema.oxfordshire
}
case object Powys extends UkCity("powys",
  CityLabels("Powys", "Powys", "Powys"), 52.3806, -3.26) {
  val cinemas: Seq[Cinema] = Cinema.powys
}
case object Renfrewshire extends UkCity("renfrewshire",
  CityLabels("Renfrewshire", "Renfrewshire", "Renfrewshire"), 55.9204, -4.5838) {
  val cinemas: Seq[Cinema] = Cinema.renfrewshire
}
case object RoxburghEttrickAndLauderdale extends UkCity("roxburgh-ettrick-and-lauderdale",
  CityLabels("Roxburgh, Ettrick and Lauderdale", "Roxburgh, Ettrick and Lauderdale", "Roxburgh, Ettrick and Lauderdale"), 55.5183, -2.7969) {
  val cinemas: Seq[Cinema] = Cinema.roxburghEttrickAndLauderdale
}
case object Shropshire extends UkCity("shropshire",
  CityLabels("Shropshire", "Shropshire", "Shropshire"), 52.6813, -2.6215) {
  val cinemas: Seq[Cinema] = Cinema.shropshire
}
case object Somerset extends UkCity("somerset",
  CityLabels("Somerset", "Somerset", "Somerset"), 51.2159, -2.824) {
  val cinemas: Seq[Cinema] = Cinema.somerset
}
case object SouthYorkshire extends UkCity("south-yorkshire",
  CityLabels("South Yorkshire", "South Yorkshire", "South Yorkshire"), 53.5141, -1.3109) {
  val cinemas: Seq[Cinema] = Cinema.southYorkshire
}
case object Staffordshire extends UkCity("staffordshire",
  CityLabels("Staffordshire", "Staffordshire", "Staffordshire"), 52.7942, -1.9887) {
  val cinemas: Seq[Cinema] = Cinema.staffordshire
}
case object Suffolk extends UkCity("suffolk",
  CityLabels("Suffolk", "Suffolk", "Suffolk"), 52.1492, 1.0262) {
  val cinemas: Seq[Cinema] = Cinema.suffolk
}
case object Surrey extends UkCity("surrey",
  CityLabels("Surrey", "Surrey", "Surrey"), 51.2269, -0.5354) {
  val cinemas: Seq[Cinema] = Cinema.surrey
}
case object Tayside extends UkCity("tayside",
  CityLabels("Tayside", "Tayside", "Tayside"), 56.5061, -3.0128) {
  val cinemas: Seq[Cinema] = Cinema.tayside
}
case object TyneAndWear extends UkCity("tyne-and-wear",
  CityLabels("Tyne and Wear", "Tyne and Wear", "Tyne and Wear"), 54.9749, -1.5397) {
  val cinemas: Seq[Cinema] = Cinema.tyneAndWear
}
case object Tyrone extends UkCity("tyrone",
  CityLabels("Tyrone", "Tyrone", "Tyrone"), 54.5255, -6.8664) {
  val cinemas: Seq[Cinema] = Cinema.tyrone
}
case object Warwickshire extends UkCity("warwickshire",
  CityLabels("Warwickshire", "Warwickshire", "Warwickshire"), 52.3602, -1.5034) {
  val cinemas: Seq[Cinema] = Cinema.warwickshire
}
case object WestSussex extends UkCity("west-sussex",
  CityLabels("West Sussex", "West Sussex", "West Sussex"), 50.9492, -0.3262) {
  val cinemas: Seq[Cinema] = Cinema.westSussex
}
case object WestYorkshire extends UkCity("west-yorkshire",
  CityLabels("West Yorkshire", "West Yorkshire", "West Yorkshire"), 53.7878, -1.665) {
  val cinemas: Seq[Cinema] = Cinema.westYorkshire
}
case object Wiltshire extends UkCity("wiltshire",
  CityLabels("Wiltshire", "Wiltshire", "Wiltshire"), 51.2955, -1.8505) {
  val cinemas: Seq[Cinema] = Cinema.wiltshire
}
case object Worcestershire extends UkCity("worcestershire",
  CityLabels("Worcestershire", "Worcestershire", "Worcestershire"), 52.1923, -2.2079) {
  val cinemas: Seq[Cinema] = Cinema.worcestershire
}
case object Yorkshire extends UkCity("yorkshire",
  CityLabels("Yorkshire", "Yorkshire", "Yorkshire"), 53.4082, -1.4756) {
  val cinemas: Seq[Cinema] = Cinema.yorkshire
}

// ── Germany (also non-declining for our purposes). ───────────────────────────

/** A German page — a major city (a municipality of 100,000+) with only its own
 *  venues, a town with three or more, or a cluster of small towns a short drive
 *  apart named after the biggest: "Bayreuth und Umgebung", "in Bayreuth und
 *  Umgebung". Poland's shape ([[PolishPage]]), built by the same rule
 *  (`data/germany/scripts/build_pages.py`); until 2026-10 a German page was a
 *  REGION of every venue within ~35 km of a hub, and `/koeln/` listed Bonn.
 *
 *  The roster (1,515 cinemas over 436 pages) is generated into `GermanRosterData`
 *  and materialised by [[GermanRoster]]; instances are built ONCE (in
 *  `City.germanPages`), so identity equality holds just like the hand-authored
 *  `case object` cities. One zone for the country, `Europe/Berlin`. */
final class GermanPage private[models] (slug: String, place: GermanPlace)
  extends City(slug, GermanPage.labels(place), place.lat, place.lon, TimeZones.Germany) {
  val cinemas: Seq[Cinema] = place.cinemas
  /** The town the page is named after first, then the rest of its towns. */
  override protected val extraPlaces: Seq[String] = (place.name +: place.towns).distinct
  /** The Bundesland the picker groups this page under. */
  def bundesland: String = place.bundesland
}

private[models] object GermanPage {
  /** German does not decline a place name after "in", so all three slots carry
   *  the page's name — "<town> und Umgebung" for a cluster, which reads right both
   *  alone and after the grammar's "in" ("in Bayreuth und Umgebung"). */
  def labels(place: GermanPlace): CityLabels = {
    val name = if (place.multiTown) s"${place.name} und Umgebung" else place.name
    CityLabels(name, name, name)
  }
}

/** A US page — a major city (a place of 100,000+) with only its own venues, a
 *  town with three or more, or a cluster of small towns a short drive apart named
 *  after the biggest: "Bend area", "in the Bend area". Poland's shape
 *  ([[PolishPage]]), built by the same rule (`data/us/scripts/build_pages.py`);
 *  until 2026-10 a US page was a METRO of every venue within 75 km of a hub, cut
 *  at state lines, and `/los-angeles/` listed Lancaster. A cluster may cross a
 *  state line; the picker files it under its anchor's state.
 *
 *  The roster (5,010 cinemas over 2,001 pages) is generated into `UsRosterData`
 *  and materialised by [[UsRoster]]; instances are built ONCE (in
 *  `City.usCities`), so identity equality holds just like the hand-authored
 *  `case object` cities.
 *
 *  Unlike [[GermanPage]] the zone is the PLACE's rather than a constant: the US
 *  spans six of them, and a page keeps the clock most of its venues keep. */
final class UsCity private[models] (slug: String, place: UsPlace)
  extends City(slug, UsCity.labels(place), place.lat, place.lon, place.zoneId) {
  val cinemas: Seq[Cinema] = place.cinemas
  /** The town the page is named after first, then the rest of its towns and the
   *  towns its venues name (Brooklyn, Hollywood). */
  override protected val extraPlaces: Seq[String] = place.towns
  /** "in the Bend area" — an area takes the article a town does not. */
  override def locativePhrase: String = if (place.multiTown) s"in the ${labels.nominative}" else super.locativePhrase
  /** The state the picker groups this page under: its anchor town's. */
  def stateName: String = place.stateName
}

private[models] object UsCity {
  /** English does not decline a place name, so all three slots carry the page's
   *  name — "<town> area" for a cluster, which reads right alone ("Bend area")
   *  and in a list of cinemas ("All Bend area cinema listings"). */
  def labels(place: UsPlace): CityLabels = {
    val name = if (place.multiTown) s"${place.name} area" else place.name
    CityLabels(name, name, name)
  }
}

/** A Spanish page — a major city (one of the 43 municipalities of 150,000+) with
 *  only its own venues, a town with three or more, or a cluster of small towns a
 *  short drive apart named after the biggest: "Sitges y alrededores", "en Sitges y
 *  alrededores". Poland's shape ([[PolishPage]]), built by the same rule
 *  (`data/spain/scripts/build_pages.py`); until 2026-10 a Spanish page was a
 *  whole province, and `/barcelona/` listed Berga, 100 km away.
 *
 *  The roster (599 cinemas over 240 pages) is generated into `SpanishRosterData`
 *  and materialised by [[SpanishRoster]]; instances are built ONCE (in
 *  `City.spanishCities`), so identity equality holds just like the hand-authored
 *  `case object` cities.
 *
 *  Like [[UsCity]] and unlike [[GermanPage]] the zone is a CONSTRUCTOR
 *  parameter: the Canary Islands run an hour behind the peninsula, and a single
 *  national zone would move their day boundary. */
final class SpanishPage private[models] (slug: String, place: SpanishPlace)
  extends City(slug, SpanishPage.labels(place), place.lat, place.lon, place.zoneId) {
  val cinemas: Seq[Cinema] = place.cinemas
  /** The town the page is named after first, then the rest of its towns. */
  override protected val extraPlaces: Seq[String] = (place.name +: place.towns).distinct
  /** The province the picker groups this page under. */
  def province: String = place.province
}

private[models] object SpanishPage {
  /** Spanish does not decline a place name, so all three slots carry the page's
   *  name — "<town> y alrededores" for a cluster, which reads right both alone and
   *  after the grammar's "en". */
  def labels(place: SpanishPlace): CityLabels = {
    val name = if (place.multiTown) s"${place.name} y alrededores" else place.name
    CityLabels(name, name, name)
  }
}

/** A named group of [[City]]s in a picker — a US STATE over its towns
 *  ("California → Los Angeles", the way a visitor finds one), or a UK NATION
 *  over its counties ("Scotland → Fife"), a Polish voivodeship or a Spanish
 *  province over its towns. Ordinarily the group is not a place you can open:
 *  `/california/` is nothing, and its films live at `/los-angeles/`. It is how
 *  the list of them is arranged, and nothing else.
 *
 *  The exception is [[soleCity]] — see there.
 *
 *  A country whose picker is one flat list leaves `Country.cityGroups` empty
 *  rather than declaring one group per city.
 *
 *  @param absorbed names of former pages now listed by this group's one place —
 *                  the UK's [[City.ukMergedPages]] — kept findable by [[searchAliases]]. */
final case class CityGroup(label: String, slug: String, cities: Seq[City] = Nil,
                          groups: Seq[CityGroup] = Nil, absorbed: Seq[String] = Nil) {

  /** The one place this group holds, pulled up a level — and `None` for a group
   *  that really arranges several.
   *
   *  A heading you open to find exactly one row underneath is a tap that buys
   *  the reader nothing, whatever the two are called. Most UK counties ARE the
   *  Flicks region under them (Cheshire, Kent), so those collapse onto a name
   *  they already share. The rest collapse onto a name they do not: Merseyside
   *  shows as Liverpool, Greater Manchester as Manchester, the District of
   *  Columbia as Washington, Saarland
   *  as Saarbrücken.
   *
   *  The group's own name is NOT lost when it differs — the picker keeps it on
   *  the row as a search alias (`data-alias`), so "merseyside" still finds
   *  Liverpool. Dropping the heading is meant to save a tap, not to make the
   *  county unfindable, and the search is where that name was doing its work.
   *
   *  Nil `groups` is part of the test: a node with one city AND a subgroup under
   *  it is still arranging two things.
   *
   *  [[slug]] is deliberately NOT consulted. An earlier version required the
   *  city's slug to equal the group's, which collapsed Cheshire and left
   *  Merseyside, Greater Manchester, Norfolk and Saarland as headings over a
   *  single row — the tap this exists to remove. Re-adding that condition would
   *  silently restore all four; `CountrySpec` pins each of them by name. */
  def soleCity: Option[City] = (cities, groups) match {
    case (Seq(only), Nil) => Some(only)
    case _                => None
  }

  /** The name the picker actually SHOWS for this node: the place's, where the
   *  group collapsed onto one, and the group's own otherwise.
   *
   *  What a list of these must be sorted by. Sorting on [[label]] would order
   *  Merseyside and Greater Manchester under M and G while displaying Liverpool
   *  and Manchester — an alphabetical list that visibly is not one, which is
   *  precisely the failure the ordering exists to prevent. */
  def displayLabel: String = soleCity.map(_.labels.nominative).getOrElse(label)

  /** The group's own name, where it differs from the single place it collapsed
   *  onto — what the picker keeps searchable after pulling that place up, and
   *  `None` when there is nothing extra to remember (Cheshire under Cheshire) or
   *  when the group did not collapse at all. */
  def collapsedAlias: Option[String] =
    soleCity.filter(_.labels.nominative != label).map(_ => label)

  /** Every other name the picker should find this row by: [[collapsedAlias]], then
   *  the names of the pages [[absorbed]] into its one place. Empty for a group
   *  that did not collapse — its heading is already on screen. */
  def searchAliases: Seq[String] = soleCity.fold(Seq.empty[String])(_ => collapsedAlias.toSeq ++ absorbed)

  /** Every city under this group at any depth — its own, plus its subgroups'.
   *
   *  A picker nests (a UK nation over its counties over their places), but the
   *  things that read a grouping FLAT do not: `<optgroup>` may not contain
   *  another, and the apps' `region` is one string per city. Both take the top
   *  level's label over this, so a deeper tree arranges the page without
   *  changing what they get. */
  def allCities: Seq[City] = cities ++ groups.flatMap(_.allCities)
}

object City {

  /** Poland's cities — the authoritative list for [[Country.Poland]]. [[all]] is
   *  the union across every [[Country]], so a new country contributes its own
   *  list (e.g. `ukCities`) here and [[all]] picks it up automatically. */
  private[models] val majorPolishCities: Seq[City] = Seq(
    Poznan, Wroclaw, Warszawa, Krakow, Lodz, Katowice, Szczecin, Bialystok, Trojmiasto, Bydgoszcz, Lublin,
    Czestochowa, Radom, Sosnowiec, Torun, Kielce, Rzeszow, Gliwice, Zabrze,
    Olsztyn, BielskoBiala, Opole, Rybnik, GorzowWielkopolski, Elblag, Koszalin, Kalisz, ZielonaGora, Tychy,
    Walbrzych, Tarnow, Wloclawek, Legnica, Plock, Bytom, DabrowaGornicza, NowySacz, Slupsk, JeleniaGora,
    Przemysl, Konin,
  )
  /** Every Polish page that is not a major city — see [[PolishPage]]. */
  private[models] lazy val polishPages: Seq[PolishPage] = PolishPages.rows.map(new PolishPage(_))
  private[models] lazy val polishCities: Seq[City] = majorPolishCities ++ polishPages

  /** Poland's picker: one group per voivodeship, holding its major cities and its
   *  town and cluster pages side by side, alphabetically — a flat list of 180
   *  pages is not one anybody reads. */
  private[models] lazy val polishVoivodeships: Seq[CityGroup] = {
    val pl = Locale.forLanguageTag("pl-PL")
    val voivodeshipOf: City => String = {
      case page: PolishPage => page.voivodeship
      case major            => PolishPages.majorVoivodeship(major.slug)
    }
    val built = polishCities.groupBy(voivodeshipOf).toSeq.map { case (name, cities) =>
      CityGroup(name, Slugify.stable(name), CityListing.sorted(cities, pl))
    }
    CityListing.sortedLabels(built, pl)(_.displayLabel)
  }

  /** The United Kingdom's full modelled roster — every Flicks region we know how
   *  to scrape (76), less the ones folded into a neighbour ([[ukMergedPages]]). Retained in full even while only a subset is live so that
   *  bringing a city back online is a one-line edit to [[activeUkCities]], never
   *  a re-declaration. Do NOT trim this list to disable cities — narrow
   *  [[activeUkCities]] instead. */
  private[models] val allUkCities: Seq[City] = Seq(
    London, Manchester, Norwich, Aberdeenshire, Antrim, Armagh, AyrshireAndArran, Bedfordshire, Belfast, Berkshire, Birmingham, Bristol, Buckinghamshire, Cambridgeshire, Cardiff, CentralScotland, Cheshire, Clwyd, Cornwall, CountyDurham, Cumbria, Derbyshire, Devon, Dorset, Down, DumfriesAndGalloway, DunbartonshireArgyllBute, Dyfed, EastSussex, EastYorkshire, EdinburghAndLothians, Essex, Fermanagh, Fife, Glamorgan, Glasgow, Gloucestershire, Guernsey, Gwent, Gwynedd, Hampshire, Herefordshire, Hertfordshire, HighlandsAndIslands, IsleOfMan, IsleOfWight, Jersey, Kent, Lancashire, Leicestershire, Lincolnshire, Londonderry, Liverpool, NorthYorkshire, Northamptonshire, Northumberland, Nottinghamshire, Oxfordshire, Powys, Renfrewshire, RoxburghEttrickAndLauderdale, Shropshire, Somerset, SouthYorkshire, Staffordshire, Suffolk, Surrey, Tayside, TyneAndWear, Tyrone, Warwickshire, WestSussex, WestYorkshire, Wiltshire, Worcestershire, Yorkshire,
  )

  /** The UK cities currently live — the ones web serves and the worker scrapes.
   *  Currently the FULL modelled roster: every Flicks region is live. To scope
   *  the live set down again, narrow this to a subset of [[allUkCities]] (e.g.
   *  `Set(London, Manchester, …)`); the disabled cities stay fully modelled
   *  above, so re-enabling is always a one-line edit here — never a
   *  re-declaration. */
  private[models] val activeUkCities: Set[City] = allUkCities.toSet

  /** Flicks regions that are not pages of their own, by name, and the page that
   *  lists their venues instead. Each one's hub was under 15 km from its
   *  neighbour's — two halves of one urban area, which `UkPageGeographySpec`
   *  holds no two pages may be: Dudley (13 km) and Sandwell (9 km) are boroughs
   *  of the Birmingham conurbation, and Lanarkshire's hub sits between East
   *  Kilbride and Hamilton, 12 km from Glasgow's.
   *
   *  The ONE place the merge is said: their old slugs redirect from here (see
   *  [[renamedSlugs]]), and the picker keeps each name as a search alias on
   *  the row that absorbed it (see [[CityGroup.absorbed]]). */
  private[models] val ukMergedPages: Seq[(String, City)] = Seq(
    "Dudley"      -> Birmingham,
    "Sandwell"    -> Birmingham,
    "Lanarkshire" -> Glasgow,
  )

  /** The authoritative UK list for [[Country.UnitedKingdom]] — the live subset
   *  of [[allUkCities]], kept in that list's declared order. */
  private[models] val ukCities: Seq[City] = allUkCities.filter(activeUkCities)

  /** The UK picker's grouping: NATION over COUNTY over the places inside it.
   *
   *  76 places is past what anyone reads as one A-to-Z — the same problem the US
   *  has at 468 — but the UK's answer is levels UP rather than one down. A Flicks
   *  region is usually already the county ("Cheshire", "Cornwall", "Kent"), so
   *  there is nothing to cut those into; what the list wanted was the county over
   *  them and the nation over that.
   *
   *  Most counties therefore stand exactly where their one place stands and
   *  collapse back into it — see [[CityGroup.soleCity]]. The level earns its keep
   *  on the entries that are a CITY rather than a county: Cardiff and Belfast sit
   *  beside the rest of Glamorgan and Antrim, and Manchester is where it is
   *  because Greater Manchester is.
   *
   *  Declared as a table rather than derived, because nothing in a `UkCity` knows
   *  its county or its nation: the roster is hand-authored `case object`s
   *  carrying a slug, a label and a fix, and neither fact is one those imply.
   *  Nations are in this fixed reading order (England, Scotland, Northern
   *  Ireland, Wales, Crown Dependencies) — the same order every client's picker
   *  shows, kept in one place rather than re-derived per platform — and
   *  everything below is alphabetical, which is the only order a reader can
   *  search by eye.
   *
   *  Every LIVE city appears exactly once (`CountrySpec` holds that, the same
   *  partition it holds over the US states — a place reachable from no heading is
   *  a place nobody can find), so narrowing [[activeUkCities]] drops rows here
   *  without leaving an empty county or nation standing. */
  private[models] val ukNations: Seq[CityGroup] = {
    // (nation, nation slug, (county, its places)*). A county named for the place
    // it holds is the common case and collapses away; the ones that group
    // something are the point of the level.
    val UkLocale = Locale.forLanguageTag("en-GB")
    val table: Seq[(String, String, Seq[(String, Seq[City])])] = Seq(
      ("England", "england", Seq(
        "Bedfordshire"      -> Seq(Bedfordshire),
        "Berkshire"         -> Seq(Berkshire),
        "Bristol"           -> Seq(Bristol),
        "Buckinghamshire"   -> Seq(Buckinghamshire),
        "Cambridgeshire"    -> Seq(Cambridgeshire),
        "Cheshire"          -> Seq(Cheshire),
        "Cornwall"          -> Seq(Cornwall),
        "County Durham"     -> Seq(CountyDurham),
        "Cumbria"           -> Seq(Cumbria),
        "Derbyshire"        -> Seq(Derbyshire),
        "Devon"             -> Seq(Devon),
        "Dorset"            -> Seq(Dorset),
        "East Sussex"       -> Seq(EastSussex),
        "East Yorkshire"    -> Seq(EastYorkshire),
        "Essex"             -> Seq(Essex),
        "Gloucestershire"   -> Seq(Gloucestershire),
        "Greater London"    -> Seq(London),
        "Greater Manchester"-> Seq(Manchester),
        "Hampshire"         -> Seq(Hampshire),
        "Herefordshire"     -> Seq(Herefordshire),
        "Hertfordshire"     -> Seq(Hertfordshire),
        "Isle of Wight"     -> Seq(IsleOfWight),
        "Kent"              -> Seq(Kent),
        "Lancashire"        -> Seq(Lancashire),
        "Leicestershire"    -> Seq(Leicestershire),
        "Lincolnshire"      -> Seq(Lincolnshire),
        "Merseyside"        -> Seq(Liverpool),
        "Norfolk"           -> Seq(Norwich),
        "North Yorkshire"   -> Seq(NorthYorkshire),
        "Northamptonshire"  -> Seq(Northamptonshire),
        "Northumberland"    -> Seq(Northumberland),
        "Nottinghamshire"   -> Seq(Nottinghamshire),
        "Oxfordshire"       -> Seq(Oxfordshire),
        "Shropshire"        -> Seq(Shropshire),
        "Somerset"          -> Seq(Somerset),
        "South Yorkshire"   -> Seq(SouthYorkshire),
        "Staffordshire"     -> Seq(Staffordshire),
        "Suffolk"           -> Seq(Suffolk),
        "Surrey"            -> Seq(Surrey),
        "Tyne and Wear"     -> Seq(TyneAndWear),
        "Warwickshire"      -> Seq(Warwickshire),
        "West Midlands"     -> Seq(Birmingham),
        "West Sussex"       -> Seq(WestSussex),
        "West Yorkshire"    -> Seq(WestYorkshire),
        "Wiltshire"         -> Seq(Wiltshire),
        "Worcestershire"    -> Seq(Worcestershire),
        // Flicks carries a catch-all Yorkshire region beside the four ridings.
        "Yorkshire"         -> Seq(Yorkshire),
      )),
      ("Scotland", "scotland", Seq(
        "Aberdeenshire"                    -> Seq(Aberdeenshire),
        "Ayrshire and Arran"               -> Seq(AyrshireAndArran),
        "Central Scotland"                 -> Seq(CentralScotland),
        "Dumfries and Galloway"            -> Seq(DumfriesAndGalloway),
        "Dunbartonshire and Argyll & Bute" -> Seq(DunbartonshireArgyllBute),
        "Edinburgh & Lothians"             -> Seq(EdinburghAndLothians),
        "Fife"                             -> Seq(Fife),
        "Glasgow"                          -> Seq(Glasgow),
        "Highlands and Islands"            -> Seq(HighlandsAndIslands),
        "Renfrewshire"                     -> Seq(Renfrewshire),
        "Roxburgh Ettrick and Lauderdale"  -> Seq(RoxburghEttrickAndLauderdale),
        "Tayside"                          -> Seq(Tayside),
      )),
      ("Northern Ireland", "northern-ireland", Seq(
        // Belfast straddles Antrim and Down; County Antrim holds the bulk of it
        // and is where it is conventionally filed.
        "Antrim"      -> Seq(Antrim, Belfast),
        "Armagh"      -> Seq(Armagh),
        "Down"        -> Seq(Down),
        "Fermanagh"   -> Seq(Fermanagh),
        "Londonderry" -> Seq(Londonderry),
        "Tyrone"      -> Seq(Tyrone),
      )),
      ("Wales", "wales", Seq(
        "Clwyd"     -> Seq(Clwyd),
        "Dyfed"     -> Seq(Dyfed),
        // Cardiff is in Glamorgan, which Flicks also carries as a region of its
        // own — so the county holds the city and the rest of itself.
        "Glamorgan" -> Seq(Cardiff, Glamorgan),
        "Gwent"     -> Seq(Gwent),
        "Gwynedd"   -> Seq(Gwynedd),
        "Powys"     -> Seq(Powys),
      )),
      // Not part of the UK, but served by the same Flicks market and reached from
      // the same picker, so they need a heading rather than a nation to be filed
      // under wrongly.
      ("Crown Dependencies", "crown-dependencies", Seq(
        "Guernsey"    -> Seq(Guernsey),
        "Isle of Man" -> Seq(IsleOfMan),
        "Jersey"      -> Seq(Jersey),
      )),
    )
    table.map { case (nation, nationSlug, counties) =>
      val kept = counties
        .map { case (county, places) =>
          CityGroup(county, Slugify.stable(county),
                    CityListing.sorted(places.filter(activeUkCities), UkLocale),
                    absorbed = places.flatMap(place => ukMergedPages.collect { case (name, `place`) => name }))
        }
        .filter(_.cities.nonEmpty)
      // By what each row SHOWS, not by the county it came from: a county that
      // collapsed onto its one place displays that place's name, and sorting on
      // the county would file Manchester under "Greater Manchester".
      CityGroup(nation, nationSlug, Nil, CityListing.sortedLabels(kept, UkLocale)(_.displayLabel))
    }.filter(_.groups.nonEmpty)
  }

  /** Germany's cities — the authoritative list for [[Country.Germany]]. One per
   *  town or cluster page (see [[GermanPage]] / [[GermanRoster]]), materialised
   *  data-driven from `GermanRosterData` rather than hand-authored case objects.
   *
   *  The slug is assigned HERE rather than in [[GermanRoster]] for the reason
   *  [[usCities]] spells out: `City.bySlug` is a single global namespace. */
  private[models] val germanPages: Seq[GermanPage] =
    GermanRoster.places.map(p => new GermanPage(germanSlug(p.preferredSlug, p.qualifiedSlug), p))
  private[models] val germanCities: Seq[City] = germanPages

  /** Slugs another country serves or wants, which a German page may not take.
   *
   *  Germany's slugs are decided BEFORE Spain's (which qualifies around Germany's),
   *  so Spain's final slugs cannot be read here — its PREFERRED ones can. A German
   *  page that wants a slug a Spanish town wants yields it: the other page is the
   *  live URL, and the newcomer is the one that can still choose. None of the 139
   *  slugs the regions kept collides with one. The US is decided AFTER Germany and
   *  yields to it (`/potsdam/` is Brandenburg's, Potsdam, New York qualifies) —
   *  see [[usSlug]]. */
  private lazy val foreignToGermany: Set[String] =
    (polishCities ++ allUkCities).map(_.slug).toSet ++ SpanishRoster.places.map(_.preferredSlug)

  /** The URL slug of a German page — or of one that used to be (see
   *  [[germanSuccession]]), which answered at the slug this gives too: its own
   *  name wherever that is free, and its name qualified with its Bundesland where
   *  it is not, the way Spain's Toledo is `toledo-castilla-la-mancha`. Only the
   *  SLUG is qualified; the label the German picker shows stays the town. */
  private[models] def germanSlug(preferred: String, qualified: String): String =
    if (foreignToGermany.contains(preferred)) qualified else preferred

  /** German pages that no longer exist → the page holding the town each was
   *  named after, or — where that town has no venue left — the page now holding
   *  most of its venues. The 19 regions that did not leave a page under their own
   *  slug when Germany went from regions to towns (2026-10) among them: `/vechta/`
   *  → Cloppenburg (Vechta's cluster), `/itzehoe/` → Elmshorn. A region whose slug is a page now
   *  (`/koeln/`, `/freiburg/`) simply IS that town's page. */
  private lazy val germanSuccession: Map[String, String] =
    GermanRosterData.retired.map { case (preferred, qualified, page) =>
      val target = GermanRoster.places.find(_.preferredSlug == page)
        .fold(page)(p => germanSlug(p.preferredSlug, p.qualifiedSlug))
      germanSlug(preferred, qualified) -> target
    }.toMap

  /** The German picker's grouping: one entry per BUNDESLAND over the pages inside
   *  it — major cities, towns and clusters side by side — alphabetically both
   *  ways, as Poland's voivodeships and Spain's provinces. The Land is the
   *  anchor town's, read off its coordinates by `build_pages.py`. */
  private[models] lazy val germanStates: Seq[CityGroup] = {
    val de = Locale.forLanguageTag("de-DE")
    val built = germanPages.groupBy(_.bundesland).toSeq.map { case (land, pages) =>
      CityGroup(land, Slugify.stable(land), CityListing.sorted(pages, de))
    }
    // By the displayed name — a Land holding one page shows that page, and
    // sorting on the Land would misplace it.
    CityListing.sortedLabels(built, de)(_.displayLabel)
  }


  /** The United States' cities — the authoritative list for
   *  [[Country.UnitedStates]]. One per town or cluster page (see [[UsCity]] /
   *  [[UsRoster]]), materialised data-driven from `UsRosterData`. The state is a
   *  [[CityGroup]], not a city: nobody wants a screen of every cinema in Texas.
   *
   *  The slug is assigned HERE rather than in [[UsRoster]] because it is the one
   *  thing about a place that depends on every OTHER country: `City.bySlug` is a
   *  single global namespace, and a US town's name is not unique in it —
   *  "Birmingham" is a UK city. See [[usSlug]].
   *
   *  Lazy because it reads Spain's pages, declared below. */
  private[models] lazy val usCities: Seq[UsCity] =
    UsRoster.places.map(p => new UsCity(usSlug(p.preferredSlug, p.qualifiedSlug), p))

  /** Slugs another country serves or redirects, which a US page may not take. The
   *  US was re-cut into towns (2026-10) after Germany and Spain had theirs, so it
   *  is the newcomer that yields to them — except at the two Spain yielded to
   *  before ([[UsSlugsSpainYields]]). Retired slugs count: a US page at `/regen/`
   *  would be redirected to Deggendorf. */
  private lazy val foreignToUs: Set[String] =
    (polishCities ++ allUkCities ++ germanCities ++ spanishCities).map(_.slug).toSet ++
      PolishPages.retired.keys ++ ukMergedPages.map { case (name, _) => Slugify.stable(name) } ++
      germanSuccession.keys ++ spanishSuccession.keys

  /** The URL slug of a US page — or of one that used to be (see [[usSuccession]]),
   *  which answered at the slug this gives too: its own name wherever that is free
   *  (`build_pages.py` has already qualified a name another US page holds — two
   *  Springfields), and its name qualified with its state where another country
   *  has it: `/birmingham/` is a live UK page, so Alabama's is
   *  `/birmingham-alabama/`. Only the SLUG is qualified; the picker shows the town
   *  under its state's heading, where it is not ambiguous. */
  private def usSlug(preferred: String, qualified: String): String =
    if (foreignToUs.contains(preferred)) qualified else preferred

  /** US pages that no longer exist → the page holding the town each was named
   *  after, or — where no such town has a venue — the page now holding most of its
   *  venues. The 149 metros that did not leave a page under their own slug when
   *  the US went from metros to towns (2026-10) among them: `/philadelphia-new-jersey/`
   *  → Philadelphia, `/inland-empire/` → Chino. A metro whose slug is a page now
   *  (`/los-angeles/`, `/springfield/`) simply IS that town's page. Both sides are
   *  slugged by [[usSlug]]; a former page that now IS its heir's URL
   *  (`/birmingham-alabama/`) is no redirect at all. */
  private lazy val usSuccession: Map[String, String] = {
    val target = UsRoster.places.map(p => p.preferredSlug -> usSlug(p.preferredSlug, p.qualifiedSlug)).toMap
    val live   = usCities.map(_.slug).toSet
    UsRosterData.retired
      .map { case (preferred, qualified, page) => usSlug(preferred, qualified) -> target.getOrElse(page, page) }
      .collect { case (from, to) if from != to && !live(from) => from -> to }
      .toMap
  }

  /** The two US slugs Spain's pages yield to: the Ohio and Texas cities that held
   *  `/toledo/` and `/laredo/` before Spain had town pages, so the Spanish ones are
   *  `/toledo-castilla-la-mancha/` and `/laredo-cantabria/`. Every other collision
   *  is the US's to resolve. */
  private[models] val UsSlugsSpainYields: Set[String] = Set("toledo", "laredo")

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
   *  ONE successor each. A slug once could be split over several (Alaska over its
   *  nine metros, until 2026-10), which made every successor read the old slug's
   *  rows filtered to its own venues; every retirement now names the one page its
   *  URL lands on, and a row of a venue that moved is re-addressed to the venue's
   *  page by `WebReadModel` whatever slug it was projected under.
   *
   *  - `dudley`, `sandwell` → `birmingham` and `lanarkshire` → `glasgow`
   *    (2026-10-09): UK pages whose hubs were one urban area — see [[ukMergedPages]].
   *  - Poland's, Spain's, Germany's and the United States' retired pages, each
   *    pointing at the page holding the town it was named after — see
   *    [[germanSuccession]], [[spanishSuccession]], [[usSuccession]].
   */
  lazy val renamedSlugs: Map[String, String] = PolishPages.retired ++
    ukMergedPages.map { case (name, into) => Slugify.stable(name) -> into.slug } ++ spanishSuccession ++
    germanSuccession ++ usSuccession

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
    renamedSlugs.toSeq.groupMap(_._2)(_._1)

  /** The US picker's grouping: one entry per state or territory over the pages
   *  filed under it — major cities, towns and clusters side by side — the state
   *  being the anchor town's, so a cluster reaching across a state line is under
   *  the state of the town it is named after.
   *
   *  The pages inside a state are ALPHABETICAL: a visitor opening "California" is
   *  looking for a name they already know, and the only order that tells them
   *  where to look for it is the one the alphabet gives.
   *
   *  The states are in their SLUG's plain string order rather than re-sorted
   *  through [[CityListing.sortedLabels]] the way Germany's Bundesländer are: a
   *  Locale [[Collator]] weighs "Virgin Islands"' space against "Virginia"'s
   *  continuing letter differently than the slug does, which would silently swap
   *  the two. */
  private[models] lazy val usStates: Seq[CityGroup] = {
    val en = Locale.forLanguageTag("en-US")
    usCities.groupBy(_.stateName).toSeq
      .map { case (state, pages) => CityGroup(state, Slugify.stable(state), CityListing.sorted(pages, en)) }
      .sortBy(_.slug)
  }

  /** Spain's cities — the authoritative list for [[Country.Spain]]. One per town
   *  or cluster page (see [[SpanishPage]] / [[SpanishRoster]]), materialised
   *  data-driven from `SpanishRosterData` rather than hand-authored case objects.
   *
   *  The slug is assigned HERE rather than in [[SpanishRoster]] for the reason
   *  [[usCities]] spells out: `City.bySlug` is a single global namespace, and a
   *  Spanish town's name is not unique in it. */
  private[models] val spanishPages: Seq[SpanishPage] =
    SpanishRoster.places.map(p => new SpanishPage(spanishSlug(p.preferredSlug, p.qualifiedSlug), p))
  private[models] val spanishCities: Seq[City] = spanishPages

  /** Slugs another country already serves, which a Spanish page may not take —
   *  the US's only where it held them first ([[UsSlugsSpainYields]]). */
  private lazy val foreignToSpain: Set[String] =
    (polishCities ++ allUkCities ++ germanCities).map(_.slug).toSet ++ UsSlugsSpainYields

  /** The URL slug of a Spanish page — or of one that used to be (see
   *  [[spanishSuccession]]), which answered at the slug this gives too.
   *
   *  A page keeps its own folded name wherever that is free, and takes its name
   *  qualified with its autonomous community where it is not
   *  (`toledo-castilla-la-mancha`) — the same rule, and the same reason, as a US
   *  page qualifying with its state. Toledo is the case: the US roster already
   *  serves Toledo, Ohio at `/toledo/`, a live published URL with a sitemap
   *  entry, an indexed page and a `city` cookie behind it, and the newcomer is the
   *  one that can still choose.
   *
   *  Qualifying only the SLUG — the label stays "Toledo", which is what the
   *  Spanish picker shows and what is unambiguous inside Spain. */
  private def spanishSlug(preferred: String, qualified: String): String =
    if (foreignToSpain.contains(preferred)) qualified else preferred

  /** Spanish pages that no longer exist → the page now holding most of their
   *  venues. The 13 provinces that did not leave a town page under their own slug
   *  when Spain went from provinces to towns (2026-10) among them: `/asturias/` →
   *  Gijón, `/islas-baleares/` → Palma de Mallorca. A province whose slug is a
   *  town page now (`/madrid/`, `/barcelona/`) simply IS that town's page. Both
   *  sides are slugged by [[spanishSlug]], so a former page that had to be
   *  qualified redirects from the URL it really answered at. */
  private lazy val spanishSuccession: Map[String, String] =
    SpanishRosterData.retired.map { case (preferred, qualified, page) =>
      val target = SpanishRoster.places.find(_.preferredSlug == page)
        .fold(page)(p => spanishSlug(p.preferredSlug, p.qualifiedSlug))
      spanishSlug(preferred, qualified) -> target
    }.toMap

  /** Spain's picker: one group per province, its major cities, towns and
   *  clusters side by side, alphabetically — as Poland's voivodeships. */
  private[models] lazy val spanishProvinces: Seq[CityGroup] = {
    val es = Locale.forLanguageTag("es-ES")
    val built = spanishPages.groupBy(_.province).toSeq.map { case (name, cities) => CityGroup(name, Slugify.stable(name), CityListing.sorted(cities, es)) }
    CityListing.sortedLabels(built, es)(_.displayLabel)
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

  /** What starts every city's [[City.shareImage]]. */
  val ShareImagePrefix: String = "city-"

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
