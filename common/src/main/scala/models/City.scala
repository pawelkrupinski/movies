package models

import java.time.ZoneId
import java.util.Locale

import tools.Slugify

/** The Polish name of a city in the grammatical forms the templates need:
 *  nominative ("Poznań"), genitive plural for "…skich kin" ("poznańskich"),
 *  and locative for "w …" ("Poznaniu"). Kept as data so no template hardcodes
 *  a city name. */
final case class CityLabels(nominative: String, genitivePlural: String, locative: String)

object CityLabels {
  /** The labels of a place in a language that does not decline its name (English,
   *  German, Spanish): every slot carries the name as it is. */
  def undeclined(name: String): CityLabels = CityLabels(name, name, name)
}

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
  /** Might EVERY venue of this city be dark by design at `now` ([[Opening.mayBeDarkAt]]: off
   *  its season, or in its window's final month)? Then a city with no films is an open-air town
   *  whose season has ended, not one a scraper lost. Never for a city with a year-round venue,
   *  nor for one with no venues at all. */
  final def mayBeDarkAt(now: java.time.Instant): Boolean =
    cinemas.nonEmpty && cinemas.forall(_.opening.mayBeDarkAt(now))
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

/** A UK page — a major city (a place of 100,000+) with only its own venues, a
 *  town with three or more, or a cluster of small towns a short drive apart named
 *  after the biggest: "Epsom area", "in the Epsom area". Poland's shape
 *  ([[PolishPage]]), built by the same rule (`data/uk/scripts/build_pages.py`),
 *  with one addition: no venue is more than 40 km from the town a page is named
 *  after. Until 2026-10 a UK page was a Flicks REGION filed by hand — mostly a
 *  county, so `/west-yorkshire/` listed Leeds, Bradford and Ilkley, and the
 *  Highlands and Islands reached 285 km to Lerwick.
 *
 *  The venues stay the hand-written `case object`s of `Cinema.scala`; which page
 *  lists each is generated into [[UkPages]], and instances are built ONCE (in
 *  `City.ukPages`), so identity equality holds just like the hand-authored
 *  `case object` cities. Every page except [[London]], which keeps its compass
 *  areas. */
final class UkPage private[models] (row: UkPages.Row)
  extends City(row.slug, UkPage.labels(row), row.lat, row.lon, TimeZones.UnitedKingdom) {
  val cinemas: Seq[Cinema] = row.cinemas
  /** The town the page is named after first, then the rest of its towns, then
   *  the places its venues' addresses name. */
  override protected val extraPlaces: Seq[String] = (row.towns ++ VenueTowns.of(cinemas)).distinct
  /** "in the Epsom area" — an area takes the article a town does not. */
  override def locativePhrase: String = if (row.multiTown) s"in the ${labels.nominative}" else super.locativePhrase
  /** The nation and county the picker files this page under: its anchor town's. */
  def nation: String = row.nation
  def county: String = row.county
}

private[models] object UkPage {
  /** English does not decline a place name, so all three slots carry the page's
   *  name — "<town> area" for a cluster, as a US cluster reads. */
  def labels(row: UkPages.Row): CityLabels = CityLabels.undeclined(row.nominative)
}

/** Greater London — one page, as New York's five boroughs are one: every place in
 *  it (Croydon, Romford, Kingston) is London (`data/uk/scripts/geocode_towns.py`).
 *  Hand-declared rather than a [[UkPage]] for the one thing no other UK page has,
 *  its compass areas. */
case object London extends City("london", CityLabels.undeclined("London"),
                                UkPages.london.lat, UkPages.london.lon, TimeZones.UnitedKingdom) {
  val cinemas: Seq[Cinema] = UkPages.london.cinemas
  override val areas: Seq[CinemaAreaGroup] = LondonAreas.groups
}

// ── The generated rosters: Germany, the United States, Spain (non-declining). ──────────

/** A page of a generated roster — Germany's, the United States' or Spain's ([[RosterPlace]]):
 *  a major city with only its own venues, a town with three or more, or a cluster of small towns
 *  named after the biggest. Poland's shape ([[PolishPage]]), built by the same rule; until 2026-10
 *  such a page was a REGION of every venue within a radius of a hub, and `/koeln/` listed Bonn.
 *
 *  Instances are built ONCE (in `City.germanPages`, `usCities`, `spanishPages`), so identity
 *  equality holds just like the hand-authored `case object` cities. None of the three languages
 *  declines a place name, so all three label slots carry the page's name — "<town> <suffix>" for
 *  a cluster ([[RosterStyle]]). The zone is the place's: the US spans six and Spain two. */
final class RosterPage private[models] (slug: String, place: RosterPlace, style: RosterStyle)
  extends City(slug, RosterPage.labels(place, style), place.lat, place.lon, place.zoneId) {
  val cinemas: Seq[Cinema] = place.cinemas
  /** The town the page is named after first, then the rest of its towns. */
  override protected val extraPlaces: Seq[String] = (place.name +: place.towns).distinct
  override def locativePhrase: String =
    style.clusterPreposition.filter(_ => place.multiTown).fold(super.locativePhrase)(p => s"$p ${labels.nominative}")
  /** What the picker groups this page under: its anchor town's Bundesland, state or province. */
  def region: String = place.region
}

private[models] object RosterPage {
  def labels(place: RosterPlace, style: RosterStyle): CityLabels =
    CityLabels.undeclined(if (place.multiTown) s"${place.name} ${style.clusterSuffix}" else place.name)
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
 *  @param absorbed for a city of this group, the names of former pages that now
 *                  redirect to it — the UK's retired regions ([[UkPages.formerNames]])
 *                  — kept findable by [[aliasesOf]]. */
final case class CityGroup(label: String, slug: String, cities: Seq[City] = Nil,
                          groups: Seq[CityGroup] = Nil, absorbed: Map[City, Seq[String]] = Map.empty) {

  /** The one place this group holds, pulled up a level — and `None` for a group
   *  that really arranges several.
   *
   *  A heading you open to find exactly one row underneath is a tap that buys
   *  the reader nothing, whatever the two are called. A UK county holding one
   *  page collapses onto it — Rutland shows as Oakham, Shetland as Lerwick — as
   *  the District of Columbia shows as Washington and Saarland as Saarbrücken.
   *
   *  The group's own name is NOT lost when it differs — the picker keeps it on
   *  the row as a search alias (`data-alias`), so "shetland" still finds
   *  Lerwick. Dropping the heading is meant to save a tap, not to make the
   *  county unfindable, and the search is where that name was doing its work.
   *
   *  Nil `groups` is part of the test: a node with one city AND a subgroup under
   *  it is still arranging two things.
   *
   *  [[slug]] is deliberately NOT consulted. An earlier version required the
   *  city's slug to equal the group's, which collapsed Cheshire and left
   *  Merseyside, Greater Manchester, Norfolk and Saarland as headings over a
   *  single row — the tap this exists to remove. */
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

  /** Every other name the picker should find one of this group's own cities by:
   *  the group's name where the group collapsed onto it ([[collapsedAlias]]),
   *  then the names of the former pages that redirect to it ([[absorbed]]) — so
   *  "Sandwell" still finds West Bromwich and "Highlands and Islands" Inverness. */
  def aliasesOf(city: City): Seq[String] =
    soleCity.filter(_ == city).flatMap(_ => collapsedAlias).toSeq ++ absorbed.getOrElse(city, Nil)

  /** [[aliasesOf]] the one place this group collapsed onto — empty for a group
   *  that did not collapse: its heading is already on screen. */
  def searchAliases: Seq[String] = soleCity.fold(Seq.empty[String])(aliasesOf)

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
  private[models] lazy val polishVoivodeships: Seq[CityGroup] =
    CityListing.groupedAlphabetically(polishCities, Locale.forLanguageTag("pl-PL")) {
      case page: PolishPage => page.voivodeship
      case major            => PolishPages.majorVoivodeship(major.slug)
    }

  /** The United Kingdom's cities — the authoritative list for
   *  [[Country.UnitedKingdom]]: [[London]], then one [[UkPage]] per town or
   *  cluster page, materialised from [[UkPages]]. Their slugs are final there:
   *  each country owns its own, so `/uk/york/` is York whatever the US has. */
  private[models] lazy val ukPages: Seq[UkPage] = UkPages.rows.map(new UkPage(_))
  private[models] lazy val ukCities: Seq[City] = London +: ukPages

  /** UK pages that no longer exist → the page holding the town each was named
   *  after, or — where no such town has a venue — the page now holding most of
   *  its venues. The 76 Flicks regions the UK was filed under until 2026-10 and
   *  the three folded into a neighbour before that among them: `/antrim/` → the
   *  Lisburn area (Antrim's cluster), `/west-yorkshire/` → Leeds. A region whose
   *  slug is a page now (`/manchester/`, `/dudley/`) simply IS that town's page. */
  private[models] lazy val ukSuccession: Map[String, String] = UkPages.retired

  /** The UK picker's grouping: NATION over COUNTY over the pages inside it — the
   *  nation and county of each page's anchor town, read off GeoNames by
   *  `data/uk/scripts/geocode_towns.py` (England's ceremonial counties, Wales'
   *  preserved ones, Scotland's and Northern Ireland's councils, grouped).
   *
   *  264 pages is past what anyone reads as one A-to-Z, and a nation alone still
   *  is (England holds 200); the county is the level a visitor already knows a
   *  town by. A county holding one page collapses onto it ([[CityGroup.soleCity]]):
   *  Shetland shows as Lerwick, its own name kept as a search alias. A retired
   *  region's name is a search alias of the page its slug redirects to
   *  ([[UkPages.formerNames]]): "Tyrone" finds Dungannon.
   *
   *  Nations are in [[UkPages.nations]]' fixed reading order (England, Scotland,
   *  Northern Ireland, Wales, Crown Dependencies) — the same order every client's
   *  picker shows, kept in one place rather than re-derived per platform — and
   *  everything below is alphabetical, which is the only order a reader can
   *  search by eye. Every page appears exactly once (`CountrySpec` holds that). */
  private[models] lazy val ukNations: Seq[CityGroup] = {
    val en = Locale.forLanguageTag("en-GB")
    val placed: Seq[(String, String, City)] =
      (UkPages.london.nation, UkPages.london.county, London) +: ukPages.map(p => (p.nation, p.county, p))
    UkPages.nations.flatMap { nation =>
      val counties = placed.filter(_._1 == nation).groupBy(_._2).toSeq.map { case (county, pages) =>
        val cities = pages.map(_._3)
        CityGroup(county, Slugify.stable(county), CityListing.sorted(cities, en),
                  absorbed = cities.flatMap(c => UkPages.formerNames.get(c.slug).map(c -> _)).toMap)
      }
      // By what each row SHOWS, not by the county it came from: a county that
      // collapsed onto its one place displays that place's name, and sorting on
      // the county would file Shetland's Lerwick under S.
      Option.when(counties.nonEmpty)(
        CityGroup(nation, Slugify.stable(nation), Nil, CityListing.sortedLabels(counties, en)(_.displayLabel)))
    }
  }

  /** Germany's cities — the authoritative list for [[Country.Germany]]. One per
   *  town or cluster page (see [[RosterPage]] / [[GermanRoster]]), materialised
   *  data-driven from `GermanRosterData` rather than hand-authored case objects.
   *  Each country owns its own slugs, so a page's is simply its own name (or, for
   *  two German towns of one name, the qualified one `build_pages.py` gave it). */
  private[models] val germanPages: Seq[RosterPage] = GermanRoster.places.map(p => new RosterPage(p.preferredSlug, p, GermanRoster.style))
  private[models] val germanCities: Seq[City] = germanPages

  /** A retired page's URLs → the page it redirects to: under its own name AND
   *  under its qualified name, which is the one it answered at while another
   *  country held its name (until 2026-10, when every country came to own its
   *  slugs) — both are published. Neither may be a live page's slug. */
  private def succession(retired: Seq[(String, String, String)], live: Set[String]): Map[String, String] =
    retired.flatMap { case (preferred, qualified, page) => Seq(preferred -> page, qualified -> page) }
      .filter { case (from, to) => from != to && !live(from) }
      .toMap

  /** German pages that no longer exist → the page holding the town each was
   *  named after, or — where that town has no venue left — the page now holding
   *  most of its venues. The 19 regions that did not leave a page under their own
   *  slug when Germany went from regions to towns (2026-10) among them: `/vechta/`
   *  → Cloppenburg (Vechta's cluster), `/itzehoe/` → Elmshorn. A region whose slug is a page now
   *  (`/koeln/`, `/freiburg/`) simply IS that town's page. */
  private[models] lazy val germanSuccession: Map[String, String] =
    succession(GermanRosterData.retired, germanPages.map(_.slug).toSet)

  /** The German picker's grouping: one entry per BUNDESLAND over the pages inside
   *  it — major cities, towns and clusters side by side — alphabetically both
   *  ways, as Poland's voivodeships and Spain's provinces. The Land is the
   *  anchor town's, read off its coordinates by `build_pages.py`. */
  private[models] lazy val germanStates: Seq[CityGroup] =
    CityListing.groupedAlphabetically(germanPages, Locale.forLanguageTag("de-DE"))(_.region)


  /** The United States' cities — the authoritative list for
   *  [[Country.UnitedStates]]. One per town or cluster page (see [[RosterPage]] /
   *  [[UsRoster]]), materialised data-driven from `UsRosterData`. The state is a
   *  [[CityGroup]], not a city: nobody wants a screen of every cinema in Texas.
   *  A page's slug is its own name, qualified with its state only where another
   *  US page has the name (the Springfields). */
  private[models] lazy val usCities: Seq[RosterPage] = UsRoster.places.map(p => new RosterPage(p.preferredSlug, p, UsRoster.style))

  /** US pages that no longer exist → the page holding the town each was named
   *  after, or — where no such town has a venue — the page now holding most of its
   *  venues. The 149 metros that did not leave a page under their own slug when
   *  the US went from metros to towns (2026-10) among them: `/philadelphia-new-jersey/`
   *  → Philadelphia, `/inland-empire/` → Chino. A metro whose slug is a page now
   *  (`/los-angeles/`, `/springfield/`) simply IS that town's page. Plus
   *  [[FormerlyQualified]]'s US pages. */
  private[models] lazy val usSuccession: Map[String, String] =
    succession(UsRosterData.retired, usCities.map(_.slug).toSet) ++ FormerlyQualified.us

  /** The pages that answered at a slug qualified with their state or community
   *  while another country held their name — published URLs, live until every
   *  country came to own its own slugs (2026-10) — and the bare slug each answers
   *  at now. Ten US pages yielded to the UK, Germany and Spain; Spain's Toledo and
   *  Laredo yielded to the US. (The UK's qualified slugs never shipped.) */
  private[models] object FormerlyQualified {
    val us: Map[String, String] = Map(
      "birmingham-alabama" -> "birmingham", "bristol-tennessee" -> "bristol", "glasgow-kentucky" -> "glasgow",
      "kent-washington" -> "kent", "london-kentucky" -> "london", "manchester-new-hampshire" -> "manchester",
      "potsdam-new-york" -> "potsdam", "somerset-kentucky" -> "somerset", "stuttgart-arkansas" -> "stuttgart",
      "suffolk-virginia" -> "suffolk",
    )
    val es: Map[String, String] = Map("toledo-castilla-la-mancha" -> "toledo", "laredo-cantabria" -> "laredo")

    def of(country: Country): Map[String, String] = country match {
      case Country.UnitedStates => us
      case Country.Spain        => es
      case _                    => Map.empty
    }
  }

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
    usCities.groupBy(_.region).toSeq
      .map { case (state, pages) => CityGroup(state, Slugify.stable(state), CityListing.sorted(pages, en)) }
      .sortBy(_.slug)
  }

  /** Spain's cities — the authoritative list for [[Country.Spain]]. One per town
   *  or cluster page (see [[RosterPage]] / [[SpanishRoster]]), materialised
   *  data-driven from `SpanishRosterData` rather than hand-authored case objects.
   *  A page's slug is its own name: `/es/toledo/` is Toledo, whatever the US has. */
  private[models] val spanishPages: Seq[RosterPage] = SpanishRoster.places.map(p => new RosterPage(p.preferredSlug, p, SpanishRoster.style))
  private[models] val spanishCities: Seq[City] = spanishPages

  /** Spanish pages that no longer exist → the page now holding most of their
   *  venues. The 13 provinces that did not leave a town page under their own slug
   *  when Spain went from provinces to towns (2026-10) among them: `/asturias/` →
   *  Gijón, `/islas-baleares/` → Palma de Mallorca. A province whose slug is a
   *  town page now (`/madrid/`, `/barcelona/`) simply IS that town's page. Plus
   *  [[FormerlyQualified]]'s Spanish pages. */
  private[models] lazy val spanishSuccession: Map[String, String] =
    succession(SpanishRosterData.retired, spanishPages.map(_.slug).toSet) ++ FormerlyQualified.es

  /** Spain's picker: one group per province, its major cities, towns and
   *  clusters side by side, alphabetically — as Poland's voivodeships. */
  private[models] lazy val spanishProvinces: Seq[CityGroup] =
    CityListing.groupedAlphabetically(spanishPages, Locale.forLanguageTag("es-ES"))(_.region)

  /** Every modelled city, across all countries — the global view used by the
   *  worker (which scrapes every country) and by country-agnostic reverse
   *  lookups. A single-country web deployment scopes to `country.cities`.
   *
   *  Built directly from the per-country lists that live HERE (`polishCities`,
   *  `ukCities`, …), NOT via `Country.all` — `Country` depends on
   *  `City` (its `cities` read `City.polishCities`), so a back-reference would
   *  make the two objects' static initialisers wait on each other and deadlock
   *  when loaded on parallel threads. Keep the dependency one-directional:
   *  `Country → City`. A new country adds its list to this concatenation. */
  val all: Seq[City] = polishCities ++ ukCities ++ germanCities ++ usCities ++ spanishCities

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
