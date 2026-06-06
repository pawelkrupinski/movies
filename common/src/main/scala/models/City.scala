package models

import java.time.ZoneId

/** The Polish name of a city in the grammatical forms the templates need:
 *  nominative ("Poznań"), genitive plural for "…skich kin" ("poznańskich"),
 *  and locative for "w …" ("Poznaniu"). Kept as data so no template hardcodes
 *  a city name. */
final case class CityLabels(nominative: String, genitivePlural: String, locative: String)

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

object City {
  val all: Seq[City] = Seq(
    Poznan, Wroclaw, Warszawa, Krakow, Lodz, Katowice, Szczecin, Bialystok, Trojmiasto, Bydgoszcz, Lublin,
    Czestochowa, Radom, Sosnowiec, Torun, Kielce, Rzeszow, Gliwice, Zabrze,
  )
  def bySlug(slug: String): Option[City] = all.find(_.slug == slug)

  /** Compact JSON array of every city for the client (web `ALL_CITIES`,
   *  consumed by the geolocation/nearest-city picker + the filter switch).
   *  Hand-built (no play-json dependency in models); city names carry no
   *  characters needing JSON escaping. */
  def allJson: String =
    all.map(c => s"""{"slug":"${c.slug}","name":"${c.labels.nominative}","lat":${c.lat},"lon":${c.lon}}""")
       .mkString("[", ",", "]")
}
