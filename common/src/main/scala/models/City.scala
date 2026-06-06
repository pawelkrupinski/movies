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

object City {
  val all: Seq[City] = Seq(Poznan, Wroclaw, Warszawa, Krakow, Bialystok, Trojmiasto)
  def bySlug(slug: String): Option[City] = all.find(_.slug == slug)

  /** Compact JSON array of every city for the client (web `ALL_CITIES`,
   *  consumed by the geolocation/nearest-city picker + the filter switch).
   *  Hand-built (no play-json dependency in models); city names carry no
   *  characters needing JSON escaping. */
  def allJson: String =
    all.map(c => s"""{"slug":"${c.slug}","name":"${c.labels.nominative}","lat":${c.lat},"lon":${c.lon}}""")
       .mkString("[", ",", "]")
}
