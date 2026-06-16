package services.cinemas

import models.{Cinema, City}
import tools.HttpFetch

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Resolves each modelled [[Cinema]] to Filmweb's internal cinema id at RUNTIME,
 * so adding a cinema to the model/catalog auto-includes it in `FilmwebDiff` with
 * no hand-maintained id table.
 *
 * Filmweb publishes one showtimes listing per city at `/showtimes/<CityName>`;
 * each cinema appears as a link `/showtimes/<City>/<Name>-<id>`, where `<id>` is
 * the SAME id the seances API (`/api/v1/cinema/<id>/seances`) takes (verified
 * 2026-06). We fetch one listing per city, parse every `(Name, id)` pair, and
 * fuzzy-match each of our `Cinema.displayName`s against the Filmweb names. A
 * small [[overrides]] map wins first, for the handful of cinemas whose names are
 * too divergent for the fuzzy matcher (or that Filmweb lists but serves empty).
 *
 * Resolution per cinema: override (forced id, or explicit "no Filmweb data") →
 * else fuzzy match against the city listing → else UNMATCHED (`NO_FILMWEB_ID`).
 * Unmatched cinemas are reported, not errors: a new cinema with no override and
 * no fuzzy hit surfaces here automatically, prompting nothing manual.
 *
 * Pure parsing + matching are public so the spec can exercise them offline; only
 * [[resolveAll]] touches the network (one GET per city).
 */
class FilmwebCinemaIdResolver(http: HttpFetch) {
  import FilmwebCinemaIdResolver._

  /** Resolve every modelled cinema (optionally scoped to a set of city slugs).
   *  One HTTP GET per in-scope city; a city whose listing fails to fetch leaves
   *  its cinemas UNMATCHED rather than failing the whole resolution. */
  def resolveAll(cityFilter: Set[String] = Set.empty): Seq[Resolution] = {
    val cities = City.all.filter(c => cityFilter.isEmpty || cityFilter(c.slug))

    // Fetch + parse each in-scope city's Filmweb listing once, tolerantly.
    val listingByCity: Map[String, Seq[FilmwebCinema]] = cities.flatMap { city =>
      filmwebCityNames.getOrElse(city.slug, Nil).flatMap { cityName =>
        Try(http.get(listingUrl(cityName))).toOption.toSeq.flatMap(parseCinemaListing)
      } match {
        case Nil      => None
        case listings => Some(city.slug -> listings.distinctBy(_.id))
      }
    }.toMap

    cities.flatMap(_.cinemas).map { cinema =>
      val listing = cityOf(cinema).flatMap(c => listingByCity.get(c.slug)).getOrElse(Nil)
      resolveOne(cinema, listing)
    }
  }

  /** Override first, else fuzzy-match against this cinema's city listing. */
  def resolveOne(cinema: Cinema, listing: Seq[FilmwebCinema]): Resolution =
    overrides.get(cinema) match {
      case Some(Some(id)) => Resolution(cinema, Some(id), Override)
      case Some(None)     => Resolution(cinema, None, OverrideSuppressed)
      case None =>
        bestMatch(cinema.displayName, listing) match {
          case Some(m) => Resolution(cinema, Some(m.id), Fuzzy(m.name, m.score))
          case None    => Resolution(cinema, None, Unmatched)
        }
    }

  private def cityOf(cinema: Cinema): Option[City] =
    City.all.find(_.cinemas.contains(cinema))
}

object FilmwebCinemaIdResolver {

  /** Our city slug → the Filmweb city-listing name(s) under `/showtimes/<Name>`.
   *  Trójmiasto spans two Filmweb cities (Gdańsk + Gdynia). Extendable: a new
   *  modelled city just needs its Filmweb city name(s) here. */
  val filmwebCityNames: Map[String, Seq[String]] = Map(
    "poznan"     -> Seq("Poznań"),
    "wroclaw"    -> Seq("Wrocław"),
    "warszawa"   -> Seq("Warszawa"),
    "krakow"     -> Seq("Kraków"),
    "trojmiasto" -> Seq("Gdańsk", "Gdynia"),
  )

  /**
   * Forced resolutions for cinemas the fuzzy matcher gets wrong or can't reach.
   * `Some(id)` pins the verified Filmweb id; `None` explicitly suppresses (the
   * cinema is reported `NO_FILMWEB_ID`, not fuzzy-guessed). Carried over from the
   * old hand-maintained `filmwebCinemaIds` table for the divergent-name cases.
   *
   *   - Multikino Reduta  ← "Multikino Atrium Reduta"  (2119)
   *   - Multikino Wola Park ← "Multikino Wola"          (1380)
   *   - Kino Amondo        ← "Amondo Kino"              (2077)
   *   - Helios Riviera     ← Gdynia "Helios"            (1775)
   *   - Kino Muzeum (Gdańsk) ← "Kino Muzeum"            (2042)
   *   - Kino Apollo: SUPPRESSED — Filmweb lists "Kino Teatr Apollo" (3025) but
   *     its seances API returns empty across the whole window (verified
   *     2026-06), so the old 3025 produced only noise. No usable Filmweb data.
   *   - Kinoteka ← Filmweb "Kinoteka" (55). The fuzzy match is exact, so this
   *     pin is about reliability, not name divergence: kinoteka.pl is down at the
   *     TCP layer (verified globally 2026-06-16), so the venue now depends on the
   *     Filmweb fallback every tick. Pinning the verified id removes that
   *     dependence on a single boot-time `/showtimes/Warszawa` GET succeeding —
   *     a blip there would otherwise leave the venue with no fallback id and a
   *     red /uptime bar while its own site stays dead.
   */
  val overrides: Map[Cinema, Option[Int]] = Map(
    models.MultikinoReduta   -> Some(2119),
    models.MultikinoWolaPark -> Some(1380),
    models.KinoAmondo        -> Some(2077),
    models.HeliosRiviera     -> Some(1775),
    models.KinoMuzeumGdansk  -> Some(2042),
    models.Kinoteka          -> Some(55),
    models.KinoApollo        -> None,
  )

  /** How a cinema's id was resolved — for the FilmwebDiff RESOLUTION report. */
  sealed trait Source
  case object Override           extends Source              // forced id
  case object OverrideSuppressed extends Source              // override says "no Filmweb data"
  final case class Fuzzy(matchedName: String, score: Double) extends Source
  case object Unmatched          extends Source              // no override, no fuzzy hit

  final case class Resolution(cinema: Cinema, filmwebId: Option[Int], source: Source) {
    def resolved: Boolean = filmwebId.isDefined
  }

  /** One cinema as Filmweb lists it on a city showtimes page. */
  final case class FilmwebCinema(name: String, id: Int)

  /** One fuzzy match candidate + its similarity score (0..1). */
  final case class Match(name: String, id: Int, score: Double)

  def listingUrl(cityName: String): String =
    "https://www.filmweb.pl/showtimes/" + urlEncode(cityName)

  private val LinkPat =
    """href="/showtimes/[^"/]+/([^"]+)-(\d+)"""".r

  /** Parse a `/showtimes/<City>` listing's HTML into `(name, id)` pairs. The
   *  cinema name is taken from the URL slug (URL-decoded, `+`→space) — robust to
   *  markup churn, and equal to the on-page header. Pure: spec feeds fixtures. */
  def parseCinemaListing(html: String): Seq[FilmwebCinema] =
    LinkPat.findAllMatchIn(html).flatMap { m =>
      Try(m.group(2).toInt).toOption.map { id =>
        FilmwebCinema(urlDecode(m.group(1)).trim, id)
      }
    }.toSeq.distinctBy(_.id)

  /** Best fuzzy match for `displayName` among `candidates`, or None if nothing
   *  clears the acceptance threshold. Scored by token-overlap coefficient (see
   *  [[similarity]]) so the most-specific listing wins: "Mikro Bronowice" beats
   *  the bare "Mikro", and "Mikro" picks "Mikro" over "Mikro Bronowice". */
  def bestMatch(displayName: String, candidates: Seq[FilmwebCinema]): Option[Match] = {
    val ourTokens = tokens(displayName)
    if (ourTokens.isEmpty) None
    else candidates
      .map(c => Match(c.name, c.id, similarity(ourTokens, tokens(c.name))))
      .filter(_.score >= AcceptThreshold)
      .sortBy(m => (-m.score, m.name))
      .headOption
  }

  // Accept a fuzzy match only when the names share enough: ≥ this token-overlap
  // score. Tuned so "Muza" ↔ "Muza", "Helios" ↔ "Helios", "Multikino Kraków" ↔
  // "Multikino" pass while unrelated venues don't cross-match.
  private val AcceptThreshold = 0.5

  // Generic words that carry no discriminating signal between Polish cinema
  // names — dropped before scoring so "Kino Muza" ↔ "Muza" still matches.
  private val StopWords = Set("kino", "kina", "cinema", "teatr", "multipleks")

  private val Diacritics: Map[Char, Char] = Map(
    'ą' -> 'a', 'ć' -> 'c', 'ę' -> 'e', 'ł' -> 'l', 'ń' -> 'n',
    'ó' -> 'o', 'ś' -> 's', 'ż' -> 'z', 'ź' -> 'z'
  )

  private def stripDiacritics(s: String): String =
    s.map(c => Diacritics.getOrElse(c, c))

  private def tokens(name: String): Set[String] =
    stripDiacritics(name.toLowerCase)
      .split("[^a-z0-9]+").iterator
      .map(_.trim).filter(_.nonEmpty)
      .filterNot(StopWords)
      .toSet

  /** Token-overlap coefficient in [0,1]: |A ∩ B| / max(|A|,|B|). Dividing by the
   *  LARGER set (not the union) means a candidate that's a strict subset of our
   *  name doesn't get a free 1.0 — "Mikro" scores 0.5 against our "Mikro
   *  Bronowice", while the full "Mikro Bronowice" listing scores 1.0 and wins.
   *  An exact token match is 1.0; Filmweb's legitimately-shorter names (its
   *  "Helios" for our "Helios Posnania") still clear the 0.5 threshold. */
  private def similarity(a: Set[String], b: Set[String]): Double = {
    if (a.isEmpty || b.isEmpty) 0.0
    else a.intersect(b).size.toDouble / math.max(a.size, b.size).toDouble
  }

  private def urlEncode(s: String): String =
    java.net.URLEncoder.encode(s, StandardCharsets.UTF_8).replace("+", "%20")

  private def urlDecode(s: String): String =
    Try(URLDecoder.decode(s.replace("+", "%20"), StandardCharsets.UTF_8)).getOrElse(s)
}
