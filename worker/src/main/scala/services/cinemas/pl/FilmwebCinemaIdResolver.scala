package services.cinemas.pl

import models.{Cinema, City, Country}
import play.api.libs.json.{Json, Reads}
import tools.{BoundedParallel, HttpFetch}

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Resolves each modelled Polish [[Cinema]] to Filmweb's internal cinema id at
 * RUNTIME, so adding a cinema to the model/catalog auto-includes it in
 * `FilmwebDiff` and gives it a Filmweb fallback with no hand-maintained id table.
 *
 * Filmweb's API names every town it covers (`/api/v1/cities`, `{id, name}`) and
 * lists each town's cinemas (`/api/v1/city/<id>/cinemas`, `{id, name}`), where the
 * cinema `id` is the SAME id the seances API (`/api/v1/cinema/<id>/seances`)
 * takes. Each cinema is looked up in the listing of the town(s) it sits in
 * ([[City.townsOf]]: the venue table's town, else the city's own), and its
 * `displayName` fuzzy-matched against the names there. A small [[overrides]] map
 * wins first, for the handful of cinemas whose names are too divergent for the
 * fuzzy matcher (or that Filmweb lists but serves empty).
 *
 * Resolution per cinema: override (forced id, or explicit "no Filmweb data") →
 * else fuzzy match against its towns' listings → else UNMATCHED (`NO_FILMWEB_ID`).
 * Unmatched cinemas are reported, not errors: a new cinema with no override and
 * no fuzzy hit surfaces here automatically, prompting nothing manual.
 *
 * Pure parsing + matching are public so the spec can exercise them offline; only
 * [[resolveAll]] touches the network (one GET for the towns, one per town).
 */
class FilmwebCinemaIdResolver(http: HttpFetch) {
  import FilmwebCinemaIdResolver._

  /** Resolve every modelled Polish cinema (optionally scoped to a set of city
   *  slugs). A town list or town listing that fails to fetch leaves its cinemas
   *  UNMATCHED rather than failing the whole resolution. */
  def resolveAll(cityFilter: Set[String] = Set.empty): Seq[Resolution] = {
    val cinemas = City.all
      .filter(c => c.country == Country.Poland && (cityFilter.isEmpty || cityFilter(c.slug)))
      .flatMap(city => city.cinemas.map(cinema => cinema -> city.townsOf(cinema)))

    val townIds: Map[String, Seq[Int]] =
      Try(parseTowns(http.get(TownsUrl))).getOrElse(Nil).groupMap(_.name)(_.id)
    def idsOf(towns: Seq[String]): Seq[Int] = towns.flatMap(townIds.getOrElse(_, Nil)).distinct

    val listings = new ConcurrentHashMap[Int, Seq[FilmwebCinema]]()
    BoundedParallel.foreach("filmweb-town-cinemas", cinemas.flatMap((_, towns) => idsOf(towns)).distinct, MaxConcurrent) { id =>
      Try(parseCinemaListing(http.get(townCinemasUrl(id)))).foreach(listings.put(id, _))
    }
    val listingOf = listings.asScala

    cinemas.map { (cinema, towns) =>
      resolveOne(cinema, idsOf(towns).flatMap(listingOf.getOrElse(_, Nil)).distinctBy(_.id))
    }
  }

  /** Override first, else fuzzy-match against this cinema's towns' listings. */
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

}

object FilmwebCinemaIdResolver {

  val TownsUrl: String = "https://www.filmweb.pl/api/v1/cities"

  def townCinemasUrl(townId: Int): String = s"https://www.filmweb.pl/api/v1/city/$townId/cinemas"

  // One request per town the roster names (~300); Filmweb soft-blocks past ~5
  // concurrent requests (see BoundedParallel).
  private val MaxConcurrent = 5

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
   *   - Multikino Rumia    ← Filmweb lists a bare "Multikino" (no city/district
   *     suffix) for Gdynia/Trójmiasto that fuzzy-matches "Multikino Rumia" at
   *     exactly the 0.5 threshold, discarding the "rumia" token and resolving to
   *     the wrong Multikino. Pin the verified id (1464, see `Cinema.scala`).
   *   - Kino Apollo: SUPPRESSED — Filmweb lists "Kino Teatr Apollo" (3025) but
   *     its seances API returns empty across the whole window (verified
   *     2026-06), so the old 3025 produced only noise. No usable Filmweb data.
   *   - Kinoteka ← Filmweb "Kinoteka" (55). The fuzzy match is exact, so this
   *     pin is about reliability, not name divergence: kinoteka.pl is down at the
   *     TCP layer (verified globally 2026-06-16), so the venue now depends on the
   *     Filmweb fallback every tick. Pinning the verified id removes that
   *     dependence on the boot-time town-listing GETs succeeding —
   *     a blip there would otherwise leave the venue with no fallback id and a
   *     red /uptime bar while its own site stays dead.
   */
  val overrides: Map[Cinema, Option[Int]] = Map(
    models.MultikinoReduta   -> Some(2119),
    models.MultikinoWolaPark -> Some(1380),
    models.KinoAmondo        -> Some(2077),
    models.HeliosRiviera     -> Some(1775),
    models.KinoMuzeumGdansk  -> Some(2042),
    models.MultikinoRumia    -> Some(1464),
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

  /** One cinema as Filmweb lists it in a town's cinema listing. */
  final case class FilmwebCinema(name: String, id: Int)

  /** One town as Filmweb names it. Names are not unique (two Skarżysko-Kamienna
   *  entries), so a town name may map to several ids. */
  final case class FilmwebTown(name: String, id: Int)

  private given Reads[FilmwebCinema] = Json.reads[FilmwebCinema]
  private given Reads[FilmwebTown]   = Json.reads[FilmwebTown]

  /** Parse `/api/v1/city/<id>/cinemas` into `(name, id)` pairs. Pure: spec feeds fixtures. */
  def parseCinemaListing(json: String): Seq[FilmwebCinema] = Json.parse(json).as[Seq[FilmwebCinema]]

  /** Parse `/api/v1/cities` into `(name, id)` pairs. Pure: spec feeds fixtures. */
  def parseTowns(json: String): Seq[FilmwebTown] = Json.parse(json).as[Seq[FilmwebTown]]

  /** One fuzzy match candidate + its similarity score (0..1). */
  final case class Match(name: String, id: Int, score: Double)

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
}
