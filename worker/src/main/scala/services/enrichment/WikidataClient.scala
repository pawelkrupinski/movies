package services.enrichment

import play.api.libs.json._
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Harvests film-database cross-reference ids from a Filmweb entity id via
 * Wikidata (P5032 → the item's external-id claims). Last-resort fallback in
 * [[ImdbIdResolver]] when both IMDb's own suggestion endpoint and the
 * director-based disambiguation come up empty — typically classic/repertoire
 * films whose titles differ between the cinema listing, IMDb, and TMDB.
 *
 * A single `wbgetentities` claims call carries EVERY id the item records, so
 * one round-trip backfills IMDb (P345), TMDB (P4947), Rotten Tomatoes (P1258),
 * Metacritic (P1712) and Letterboxd (P6127) at once. The RT/Metacritic slugs
 * matter because those two rating clients otherwise DISCOVER their page by
 * slug-probing/scraping — a Wikidata hit gives the slug deterministically.
 *
 * Two-step Action API flow (avoids the SPARQL endpoint, which applies aggressive
 * rate limits under load):
 *   1. `action=query&list=search&srsearch=haswbstatement:P5032=<filmwebId>`
 *      → up to 3 Wikidata Q-IDs whose P5032 matches the filmweb entity id.
 *   2. `action=wbgetentities&ids=Q…&props=claims`
 *      → fetches the external-id claims from those Q-IDs.
 *
 * Wikimedia's User-Agent policy requires a meaningful UA; `HttpFetch.get(url,
 * headers)` passes it through — test fakes safely ignore the extra headers.
 */
class WikidataClient(http: HttpFetch) {
  import WikidataClient._

  /** IMDb tt-id for the given Filmweb entity id, or None when Wikidata has no
   *  cross-reference or a network call fails. Never throws. Thin accessor over
   *  [[findIdsByFilmwebId]] for callers that only want the IMDb id. */
  def findImdbIdByFilmwebId(filmwebId: String): Option[String] =
    findIdsByFilmwebId(filmwebId).flatMap(_.imdbId)

  /** Every film-database id Wikidata records for the given Filmweb entity id, or
   *  None when no item matches or the network call fails. Never throws. */
  def findIdsByFilmwebId(filmwebId: String): Option[WikidataIds] =
    Try {
      val qids = searchByFilmwebId(filmwebId)
      if (qids.isEmpty) None else harvest(qids)
    }.toOption.flatten

  private def searchByFilmwebId(filmwebId: String): Seq[String] = {
    val encoded = URLEncoder.encode(s"haswbstatement:P5032=$filmwebId", StandardCharsets.UTF_8)
    val url     = s"$ActionBase?action=query&list=search&srsearch=$encoded&srnamespace=0&srlimit=3&format=json"
    val body    = http.get(url, UserAgentHeader)
    (Json.parse(body) \ "query" \ "search").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(entry => (entry \ "title").asOpt[String])
      .filter(_.startsWith("Q"))
  }

  private def harvest(qids: Seq[String]): Option[WikidataIds] = {
    val url      = s"$ActionBase?action=wbgetentities&ids=${qids.mkString("|")}&props=claims&format=json"
    val body     = http.get(url, UserAgentHeader)
    val entities = (Json.parse(body) \ "entities").asOpt[JsObject].map(_.value).getOrElse(Map.empty)
    // Extract each id independently, each from the first Q-ID (by search rank)
    // that carries that property. Keeping the id-types independent preserves the
    // original imdbId behaviour ("first Q-ID with a P345 claim") while letting a
    // sibling Q-ID that happens to hold, say, the RT slug still contribute it.
    def claim(property: String): Option[String] =
      qids.iterator.flatMap { qid =>
        entities.get(qid).iterator.flatMap { entity =>
          (entity \ "claims" \ property).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
            .flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value").asOpt[String])
        }
      }.nextOption()

    val ids = WikidataIds(
      imdbId           = claim(PImdb).filter(_.startsWith("tt")),
      tmdbId           = claim(PTmdb).filter(_.forall(_.isDigit)).map(_.toInt),
      rottenTomatoesId = claim(PRottenTomatoes),
      metacriticId     = claim(PMetacritic),
      letterboxdId     = claim(PLetterboxd)
    )
    Some(ids).filter(_.nonEmpty)
  }
}

object WikidataClient {
  private val ActionBase = "https://www.wikidata.org/w/api.php"

  // Wikidata property ids for the film-database cross-references we harvest.
  private val PImdb           = "P345"   // IMDb id            → "tt0052080"
  private val PTmdb           = "P4947"  // TMDB movie id      → "603"
  private val PRottenTomatoes = "P1258"  // RT id (with path)  → "m/the_matrix"
  private val PMetacritic     = "P1712"  // Metacritic id      → "movie/the-matrix"
  private val PLetterboxd     = "P6127"  // Letterboxd id      → "the-matrix"

  val UserAgentHeader: Map[String, String] =
    Map("User-Agent" -> "kinowo/1.0 (pawel.krupinski@gmail.com)")

  /** Extract the numeric Filmweb entity id from a canonical Filmweb film/serial
   *  URL (`…/film/Title-Year-<id>` or `…/serial/…`). Returns None for
   *  search-redirect URLs (`filmweb.pl/search?query=…`), which have no entity
   *  id and aren't actionable. */
  def filmwebEntityId(url: String): Option[String] =
    raw"-(\d+)/?$$".r.findFirstMatchIn(url).map(_.group(1))

  /** RT's canonical Tomatometer page URL for a P1258 id (which already carries
   *  the `m/<slug>` path segment). Matches `RottenTomatoesClient`'s host. */
  def rottenTomatoesUrl(p1258: String): String = s"https://www.rottentomatoes.com/$p1258"

  /** Metacritic's canonical movie page URL for a P1712 id (which already carries
   *  the `movie/<slug>` path segment). Matches `MetacriticClient`'s host. */
  def metacriticUrl(p1712: String): String = s"https://www.metacritic.com/$p1712"
}

/** The film-database cross-reference ids Wikidata records for one film. Each is
 *  optional — an item rarely carries all five. `letterboxdId` has no row field
 *  yet; it's harvested here so a Letterboxd resolver can consume it later. */
case class WikidataIds(
  imdbId:           Option[String],   // P345  — "tt0052080"
  tmdbId:           Option[Int],      // P4947 — 603
  rottenTomatoesId: Option[String],   // P1258 — "m/the_matrix"
  metacriticId:     Option[String],   // P1712 — "movie/the-matrix"
  letterboxdId:     Option[String]    // P6127 — "the-matrix"
) {
  def nonEmpty: Boolean =
    imdbId.isDefined || tmdbId.isDefined || rottenTomatoesId.isDefined ||
      metacriticId.isDefined || letterboxdId.isDefined
}
