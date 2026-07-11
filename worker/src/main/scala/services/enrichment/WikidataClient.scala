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

  /** IMDb id for a film found by TITLE (not a Filmweb id) — the direct-title rung
   *  in [[ImdbIdResolver]] for a TMDB-less film with no Filmweb entity page. Two
   *  steps: search Wikidata items that are instance-of film (P31=Q11424) AND match
   *  the title text, then bind the first whose English label corroborates (exact,
   *  or contains + a P577 publication year within one of the queried year). A year
   *  gap of >1 vetoes even an exact label — two films share a title often enough
   *  that a title-only lookup must not cross a year gap. Never throws. */
  def findImdbIdByTitle(title: String, year: Option[Int]): Option[String] =
    Try {
      val qids = searchFilmsByTitle(title)
      if (qids.isEmpty) None
      else {
        val url  = s"$ActionBase?action=wbgetentities&ids=${qids.mkString("|")}&props=claims|labels&languages=en&format=json"
        val body = http.get(url, UserAgentHeader)
        val entities = (Json.parse(body) \ "entities").asOpt[JsObject].map(_.value).getOrElse(Map.empty)
        qids.iterator.flatMap { qid =>
          entities.get(qid).flatMap { e =>
            val label   = (e \ "labels" \ "en" \ "value").asOpt[String]
            val imdbId  = firstClaim(e, PImdb).filter(_.startsWith("tt"))
            val pubYear = firstPublicationYear(e)
            imdbId.filter(_ => titleCorroborates(title, label, year, pubYear))
          }
        }.nextOption()
      }
    }.toOption.flatten

  private def searchFilmsByTitle(title: String): Seq[String] = {
    val query   = URLEncoder.encode(s"$title haswbstatement:P31=$QFilm", StandardCharsets.UTF_8)
    val url     = s"$ActionBase?action=query&list=search&srsearch=$query&srnamespace=0&srlimit=5&format=json"
    val body    = http.get(url, UserAgentHeader)
    (Json.parse(body) \ "query" \ "search").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(entry => (entry \ "title").asOpt[String]).filter(_.startsWith("Q"))
  }

  private def firstClaim(entity: JsValue, property: String): Option[String] =
    (entity \ "claims" \ property).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value").asOpt[String]).headOption

  /** Year from the first P577 (publication date) claim, whose value is a time
   *  object (`{"time":"+2026-01-01T00:00:00Z",…}`), not a plain string. */
  private def firstPublicationYear(entity: JsValue): Option[Int] =
    (entity \ "claims" \ PPublicationDate).asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value" \ "time").asOpt[String])
      .flatMap(t => raw"(\d{4})".r.findFirstIn(t)).map(_.toInt).headOption

  /** Bind a title-search hit only when its label corroborates: an exact
   *  (deburred) label, or a contains-match with a P577 year within one of the
   *  queried year. A year gap of >1 vetoes even an exact label. */
  private def titleCorroborates(queryTitle: String, label: Option[String], queryYear: Option[Int], pubYear: Option[Int]): Boolean =
    label.exists { l =>
      val q = norm(queryTitle); val n = norm(l)
      val exact           = q.nonEmpty && q == n
      val titleContains   = q.nonEmpty && n.nonEmpty && (q.startsWith(n) || n.startsWith(q))
      val yearMatch       = (for { y <- queryYear; py <- pubYear } yield math.abs(y - py) <= 1).getOrElse(false)
      val yearContradicts = (for { y <- queryYear; py <- pubYear } yield math.abs(y - py) > 1).getOrElse(false)
      (exact || (titleContains && yearMatch)) && !yearContradicts
    }

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
  private val PImdb            = "P345"   // IMDb id            → "tt0052080"
  private val PTmdb            = "P4947"  // TMDB movie id      → "603"
  private val PRottenTomatoes  = "P1258"  // RT id (with path)  → "m/the_matrix"
  private val PMetacritic      = "P1712"  // Metacritic id      → "movie/the-matrix"
  private val PLetterboxd      = "P6127"  // Letterboxd id      → "the-matrix"
  private val PPublicationDate = "P577"   // publication date   → {"time":"+2026-…"}
  private val QFilm            = "Q11424" // instance-of value: film (title-search filter)

  /** Deburred, case-folded, alnum-only — matches the shape the other resolvers'
   *  corroboration uses so label/title comparison is diacritic/case-insensitive. */
  private def norm(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}+", "").toLowerCase.replaceAll("[^a-z0-9]+", "")

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
