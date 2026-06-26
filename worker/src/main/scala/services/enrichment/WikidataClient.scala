package services.enrichment

import play.api.libs.json._
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Resolves an IMDb id from a Filmweb entity id via Wikidata's cross-reference
 * (P5032 → P345). Last-resort fallback in [[ImdbIdResolver]] when both IMDb's
 * own suggestion endpoint and the director-based disambiguation come up empty —
 * typically classic/repertoire films whose titles differ between the cinema
 * listing, IMDb, and TMDB.
 *
 * Two-step Action API flow (avoids the SPARQL endpoint, which applies aggressive
 * rate limits under load):
 *   1. `action=query&list=search&srsearch=haswbstatement:P5032=<filmwebId>`
 *      → up to 3 Wikidata Q-IDs whose P5032 matches the filmweb entity id.
 *   2. `action=wbgetentities&ids=Q…&props=claims`
 *      → fetches P345 (IMDb ID) from those Q-IDs.
 *
 * Wikimedia's User-Agent policy requires a meaningful UA; `HttpFetch.get(url,
 * headers)` passes it through — test fakes safely ignore the extra headers.
 */
class WikidataClient(http: HttpFetch) {
  import WikidataClient._

  /** IMDb tt-id for the given Filmweb entity id, or None when Wikidata has no
   *  cross-reference or a network call fails. Never throws. */
  def findImdbIdByFilmwebId(filmwebId: String): Option[String] =
    Try {
      val qids = searchByFilmwebId(filmwebId)
      if (qids.isEmpty) None else fetchImdbId(qids)
    }.toOption.flatten

  private def searchByFilmwebId(filmwebId: String): Seq[String] = {
    val encoded = URLEncoder.encode(s"haswbstatement:P5032=$filmwebId", StandardCharsets.UTF_8)
    val url     = s"$ActionBase?action=query&list=search&srsearch=$encoded&srnamespace=0&srlimit=3&format=json"
    val body    = http.get(url, UserAgentHeader)
    (Json.parse(body) \ "query" \ "search").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      .flatMap(entry => (entry \ "title").asOpt[String])
      .filter(_.startsWith("Q"))
  }

  private def fetchImdbId(qids: Seq[String]): Option[String] = {
    val url      = s"$ActionBase?action=wbgetentities&ids=${qids.mkString("|")}&props=claims&format=json"
    val body     = http.get(url, UserAgentHeader)
    val entities = (Json.parse(body) \ "entities").asOpt[JsObject].map(_.value).getOrElse(Map.empty)
    // Return the first Q-ID (by search rank) that carries a P345 claim
    qids.iterator.flatMap { qid =>
      entities.get(qid).iterator.flatMap { entity =>
        (entity \ "claims" \ "P345").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
          .flatMap(c => (c \ "mainsnak" \ "datavalue" \ "value").asOpt[String])
          .filter(_.startsWith("tt"))
      }
    }.nextOption()
  }
}

object WikidataClient {
  private val ActionBase = "https://www.wikidata.org/w/api.php"

  val UserAgentHeader: Map[String, String] =
    Map("User-Agent" -> "kinowo/1.0 (pawel.krupinski@gmail.com)")

  /** Extract the numeric Filmweb entity id from a canonical Filmweb film/serial
   *  URL (`…/film/Title-Year-<id>` or `…/serial/…`). Returns None for
   *  search-redirect URLs (`filmweb.pl/search?query=…`), which have no entity
   *  id and aren't actionable. */
  def filmwebEntityId(url: String): Option[String] =
    raw"-(\d+)/?$$".r.findFirstMatchIn(url).map(_.group(1))
}
