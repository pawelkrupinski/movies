package services.enrichment

import play.api.libs.json._
import tools.HttpFetch

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Cinemeta (Stremio's public catalogue addon) as an IMDb-id resolver of last
 * resort in [[ImdbIdResolver]] — after IMDb suggestion, director, Wikidata,
 * Trakt, Letterboxd and OMDb have all abstained.
 *
 * Cinemeta is IMDb-KEYED: every catalogue entry's `id` IS the `tt…` imdb id, and
 * the search catalogue indexes a broad long tail (regional/foreign releases) that
 * IMDb's own suggestion endpoint and OMDb's English DB can miss. One GET:
 *
 *   GET https://v3-cinemeta.strem.io/catalog/movie/top/search=<title>.json
 *     → { "metas": [ { "id":"tt…", "name":"…", "releaseInfo":"2026", … }, … ] }
 *
 * Corroboration (same "never guess" bar as OMDb/Trakt): a candidate binds only
 * when its name matches the query EXACTLY (deburred, case-folded), OR its name
 * contains/starts-with the query AND its `releaseInfo` year is within one of the
 * queried year. A bare fuzzy hit is refused so a wrong film can't get bound.
 * Never throws — any network/parse failure yields None.
 */
class CinemetaClient(http: HttpFetch) {
  import CinemetaClient._

  /** IMDb id for the first title spelling Cinemeta corroborates, or None. Pass the
   *  original/English title first — Cinemeta indexes primarily on that. */
  def findImdbId(titles: Seq[String], year: Option[Int]): Option[String] =
    titles.map(_.trim).filter(_.nonEmpty).distinct.iterator
      .flatMap(t => resolveTitle(t, year).iterator)
      .nextOption()

  private def resolveTitle(title: String, year: Option[Int]): Option[String] =
    Try {
      val body  = http.get(searchUrl(title))
      val metas = (Json.parse(body) \ "metas").asOpt[JsArray].map(_.value.toSeq).getOrElse(Seq.empty)
      metas.iterator.flatMap(m => candidate(m)).find(c => corroborated(c, title, year)).map(_.imdbId)
    }.toOption.flatten

  private def candidate(meta: JsValue): Option[Candidate] =
    for {
      id   <- (meta \ "id").asOpt[String].map(_.trim).filter(_.startsWith("tt"))
      name <- (meta \ "name").asOpt[String].map(_.trim).filter(_.nonEmpty)
    } yield Candidate(id, name, (meta \ "releaseInfo").asOpt[String].flatMap(firstYear))

  private def corroborated(c: Candidate, queryTitle: String, year: Option[Int]): Boolean = {
    val q = norm(queryTitle); val n = norm(c.name)
    val exact         = q.nonEmpty && q == n
    val titleContains = q.nonEmpty && n.nonEmpty && (q.startsWith(n) || n.startsWith(q))
    val yearMatch     = (for { y <- year; cy <- c.year } yield math.abs(y - cy) <= 1).getOrElse(false)
    // A year off by >1 vetoes even an EXACT title — two different films share a
    // name often enough ("Alpha" 2015 vs 2026) that a title-only resolver must not
    // bind across a year gap. When either year is unknown there's nothing to
    // contradict, so an exact title still binds.
    val yearContradicts = (for { y <- year; cy <- c.year } yield math.abs(y - cy) > 1).getOrElse(false)
    (exact || (titleContains && yearMatch)) && !yearContradicts
  }
}

object CinemetaClient {
  private val Base = "https://v3-cinemeta.strem.io/catalog/movie/top"

  private final case class Candidate(imdbId: String, name: String, year: Option[Int])

  def searchUrl(title: String): String =
    s"$Base/search=${URLEncoder.encode(title, StandardCharsets.UTF_8)}.json"

  /** `releaseInfo` is a year ("2026") or a range ("2011-2019"); take the first year. */
  private def firstYear(releaseInfo: String): Option[Int] =
    raw"(\d{4})".r.findFirstIn(releaseInfo).map(_.toInt)

  /** Deburred, case-folded, alnum-only — the same shape the other resolvers'
   *  corroboration uses so "Vaiana" vs "Vaiana " vs "vaiana" all match. */
  private def norm(s: String): String =
    java.text.Normalizer.normalize(s, java.text.Normalizer.Form.NFD)
      .replaceAll("\\p{M}+", "").toLowerCase.replaceAll("[^a-z0-9]+", "")
}
