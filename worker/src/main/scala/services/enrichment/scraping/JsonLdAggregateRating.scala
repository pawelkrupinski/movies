package services.enrichment.scraping

import org.jsoup.Jsoup
import play.api.libs.json.{JsValue, Json}

import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Helper for parsing the `aggregateRating.ratingValue` field out of a page's
 * `<script type="application/ld+json">` block — the schema.org structured-
 * data signal that both Metacritic and Rotten Tomatoes use to publish their
 * critic-aggregate scores. CLAUDE.md threshold-2 extraction; MC and RT had
 * the same select-parse-pluck idiom, only the post-filter differed.
 *
 * The block can appear several times on a page (canonical metadata,
 * breadcrumb, etc.); we iterate and return the first `ratingValue` that
 * parses as an Int. Callers apply their own range filter (RT clamps to
 * 0–100, MC does not) on top of the result.
 */
object JsonLdAggregateRating {

  private val YearRegex = "(19\\d{2}|20\\d{2})".r

  /** First numeric `aggregateRating.ratingValue` found in any JSON-LD
   *  block, parsed as Int. The JSON-LD spec allows both numeric and string
   *  values, so we accept either. Returns None when the page has no
   *  JSON-LD, no `aggregateRating`, or only non-numeric values. */
  def parseInt(html: String): Option[Int] = {
    val document = Jsoup.parse(html)
    document.select("script[type=application/ld+json]").asScala.iterator
      .map(_.data())
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>
          (js \ "aggregateRating" \ "ratingValue").asOpt[JsValue].flatMap(JsonScalars.intValue)
        }
      }
      .toSeq.headOption
  }

  /** The four-digit year from the first `datePublished` found in any JSON-LD
   *  block (schema.org publishes it as an ISO date like "1994-07-22"). Used to
   *  validate that a probed movie page is actually the film we're resolving —
   *  a title-slug can collide with an unrelated film of the same name (e.g.
   *  "The North" 2026 de-articles to the slug of Rob Reiner's "North" 1994).
   *  Returns None when no JSON-LD block carries a parseable `datePublished`. */
  /** The `director` names in a page's JSON-LD, if it names any.
   *
   *  Metacritic publishes them as `"director":[{"@type":"Person","name":"…"}]`.
   *  A rating site's page is identified by its TITLE and YEAR alone otherwise,
   *  and that is not always enough: Metacritic carries TWO 2025 films called
   *  "Dreams" — Michel Franco's and Dag Johan Haugerud's — so title+year matched
   *  both and the first won, giving Franco's film the Norwegian one's score. The
   *  director is what tells them apart, exactly as it does in the TMDB walk. */
  def directorNames(html: String): Set[String] = {
    val document = Jsoup.parse(html)
    document.select("script[type=application/ld+json]").asScala.iterator
      .map(_.data())
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>
          val node = js \ "director"
          node.asOpt[Seq[JsValue]].getOrElse(node.asOpt[JsValue].toSeq)
            .flatMap(d => (d \ "name").asOpt[String].orElse(d.asOpt[String]))
        }
      }
      .map(_.trim)
      .filter(_.nonEmpty)
      .toSet
  }

  def datePublishedYear(html: String): Option[Int] = {
    val document = Jsoup.parse(html)
    document.select("script[type=application/ld+json]").asScala.iterator
      .map(_.data())
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap(js => (js \ "datePublished").asOpt[String])
      }
      .flatMap(YearRegex.findFirstIn)
      .map(_.toInt)
      .nextOption()
  }
}
