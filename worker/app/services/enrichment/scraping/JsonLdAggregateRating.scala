package services.enrichment.scraping

import org.jsoup.Jsoup
import play.api.libs.json.{JsNumber, JsString, JsValue, Json}

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

  /** First numeric `aggregateRating.ratingValue` found in any JSON-LD
   *  block, parsed as Int. The JSON-LD spec allows both numeric and string
   *  values, so we accept either. Returns None when the page has no
   *  JSON-LD, no `aggregateRating`, or only non-numeric values. */
  def parseInt(html: String): Option[Int] = {
    val doc = Jsoup.parse(html)
    doc.select("script[type=application/ld+json]").asScala.iterator
      .map(_.data())
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>
          (js \ "aggregateRating" \ "ratingValue").asOpt[JsValue].flatMap(parseAsInt)
        }
      }
      .toSeq.headOption
  }

  private def parseAsInt(v: JsValue): Option[Int] = v match {
    case JsString(s) => Try(s.toInt).toOption
    case JsNumber(n) => Try(n.toInt).toOption
    case _           => None
  }
}
