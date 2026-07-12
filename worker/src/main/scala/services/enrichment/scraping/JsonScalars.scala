package services.enrichment.scraping

import play.api.libs.json.{JsNumber, JsString, JsValue}

import scala.util.Try

/**
 * Coerce a JSON-LD / data-island scalar into an Int. Both the schema.org
 * `aggregateRating.ratingValue` and RT's `media-scorecard-json`
 * `criticsScore.score` publish their percentage as *either* a JSON string
 * ("94") or a bare number (94), so every score extractor needs the same
 * string-or-number → Int coercion. CLAUDE.md threshold-2 extraction.
 */
object JsonScalars {

  def intValue(v: JsValue): Option[Int] = v match {
    case JsString(s) => Try(s.trim.toInt).toOption
    case JsNumber(n) => Try(n.toInt).toOption
    case _           => None
  }
}
