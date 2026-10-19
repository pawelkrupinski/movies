package services.enrichment.scraping

import play.api.libs.json.{JsNumber, JsReadable, JsString, JsValue}

import scala.util.Try

/**
 * Read a JSON scalar the way every scraper and rating reader wants it.
 *
 * [[intValue]]: both the schema.org `aggregateRating.ratingValue` and RT's
 * `media-scorecard-json` `criticsScore.score` publish their percentage as
 * *either* a JSON string ("94") or a bare number (94), so every score
 * extractor needs the same string-or-number → Int coercion.
 *
 * [[text]]: a feed's string field, trimmed, and absent when it is missing,
 * not a string, or blank — what every client means by "the field's value".
 */
object JsonScalars {

  /** The string at `js`, trimmed; `None` when it is missing, not a string, or blank. */
  def text(js: JsReadable): Option[String] = js.asOpt[String].map(_.trim).filter(_.nonEmpty)

  def intValue(v: JsValue): Option[Int] = v match {
    case JsString(s) => Try(s.trim.toInt).toOption
    case JsNumber(n) => Try(n.toInt).toOption
    case _           => None
  }
}
