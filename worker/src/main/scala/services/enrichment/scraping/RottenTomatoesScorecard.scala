package services.enrichment.scraping

import play.api.libs.json.{JsValue, Json}

import scala.util.Try

/**
 * Extract the Tomatometer (critics) percentage from RT's `media-scorecard-json`
 * data island — a `<script id="media-scorecard-json" type="application/json">`
 * block whose `criticsScore.score` field carries the percentage ("94"), the
 * value RT now hydrates the visual score board from.
 *
 * RT dropped `aggregateRating.ratingValue` from the JSON-LD on most movie
 * pages, so this data island is the primary signal for the Tomatometer;
 * [[JsonLdAggregateRating]] is the fallback for pages that still publish it.
 * Pages with no rated critics leave `criticsScore` present but without a
 * `score` field, so a missing score yields None rather than a bogus value.
 */
object RottenTomatoesScorecard {

  /** First numeric `criticsScore.score` found in a `media-scorecard-json`
   *  block, parsed as Int. Returns None when the page has no scorecard block,
   *  no `criticsScore`, or a `criticsScore` without a numeric `score`. The block
   *  is found by [[HtmlScripts]], not a parsed DOM: the island is the page's only
   *  use here, and parsing every rating page whole for it was a fifth of a
   *  rating refresh's CPU (JFR, US convergence leg). `JsonLdScanSpec` holds it to Jsoup's `script#media-scorecard-json`. */
  def criticsScore(html: String): Option[Int] =
    blocks(html).iterator
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>
          (js \ "criticsScore" \ "score").asOpt[JsValue].flatMap(JsonScalars.intValue)
        }
      }
      .nextOption()

  // The `id` attribute, its name in any case and its value exactly — Jsoup's `#id` — however it is quoted.
  private val ScorecardId = """(?:^|\s)(?i:id)\s*=\s*(["']?)media-scorecard-json\1(?=[\s/>]|$)""".r

  /** The raw text of every `<script id="media-scorecard-json">`, in page order. */
  private[scraping] def blocks(html: String): Seq[String] =
    HtmlScripts.all(html).collect { case s if ScorecardId.findFirstIn(s.attributes).isDefined => s.body }.toSeq
}
