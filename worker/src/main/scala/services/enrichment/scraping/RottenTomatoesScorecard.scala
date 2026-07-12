package services.enrichment.scraping

import org.jsoup.Jsoup
import play.api.libs.json.{JsValue, Json}

import scala.jdk.CollectionConverters._
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
   *  no `criticsScore`, or a `criticsScore` without a numeric `score`. */
  def criticsScore(html: String): Option[Int] = {
    val document = Jsoup.parse(html)
    document.select("script#media-scorecard-json").asScala.iterator
      .map(_.data())
      .flatMap { raw =>
        Try(Json.parse(raw)).toOption.toSeq.flatMap { js =>
          (js \ "criticsScore" \ "score").asOpt[JsValue].flatMap(JsonScalars.intValue)
        }
      }
      .toSeq.headOption
  }
}
