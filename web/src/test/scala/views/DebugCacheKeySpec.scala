package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import models.{MovieRecord, Source, SourceData}

/**
 * The debug detail grid surfaces the row's cache key — the normalised
 * `(title, year)` identity that also forms the Mongo `_id`
 * (`sanitize(title)|year`). Diacritics-stripped, lower-cased, punctuation
 * removed, so every cinema-reported spelling of a film resolves to the same
 * key. Locks that the key is shown and is the sanitized form, not the raw
 * display title.
 */
class DebugCacheKeySpec extends AnyFlatSpec with Matchers {

  private val record =
    MovieRecord(data = Map[Source, SourceData]())

  private def render(title: String, year: Option[Int]): String =
    views.html.debugDetails(title, year, record).body

  "debugDetails" should "show the sanitized cache key with its year" in {
    render("Diabeł ubiera się u Prady", Some(2024)) should include ("diabelubierasieuprady|2024")
  }

  it should "drop the year suffix when the row has no year" in {
    val html = render("My Film", None)
    html should include ("myfilm")
    html should not include ("myfilm|")
  }
}
