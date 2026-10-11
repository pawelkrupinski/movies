package controllers

import models.Country
import play.twirl.api.Html

/**
 * Every deployed country's `/` picker fragment (`_pickerPane.scala.html`), rendered
 * ONCE when this is built and kept for the life of the process: the roster is fixed
 * per deploy, so a per-request render would only redo identical work. The landing
 * embeds its own country's from here ([[pane]]); `GET /api/picker/:country` serves
 * the others ([[PickerController]]) — one source of markup for both.
 *
 * Language-neutral, so one copy per country serves every visitor: the fragment
 * holds nothing but place and region names (see the template), and the picker
 * chrome around it is what `applyLanguage` relabels.
 *
 * `render` is the seam a spec counts renders through.
 */
final class PickerFragments(render: Country => String) {

  private val bodies: Map[String, FixedBody] =
    Country.switchable.map(c => c.code -> FixedBody.html(render(c))).toMap

  def body(countryCode: String): Option[FixedBody] = bodies.get(countryCode)

  /** `country`'s fragment as markup for the landing to embed. */
  def pane(country: Country): Html = Html(bodies.get(country.code).fold("")(_.text))

  /** Where the landing fetches `countryCode`'s fragment: versioned by its content, so
   *  the URL changes exactly when the bytes do and may be cached for good (see
   *  [[PickerController]]). */
  def url(countryCode: String): String =
    routes.PickerController.fragment(countryCode, bodies.get(countryCode).map(_.version)).url

  /** The heap these hold: each fragment's text (two bytes a char — place names are not
   *  Latin-1) and its gzipped copy. */
  def heldBytes: Long = bodies.values.map(b => 2L * b.text.length + b.heldBytes).sum
}

object PickerFragments {
  def rendered(): PickerFragments = new PickerFragments(render)

  /** `country`'s fragment, as the template renders it. */
  def render(country: Country): String =
    views.html._pickerPane(country.code, country.webUrl.getOrElse(""), country.cityGroups, country.allSorted).body.trim
}
