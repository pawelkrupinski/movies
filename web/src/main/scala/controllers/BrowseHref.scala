package controllers

import models.City

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

/** Per-axis browse links (`/{city}/movies?country=…` etc.). City comes in
 *  implicitly so call sites in city-scoped templates read unchanged.
 *
 *  Param names are English in every country, matching the index page's own
 *  filters — the route table is shared across Poland, Germany and the UK.
 *  `BrowseFilterParamsSpec` pins them to what the routes file binds. */
object BrowseHref {
  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  def country(name: String)(implicit city: City): String  = s"/${city.slug}/movies?country=${enc(name)}"
  def director(name: String)(implicit city: City): String = s"/${city.slug}/movies?director=${enc(name)}"
  def actor(name: String)(implicit city: City): String    = s"/${city.slug}/movies?cast=${enc(name)}"
  def genre(name: String)(implicit city: City): String    = s"/${city.slug}/movies?genre=${enc(name)}"
}
