package controllers

import models.{City, CityPath}
import play.api.libs.json.{JsValue, Json}

/** The Speculation Rules a city page carries (`_resourceHints`): Chromium prefetches the city's
 *  film page (`/{city}/movie/{slug}`) once the pointer rests on its link ~200 ms, or on
 *  pointerdown on touch (`moderate`), so the tap that follows lands on a response already in
 *  memory. Safari and Firefox ignore the block.
 *
 *  Prefetch, never prerender: the page is fetched, not run, so no analytics fire for a film
 *  nobody opened and nothing renders off-screen. `moderate`, never `eager`: it costs one extra
 *  origin request per link a visitor actually hovers or touches (Chrome keeps at most two such
 *  prefetches in flight), not one per card on screen — the film page is `private, no-cache`,
 *  so every prefetch is answered by Play rather than Cloudflare's edge.
 *
 *  Only this city's films: a film page sets the `city` cookie the landing redirect reads, and a
 *  prefetch's `Set-Cookie` lands even when nobody follows the link — so the "also showing in"
 *  links to other cities must not be prefetched. */
object SpeculationRules {
  def forCity(city: City): JsValue = {
    val films = s"${CityPath(city)}/movie/"
    Json.obj("prefetch" -> Json.arr(Json.obj(
      "where"     -> Json.obj("href_matches" -> s"$films*"),
      "eagerness" -> "moderate",
    )))
  }
}
