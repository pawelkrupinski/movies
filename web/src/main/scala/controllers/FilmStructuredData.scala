package controllers

import models.City
import play.api.libs.json.{JsArray, Json}

/** A film detail page's JSON-LD ([[StructuredData]] has the site's and a city's): the Movie itself, a
 *  breadcrumb, and a ScreeningEvent per showtime ([[ScreeningEvents]]) — which makes the screenings
 *  eligible for Google's event surfaces. */
object FilmStructuredData {
  import StructuredData.{Ctx, breadcrumb, originOf, render}

  def film(canonicalUrl: String, city: City, fs: FilmSchedule): String = {
    val origin  = originOf(canonicalUrl, city)
    val cityUrl = s"$origin/${city.slug}/"
    val m       = fs.movie

    // Merging an empty object adds no fields, so the optional pieces below drop
    // out cleanly when their data is absent.
    val movie = Json.obj("@context" -> Ctx, "@type" -> "Movie", "name" -> m.title, "url" -> canonicalUrl)
      .++(optStr("description", fs.synopsis))
      .++(optStr("image", fs.posterHref.map(_.url)))
      .++(seqObj("genre", m.genres))
      .++(seqPersons("director", fs.director))
      .++(seqPersons("actor", fs.cast.take(15)))
      .++(m.releaseYear.fold(Json.obj())(y => Json.obj("dateCreated" -> y.toString)))
      .++(m.runtimeMinutes.fold(Json.obj())(min => Json.obj("duration" -> s"PT${min}M")))

    val address = Json.obj(
      "@type" -> "PostalAddress",
      "addressLocality" -> city.labels.nominative,
      "addressCountry" -> city.country.language.getCountry,
    )
    val events = ScreeningEvents.of(fs.showings, m.title, canonicalUrl, city.zoneId, address)

    val crumb = breadcrumb(Seq(
      city.country.brandName -> s"$origin/", city.labels.nominative -> cityUrl, m.title -> canonicalUrl,
    ))

    render(JsArray(movie +: crumb +: events))
  }

  private def optStr(key: String, v: Option[String]) =
    v.filter(_.nonEmpty).fold(Json.obj())(s => Json.obj(key -> s))

  private def seqObj(key: String, vs: Seq[String]) =
    if (vs.isEmpty) Json.obj() else Json.obj(key -> vs)

  private def seqPersons(key: String, names: Seq[String]) =
    if (names.isEmpty) Json.obj()
    else Json.obj(key -> names.map(n => Json.obj("@type" -> "Person", "name" -> n)))
}
