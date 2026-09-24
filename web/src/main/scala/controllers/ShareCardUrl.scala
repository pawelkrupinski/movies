package controllers

import models.{City, Country, ResolvedMovie}

/** Where a page's `og:image` points. The web renders no image itself: a film's card is drawn by
 *  the worker into one file per film, served from disk by Caddy at `/share-cards/<country>/<film>.jpg`,
 *  and `web_movies.shareCard` names it with its version (`<film>.jpg?v=<version>`) so a changed card
 *  is a new URL to preview caches. A city's card is the static one generated into the assets. */
object ShareCardUrl {

  /** `movie`'s share card on `origin` (the request's scheme + host — the path is served on every
   *  public host, ahead of any country mount), or None while it has none. */
  private def film(movie: ResolvedMovie, country: Country, origin: String): Option[String] =
    movie.shareCard.map(card => s"$origin/share-cards/${country.code}/$card")

  /** The city's static share card — a film page's fallback, and the repertoire page's image. */
  def city(city: City): String = s"${Country.of(city).ogOrigin}/assets/img/${city.shareImage}"

  /** A film page's `og:image` in `city`: its share card, or the city's while it has none. */
  def forFilm(movie: ResolvedMovie, city: City, origin: String): String =
    film(movie, Country.of(city), origin).getOrElse(this.city(city))
}
