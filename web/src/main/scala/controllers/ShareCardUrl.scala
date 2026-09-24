package controllers

import models.{City, Country, ResolvedMovie}

/** Where a page's `og:image` points. The web renders no image itself: a film's card is drawn by
 *  the worker into one file per film, served from disk by Caddy at `/share-cards/<country>/<film>.jpg`,
 *  and `web_movies.shareCard` names it with its version (`<film>.jpg?v=<version>`) so a changed card
 *  is a new URL to preview caches. A city's card is the static one generated into the assets. */
object ShareCardUrl {

  /** `movie`'s share card on its country's public host, or None while it has none. The host
   *  alone, not the country's base URL: Caddy serves `/share-cards/` on every public host
   *  AHEAD of any country mount, so `showtimes.cc/share-cards/uk/…`, never `showtimes.cc/uk/…`.
   *  Never the request's host, which is whichever vhost the proxy matched (the Polish pods
   *  answer both kinowo.net and the showtimes.cc front door) — a preview card's address is
   *  the country's, like the city card it falls back to. */
  private def film(movie: ResolvedMovie, country: Country): Option[String] =
    movie.shareCard.map(card => s"${ForwardedUrl.originOf(country.ogOrigin)}/share-cards/${country.code}/$card")

  /** The city's static share card — a film page's fallback, and the repertoire page's image. */
  def city(city: City): String = s"${Country.of(city).ogOrigin}/assets/img/${city.shareImage}"

  /** A film page's `og:image` in `city`: its share card, or the city's while it has none. */
  def forFilm(movie: ResolvedMovie, city: City): String =
    film(movie, Country.of(city)).getOrElse(this.city(city))
}
