package controllers

import models.{City, Country, ResolvedMovie}

/** Where a page's `og:image` points. The web renders no image itself: film and city cards are drawn
 *  by the worker, one file each, served from disk by Caddy at `/share-cards/<country>/<file>.jpg`.
 *  A film's `web_movies.shareCard` names its file with its version (`<film>.jpg?v=<version>`), so
 *  a changed card is a new URL to preview caches; a city's is its bare name (`city-<slug>.jpg`,
 *  re-rendered weekly — Caddy caches a bare card for five minutes, and a city with no card yet is
 *  redirected to its country's home card).
 *
 *  Every card is on its country's public HOST alone, not its base URL: Caddy serves
 *  `/share-cards/` on every public host AHEAD of any country mount, so `showtimes.cc/share-cards/uk/…`,
 *  never `showtimes.cc/uk/…`. Never the request's host, which is whichever vhost the proxy matched
 *  (the Polish pods answer both kinowo.net and the showtimes.cc front door) — a preview card's
 *  address is the country's. */
object ShareCardUrl {

  private def onDisk(country: Country, file: String): String =
    s"${ForwardedUrl.originOf(country.ogOrigin)}/share-cards/${country.code}/$file"

  /** `movie`'s share card, or None while it has none. */
  private def film(movie: ResolvedMovie, country: Country): Option[String] =
    movie.shareCard.map(onDisk(country, _))

  /** The city's share card — a film page's fallback, and the repertoire page's image. */
  def city(city: City): String = onDisk(Country.of(city), city.shareImage)

  /** The country's landing card, a static asset — what a city with no card yet falls back to. */
  def home(country: Country): String = s"${country.ogOrigin}/assets/img/${country.homeOgImage}"

  /** A film page's `og:image` in `city`: its share card, or the city's while it has none. */
  def forFilm(movie: ResolvedMovie, city: City): String =
    film(movie, Country.of(city)).getOrElse(this.city(city))
}
