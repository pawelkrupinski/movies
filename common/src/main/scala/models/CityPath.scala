package models

/** The absolute path every one of a city's pages hangs off — `/poznan` on a
 *  country that owns its domain, `/uk/kent` on one mounted under a country
 *  segment of the shared brand domain. No trailing slash: callers append the
 *  page (`+ "/"`, `+ "/movie/$slug"`).
 *
 *  Exists because the mount point is the easiest thing in this codebase to
 *  forget. Play strips `play.http.context` before matching routes, so inside a
 *  request handler the city slug is all you see — and a URL built from the slug
 *  alone still looks right in every Poland-based test while pointing off-site on
 *  the three countries that share `showtimes.cc`. It has gone wrong three
 *  separate ways already (a canonical tag, the browse links, and a sitemap that
 *  doubled the prefix by adding it to a base that already carried it), so the
 *  rule lives in one place and every builder goes through it.
 *
 *  Taken from the CITY rather than from the router's published prefix or the
 *  process environment: a deployment only ever links to cities of the country it
 *  serves, so the city's country IS the mount point — and the rule stays pure
 *  and testable for every country at once. */
object CityPath {
  def apply(city: City): String = s"${city.country.pathPrefix}/${city.slug}"

  /** A film page under the city — `/poznan/movie/diuna-2` — for the film's assigned slug
   *  (`services.readmodel.FilmSlugs`). The web's links and the worker's share-card re-scrape
   *  requests both build it here, so the two cannot address different pages. */
  def film(city: City, slug: String): String = s"${apply(city)}/movie/$slug"
}
