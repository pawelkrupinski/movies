package controllers

import models.City
import play.api.mvc.*

/** The city listing (`/{city}/`, `/{city}/movies`, [[ListingResponse]]) and its faceted browse pages ([[BrowsePage]]),
 *  each behind our cities' check and the genre-link canonicalisation. */
class MovieController( cc: ControllerComponents,
                       // Which cities are ours, the remembered-city cookie, the deployment's
                       // one language and the conditional GET — shared with every other
                       // city-route controller (`CityRequests`).
                       cityRequests: CityRequests,
                       // NO `UserRepository` — deliberately, and it is the strongest
                       // form of the promise `CachePolicy` needs. This
                       // controller renders every page it serves without the means to
                       // find out who asked for it, so no response of its can vary by
                       // session, so a shared cache holding the listing cannot leak
                       // anybody.
                       // Who is signed in is `SessionController`'s question, answered
                       // per client at `/api/me`.
                       listing: ListingResponse,
                       browsePage: BrowsePage,
                     ) extends AbstractController(cc) {
  // The film pages are [[FilmPageController]]'s.

  import cityRequests.withCity

  /** How many listings' JSON-LD this controller is holding — for the spec. */
  private[controllers] def structuredDataHeld: Long = listing.structuredDataHeld

  def index(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request)(c => canonicalGenreLink(c, "", request).getOrElse(listing.renderIndex(c, request)))
  }

  /** A link spelling a genre as a venue printed it 301s onto the vocabulary's spelling ([[GenreLinks]]). */
  private def canonicalGenreLink(c: City, tail: String, request: RequestHeader): Option[Result] =
    GenreLinks.canonical(c, tail, request.queryString).map(MovedPermanently(_))

  /** The four legacy Polish param names (`kraj`/`rezyser`/`aktor`/`gatunek`) are still
   *  bound and still filter. They were the only spelling until the facets were renamed to
   *  English for the shared route table, so every link minted before that — a bookmark, a
   *  shared URL, anything already crawled — carries them. Dropping the binding did not 404
   *  those; it fell through to the no-axis case and rendered the UNFILTERED city listing,
   *  a 200 with the wrong content, which is the failure mode nobody reports. The English
   *  name wins when both are present. */
  def browse(city: String, country: Option[String], director: Option[String], cast: Option[String], genre: Option[String],
             kraj: Option[String] = None, rezyser: Option[String] = None,
             aktor: Option[String] = None, gatunek: Option[String] = None): Action[AnyContent] = Action { request =>
    withCity(city, request) { c => canonicalGenreLink(c, "movies", request).getOrElse {
      // `/{city}/movies` with no filter axis is the main listing — the same
      // view as `/{city}/`. The browse view only kicks in for the per-axis
      // pages reached from the meta-link rows on /movie.
      browsePage.render(c, country.orElse(kraj), director.orElse(rezyser), cast.orElse(aktor), genre.orElse(gatunek), request)
        .getOrElse(listing.renderIndex(c, request))
    } }
  }
}
