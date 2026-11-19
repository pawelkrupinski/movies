package controllers

import models.City
import play.api.mvc.{RequestHeader, Result}

/** The faceted browse pages (`/{city}/movies?country=|director=|cast=|genre=`, [[MovieController.browse]]): the
 *  city's films narrowed to one facet's name, client-independent like the listing (nobody is rendered into it), but
 *  kept out of every index. */
final class BrowsePage(
  cityRequests: CityRequests,
  movieControllerService: MovieControllerService,
  oauthProviders: Set[String],
  // The listing's: see [[ListingHtml]].
  minifier: tools.Minifier,
  pageTags: () => PageTags,
  filmCards: FilmCardFragments,
) {
  import cityRequests.{cityCookie, deploymentMessages}

  private implicit val fragments: FilmCardFragments = filmCards

  /** The page of the first facet given (country, director, cast, genre, in that order), or `None` for none: the
   *  caller's to render as the main listing. */
  def render(city: City, country: Option[String], director: Option[String], cast: Option[String], genre: Option[String],
             request: RequestHeader): Option[Result] = {
    val all = movieControllerService.toSchedules(city)
    (country, director, cast, genre) match {
      case (Some(name), _, _, _) => Some(renderBrowse(city, name, all.filter(_.movie.countries.contains(name)), request))
      case (_, Some(name), _, _) => Some(renderBrowse(city, name, all.filter(_.director.contains(name)),        request))
      case (_, _, Some(name), _) => Some(renderBrowse(city, name, all.filter(_.cast.contains(name)),            request))
      case (_, _, _, Some(name)) => Some(renderBrowse(city, name, all.filter(_.movie.genres.contains(name)),    request))
      case _                     => None
    }
  }

  private def renderBrowse(city: City, heading: String, films: Seq[FilmSchedule], request: RequestHeader): Result = {
    implicit val c: City = city
    implicit val messages: play.api.i18n.Messages = deploymentMessages
    // Client-independent like the listing (nobody is rendered into it), but a
    // facet URL is one of combinatorially many and earns no edge entry.
    ResponseBody.okHtml(views.html.browse(
      films, heading, minifier, oauthProviders,
      pageUrl = PageMeta.canonicalUrl(request),
      pageTags = pageTags(),
      // A FACET IS UI STATE, NOT A PAGE. `?cast=` alone is one URL per cast
      // member per city, so the set of these is combinatorial rather than
      // merely large, and every one of them is a near-duplicate of the city
      // listing built from the same films. og:url keeps the filtered URL (a
      // shared filtered link should preview its filter), the canonical folds
      // them back onto the listing, and `noindex,follow` keeps the crawler
      // walking through to the film pages, which ARE the content.
      //
      // robots.txt already disallows this path; this is the second line, for
      // the crawlers that ignore it — see the note on `_ogTagsApp`.
      canonicalUrl = PageMeta.listingUrl(request, city),
      robots = BrowsePage.FacetRobots,
    )).withHeaders("Cache-Control" -> "private, no-cache").withCookies(cityCookie(city))
  }
}

object BrowsePage {

  /** What the faceted browse pages tell a crawler about themselves.
   *
   *  `follow` and not `none`: the point is to keep the facet URLs out of an
   *  index, not to hide the film links they carry. Those links are the reason
   *  the page is worth crawling at all, and each one lands on a film page that
   *  IS indexable. */
  val FacetRobots = "noindex,follow"
}
