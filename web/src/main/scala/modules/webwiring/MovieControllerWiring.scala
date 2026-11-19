package modules.webwiring

import controllers.{BrowsePage, CityRequests, DevMode, FilmCardFragments, ListingHtml, ListingRenders, ListingResponse,
  MovieController, MovieControllerService, PageTags}
import play.api.Mode
import play.api.mvc.ControllerComponents

/** The city listing's controller and its pieces — the listing's HTML, how it is served, the browse pages — wired over
 *  one deployment's [[CityRequests]] and schedules: the composition root's (`ControllersWiring`) and the controller
 *  specs' one assembly. */
object MovieControllerWiring {
  def movieController(cc: ControllerComponents, cityRequests: CityRequests, movieControllerService: MovieControllerService,
                      oauthProviders: Set[String], environment: Mode, minifier: tools.Minifier, pageTags: () => PageTags,
                      filmCards: FilmCardFragments, recordRender: (String, Long) => Unit): MovieController = {
    val html = new ListingHtml(oauthProviders, DevMode.enabled(environment), minifier, pageTags, filmCards)
    new MovieController(cc, cityRequests,
      new ListingResponse(cityRequests, movieControllerService, html, new ListingRenders(recordRender)),
      new BrowsePage(cityRequests, movieControllerService, oauthProviders, minifier, pageTags, filmCards))
  }
}
