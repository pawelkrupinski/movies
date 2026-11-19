package controllers

import org.apache.pekko.util.ByteString
import play.api.Mode
import play.api.mvc._

import java.nio.file.Files

/** The dev-only share-card gallery: `/debug/share-cards/cities` (every city card of `?country=` plus its home card)
 *  and `/debug/share-cards/movies` (the cards of the first `?limit=` films it is showing), and the card files
 *  themselves off this machine's disk — production's Caddy serves `/share-cards/`, which a dev server has not got.
 *  All 404 in production ([[DevMode]]). */
class ShareCardGalleryController(cc: ControllerComponents, environment: Mode,
                                 // The films each country is showing: its read model, from the local mirror in Dev.
                                 readModels: DebugReadModels,
                                 files: ShareCardFiles) extends AbstractController(cc) {

  def cities(): Action[AnyContent] = Action { request =>
    DevMode.gate(environment) {
      val country = ShareCardGallery.countryOf(request)
      Ok(views.html.shareCardCities(country, ShareCardGallery.homeCard(country) +: ShareCardGallery.cityCards(country, files),
        files.directoryOf(country)))
    }
  }

  def movies(limit: Option[Int]): Action[AnyContent] = Action { request =>
    DevMode.gate(environment) {
      val country = ShareCardGallery.countryOf(request)
      if (!readModels.countries.contains(country))
        NotFound(s"no ${country.code} read model on this server: scripts/dev-server.sh reads every country's from the local mirror")
      else readModels(country).reader.findAllMoviesChecked().answered match {
        case Some(films) =>
          val cards = ShareCardGallery.filmCards(films, country, limit.filter(_ > 0).getOrElse(ShareCardGallery.DefaultFilmLimit), files)
          Ok(views.html.shareCardMovies(country, cards, films.size, files.directoryOf(country)))
        case None => ServiceUnavailable(s"could not read the ${country.code} read model whole")
      }
    }
  }

  /** One card file of `country`'s share-card directory, as production's Caddy serves it. */
  def file(country: String, file: String): Action[AnyContent] = Action {
    DevMode.gate(environment) {
      models.Country.byCode(country).flatMap(files.find(_, file))
        .fold(NotFound(s"no card $file for $country on this machine"))(path => Ok(ByteString(Files.readAllBytes(path))).as("image/jpeg"))
    }
  }
}
