package controllers

import models.{Country, ResolvedMovie}
import play.api.mvc._
import services.readmodel.WebReadModel

/**
 * `/admin/share-cards` — every share card this deployment's country names, to look over by eye: one tab
 * of its cities' cards, one of its films', each with a search box. The images are the very URLs the
 * pages' `og:image` points at ([[ShareCardUrl]]), so what shows here is what a preview scraper fetches;
 * a city whose card is not drawn yet shows Caddy's fallback, a film with none says so.
 *
 * Gated by [[AdminAction]] (login + ADMIN_ALLOWLIST), like the other `/admin` pages.
 */
class ShareCardGalleryController(cc: ControllerComponents, adminAction: AdminAction, readModel: WebReadModel,
                                 country: Country) extends AbstractController(cc) {

  def index: Action[AnyContent] = adminAction {
    Ok(views.html.admin.shareCards(ShareCardGalleryController.Gallery.of(country, readModel.allMovies())))
  }
}

object ShareCardGalleryController {

  /** `image` is the card's address — None only if it were not http(s), which [[ShareCardUrl]] never builds. */
  final case class CityCard(name: String, slug: String, image: Option[WebHref])

  /** `image` is None while the film has no card: its pages then show the city's. */
  final case class FilmCard(title: String, year: Option[Int], image: Option[WebHref], pending: Boolean)

  final case class Gallery(country: Country, cities: Seq[CityCard], films: Seq[FilmCard]) {
    def filmsWithoutCard: Int = films.count(_.image.isEmpty)
  }

  object Gallery {
    def of(country: Country, movies: Seq[ResolvedMovie]): Gallery = Gallery(
      country,
      country.cities.map(c => CityCard(c.labels.nominative, c.slug, WebHref.of(ShareCardUrl.city(c)))).sortBy(_.name),
      movies.map(m => FilmCard(m.title, m.releaseYear, ShareCardUrl.film(m, country).flatMap(WebHref.of), m.shareCardPending))
        .sortBy(f => (f.image.isDefined, f.title)))
  }
}
