package controllers

import models.{Country, ResolvedMovie}
import play.api.mvc.RequestHeader

/** One card on the dev-only share-card gallery: its caption, where production serves it (`None`: the film has no
 *  card yet), and where this dev server shows it (`None`: its file is not on this machine — "not drawn yet"). */
final case class GalleryCard(label: String, productionUrl: Option[String], localUrl: Option[String])

/** What the two gallery pages ([[ShareCardGalleryController]]) show: every city card of a country plus its home
 *  card, and a sample of the films it is showing — each at its production address ([[ShareCardUrl]]) and, when the
 *  file is on disk ([[ShareCardFiles]]), at the dev route that serves it. */
object ShareCardGallery {
  val DefaultCountry: Country = Country.Poland
  val DefaultFilmLimit: Int   = 30

  /** `?country=`, else Poland. */
  def countryOf(request: RequestHeader): Country =
    request.getQueryString("country").flatMap(Country.byCode).getOrElse(DefaultCountry)

  /** The country's landing card, a checked-in asset this server always has. */
  def homeCard(country: Country): GalleryCard =
    GalleryCard(s"home (${country.homeOgImage})", Some(ShareCardUrl.home(country)),
      Some(routes.Assets.versioned(Assets.Asset(s"img/${country.homeOgImage}")).url))

  /** Every city of the country's roster, in roster order. */
  def cityCards(country: Country, files: ShareCardFiles): Seq[GalleryCard] =
    country.cities.map(city => GalleryCard(city.labels.nominative, Some(ShareCardUrl.city(city)),
      local(country, city.shareImage, files)))

  /** The first `limit` films by title. */
  def filmCards(films: Seq[ResolvedMovie], country: Country, limit: Int, files: ShareCardFiles): Seq[GalleryCard] =
    films.sortBy(_.title).take(limit).map(film => GalleryCard(film.title, ShareCardUrl.film(film, country),
      film.shareCard.map(ShareCardFiles.fileOf).flatMap(local(country, _, files))))

  private def local(country: Country, file: String, files: ShareCardFiles): Option[String] =
    files.find(country, file).map(_ => routes.ShareCardGalleryController.file(country.code, file).url)
}
