package controllers

import models.City
import play.api.i18n.Messages
import play.api.mvc.{RequestHeader, Result, Results}

/** Resolve a city slug against THIS deployment's country, 404ing anything else.
 *
 *  A slug only means something within a country — each owns its own, so `york`
 *  is York on `/uk/` and York, Pennsylvania on `/us/` — and Berlin must not
 *  resolve on the Poland host: it is a real city, just not a Polish one. Serving
 *  it 200 with an empty body would be worse than a 404, because an empty listing is
 *  indistinguishable from a genuine "nothing on today": a client caches it
 *  along with the `Last-Modified` this deployment stamps, and the German
 *  deployment then answers that timestamp with a 304, leaving the client
 *  stranded on an empty listing for a city that has a full one. That is how a
 *  cross-country deep link came up as "no screenings" in the iOS app.
 *
 *  Same scope `CrawlerController.sitemap` applies, for the same reason — a `KINOWO_COUNTRY=pl`
 *  host owns Poland's cities and nothing else. Note this is a COUNTRY scope,
 *  not a data one: a Polish city with no films today still renders (and still
 *  answers `[]`), because "we don't serve this city" and "this city is quiet
 *  tonight" are different answers.
 */
object ServedCity {

  def resolve(slug: String, servingCountry: models.Country)(f: City => Result)(implicit messages: Messages): Result =
    servingCountry.bySlug.get(slug) match {
      case Some(c) => f(c)
      case None    => unknown(slug)
    }

  /** [[resolve]] for a city-scoped route, its 404 in the language the HOST speaks: the
   *  `showtimes.cc` front door's English ([[FrontDoorMessages]]) on the apex — which
   *  Poland's deployment answers — and the deployment's own everywhere else, so
   *  `showtimes.cc/xyzzy/…` reads "Unknown city" on every route while kinowo.net stays
   *  Polish. The one place that choice is made, for every controller with city routes. */
  def resolve(slug: String, request: RequestHeader, servingCountry: models.Country,
              deploymentMessages: Messages, frontDoorMessages: Messages)(f: City => Result): Result =
    resolve(slug, servingCountry)(f)(
      using if (servingCountry.servesApex(PageMeta.host(request))) frontDoorMessages else deploymentMessages)

  /** The answer for a slug this deployment does not serve — also what the
   *  `showtimes.cc` front door gives a Polish place (`FrontDoorRedirectFilter`),
   *  so it reads exactly like any other unknown city there. */
  def unknown(slug: String)(implicit messages: Messages): Result =
    Results.NotFound(messages("error.unknownCity", slug))
}
