package controllers

import models.City
import play.api.i18n.Messages
import play.api.mvc.{Result, Results}

/** Resolve a city slug against THIS deployment's country, 404ing anything else.
 *
 *  Resolving is not enough on its own: `City.bySlug` searches the global
 *  `City.all` (the union across every country), so Berlin resolves on the
 *  Poland host too — it is a real city, just not a Polish one. Serving it 200
 *  with an empty body is worse than a 404, because an empty listing is
 *  indistinguishable from a genuine "nothing on today": a client caches it
 *  along with the `Last-Modified` this deployment stamps, and the German
 *  deployment then answers that timestamp with a 304, leaving the client
 *  stranded on an empty listing for a city that has a full one. That is how a
 *  cross-country deep link came up as "no screenings" in the iOS app.
 *
 *  Same scope `MovieController.sitemap` applies, for the same reason — a `KINOWO_COUNTRY=pl`
 *  host owns Poland's cities and nothing else. Note this is a COUNTRY scope,
 *  not a data one: a Polish city with no films today still renders (and still
 *  answers `[]`), because "we don't serve this city" and "this city is quiet
 *  tonight" are different answers.
 */
object ServedCity {

  def resolve(slug: String, servingCountry: models.Country)(f: City => Result)(implicit messages: Messages): Result =
    City.bySlug(slug).filter(servingCountry.cities.contains) match {
      case Some(c) => f(c)
      case None    => Results.NotFound(messages("error.unknownCity", slug))
    }
}
