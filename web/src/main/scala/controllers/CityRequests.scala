package controllers

import models.City
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.mvc.{Cookie, RequestHeader, Result}
import services.readmodel.WebReadModel

/** What every controller answering a `/{city}/…` route of this deployment shares:
 *  which cities are ours and the 404 for the rest ([[withCity]]), the remembered-city
 *  cookie ([[cityCookie]]), the deployment's one rendered language
 *  ([[deploymentMessages]]) and the conditional-GET machinery over the shared gzip
 *  cache ([[conditionalResponse]]). One instance, built at the composition root and
 *  handed to the listing, API, crawler and legacy-redirect controllers alike, so the
 *  four cannot drift apart on any of these.
 *
 *  `servingCountry` is the ONE country this deployment serves — which cities are
 *  ours and which the sitemap advertises. Injected rather than read from the process
 *  configuration at each use so a spec can exercise a non-Polish host by passing one,
 *  instead of mutating the process-global env that parallel suites share. */
final class CityRequests(messagesApi: MessagesApi,
                         val servingCountry: models.Country,
                         readModel: WebReadModel,
                         responseCache: EncodedResponseCache,
                         now: () => java.time.Instant,
                         // The build serving the pages — in every validator, so two builds side by
                         // side in a rolling deploy never answer one another's revalidation with a 304.
                         build: settings.CommitSha) {

  // The deployment's own, and ONLY, language. Every visitor gets this same
  // rendered `Messages` regardless of `Accept-Language`, cookie, or anything
  // else about the request — an explicit language pick swaps the visible
  // copy client-side instead (`shared.js`'s `applyLanguage`, fed by the
  // language pack `I18nPacks` embeds), so the server never has to branch per
  // visitor and every plain city URL stays edge-cacheable unconditionally
  // (see `MovieController.cacheablePlainPage`).
  //
  // BARE ("pl", not "pl-PL"), NOT `Lang(servingCountry.language)` directly —
  // `messagesApi.preferred(...)` always normalises its result down to one
  // of the region-less codes actually registered in `play.i18n.langs`.
  private val deploymentDefaultLang: Lang = Lang(servingCountry.language.getLanguage)

  /** The one `Messages` every render on this deployment uses — memoised, since it is
   *  the one language and its fixed labels never change (`MemoisedMessages`). */
  val deploymentMessages: Messages = new MemoisedMessages(messagesApi.preferred(Seq(deploymentDefaultLang)))

  // The unknown-city 404 renders in `deploymentMessages`, not a per-request
  // resolution — except on the `showtimes.cc` front door, which speaks the brand's
  // English ([[FrontDoorMessages]]); `ServedCity.resolve` picks which.
  private val frontDoorMessages: Messages = FrontDoorMessages(messagesApi)

  /** Every city-scoped handler wraps its body in this so resolution + not-found
   *  behaviour lives in one place — see `ServedCity` for the country scope. */
  def withCity(slug: String, request: RequestHeader)(f: City => Result): Result =
    ServedCity.resolve(slug, request, servingCountry, deploymentMessages, frontDoorMessages)(f)

  /** Persist the viewed city so the bare `/` landing can bounce a returning
   *  visitor straight to it. Readable by JS (httpOnly = false) so the client can
   *  also honour it; long-lived; scoped to the deployment's MOUNT POINT so it
   *  rides every request of this country's site and none of a neighbour's — on
   *  the shared brand domain a cookie at "/" would be sent to (and overwritten
   *  by) `/de` and `/us`, bouncing a UK visitor's landing to a city that country
   *  does not serve. */
  def cityCookie(city: City): Cookie =
    Cookie("city", city.slug, maxAge = Some(60 * 60 * 24 * 365), path = city.country.mountPath, httpOnly = false)

  /** Validators, `Cache-Control`, the 304 short-circuit and the gzip blob for
   *  every client-independent response served for a city. The validator starts
   *  from the city's own read-model stamp, or the model-wide one for a payload
   *  with no city in it -- see `ConditionalResponse.validatorFor` for why that
   *  distinction, and the city's calendar day, decide when a held copy retires. */
  val conditionalResponse: ConditionalResponse = new ConditionalResponse(
    responseCache,
    modelStamp = city => city.fold(readModel.lastModified)(c => readModel.lastModifiedFor(c.slug)),
    now        = now,
    build      = build,
  )
}
