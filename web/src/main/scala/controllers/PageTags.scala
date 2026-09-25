package controllers

import tools.Env

/** The deployment's optional third-party page tags — Facebook's `fb:app_id`,
 *  the GA4 measurement id and the Sentry loader script. Each is absent unless its
 *  variable is set, so local dev, previews and fixture renders ship none of them.
 *  Read from the deployment's [[Env]] per render (see `MovieController`), so an
 *  `/admin/config` change reaches the next page without a restart. */
final case class PageTags(
    fbAppId:         Option[String] = None,
    gaMeasurementId: Option[String] = None,
    sentryLoaderUrl: Option[String] = None)

object PageTags {
  val none: PageTags = PageTags()

  def from(env: Env): PageTags =
    PageTags(env.get("FB_APP_ID"), env.get("GA_MEASUREMENT_ID"), env.get("SENTRY_LOADER_URL"))
}
