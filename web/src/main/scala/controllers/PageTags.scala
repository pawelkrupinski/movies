package controllers

import settings.{FacebookPageAppId, GoogleAnalyticsMeasurementId, ProcessConfiguration, SentryLoaderUrl}

/** The deployment's optional third-party page tags — Facebook's `fb:app_id`,
 *  the GA4 measurement id and the Sentry loader script. Each is absent unless its
 *  variable is set, so local dev, previews and fixture renders ship none of them.
 *  Resolved from the deployment's configuration per render (see `MovieController`), so an
 *  `/admin/config` change reaches the next page without a restart. */
final case class PageTags(
    fbAppId:         Option[FacebookPageAppId] = None,
    gaMeasurementId: Option[GoogleAnalyticsMeasurementId] = None,
    sentryLoaderUrl: Option[SentryLoaderUrl] = None)

object PageTags {
  val none: PageTags = PageTags()

  def from(configuration: ProcessConfiguration): PageTags =
    PageTags(configuration.facebookPageAppId, configuration.googleAnalyticsId, configuration.sentryLoaderUrl)
}
