package controllers

import models.InstalledBuildSlugs
import play.api.libs.json.{JsArray, JsObject, JsTrue, Json}
import play.api.mvc._

import scala.io.Source
import scala.util.Using

/**
 * Serves the mobile app-association files that let `https://kinowo.net/...`
 * URLs open the native apps directly (iOS Universal Links, Android App Links)
 * instead of the website. The same links — including the copy-to-clipboard
 * filter links — therefore deep-link into the app when it is installed and fall
 * back to the browser when it is not.
 *
 *  - `/.well-known/apple-app-site-association` — iOS. MUST be served as
 *    `application/json` with NO file extension (Apple's CDN fetches this exact
 *    path); the bytes live in `resources/wellknown/apple-app-site-association`.
 *  - `/.well-known/assetlinks.json` — Android Digital Asset Links.
 *
 * The bodies are checked-in resources rather than rendered here so editing a
 * fingerprint or app-ID is a one-file change with no Scala recompile of logic —
 * with one generated addition to the AASA, [[WellKnownController.withInstalledBuildExcludes]].
 */
class WellKnownController(cc: ControllerComponents) extends AbstractController(cc) {

  private def resource(path: String): String =
    Using.resource(Option(getClass.getResourceAsStream(path))
      .getOrElse(throw new IllegalStateException(s"missing classpath resource $path")))(
      stream => Source.fromInputStream(stream, "UTF-8").mkString)

  // Loaded once at construction so a missing/renamed resource fails fast at
  // boot rather than 500-ing the first app that probes the association file.
  private val appleAppSiteAssociationBody =
    WellKnownController.withInstalledBuildExcludes(resource("/wellknown/apple-app-site-association"))
  private val assetLinksBody              = resource("/wellknown/assetlinks.json")

  def appleAppSiteAssociation: Action[AnyContent] = Action {
    Ok(appleAppSiteAssociationBody).as(JSON)
  }

  def assetLinks: Action[AnyContent] = Action {
    Ok(assetLinksBody).as(JSON)
  }
}

object WellKnownController {

  /**
   * TEMPORARY, with [[models.LegacySlugOwners]]: `aasa` with an exclude for every
   * city link installed iOS builds from before slugs were per country
   * (`mobile-ios-2.0.11`) open on the wrong city ([[InstalledBuildSlugs.misopened]])
   * — its listing and everything under it. Such a build drops a link's country
   * segment and picks the city by slug, so `/uk/york/` would open Pennsylvania's
   * York and `/uk/rochester/` (a UK town on Chatham's page) Rochester, New York;
   * excluded, the link opens in Safari on the right page. Inserted after the file's own excludes,
   * ahead of the claims they carve out of: iOS takes the first component that
   * matches.
   */
  def withInstalledBuildExcludes(aasa: String): String = {
    val json       = Json.parse(aasa).as[JsObject]
    val details    = (json \ "applinks" \ "details").as[Seq[JsObject]]
    val components = (details.head \ "components").as[Seq[JsObject]]
    val (excludes, claims) = components.span(c => (c \ "exclude").asOpt[Boolean].contains(true))
    val added = InstalledBuildSlugs.misopened.flatMap { case (country, segment) =>
      Seq(s"${country.pathPrefix}/$segment/", s"${country.pathPrefix}/$segment/*")
        .map(path => Json.obj("/" -> path, "exclude" -> JsTrue))
    }
    val detail   = details.head + ("components" -> JsArray(excludes ++ added ++ claims))
    val applinks = (json \ "applinks").as[JsObject] + ("details" -> JsArray(detail +: details.tail))
    Json.prettyPrint(json + ("applinks" -> applinks))
  }
}
