package controllers

import play.api.mvc._

import scala.io.Source
import scala.util.Using

/**
 * Serves the mobile app-association files that let `https://kinowo.fly.dev/...`
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
 * fingerprint or app-ID is a one-file change with no Scala recompile of logic.
 */
class WellKnownController(cc: ControllerComponents) extends AbstractController(cc) {

  private def resource(path: String): String =
    Using.resource(Option(getClass.getResourceAsStream(path))
      .getOrElse(throw new IllegalStateException(s"missing classpath resource $path")))(
      stream => Source.fromInputStream(stream, "UTF-8").mkString)

  // Loaded once at construction so a missing/renamed resource fails fast at
  // boot rather than 500-ing the first app that probes the association file.
  private val appleAppSiteAssociationBody = resource("/wellknown/apple-app-site-association")
  private val assetLinksBody              = resource("/wellknown/assetlinks.json")

  def appleAppSiteAssociation: Action[AnyContent] = Action {
    Ok(appleAppSiteAssociationBody).as(JSON)
  }

  def assetLinks: Action[AnyContent] = Action {
    Ok(assetLinksBody).as(JSON)
  }
}
