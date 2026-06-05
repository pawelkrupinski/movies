package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks in the contract that the Debug nav link is dev-only AND
 * hidden on mobile (portrait + landscape). The CSS rule that hides
 * it keys on the `nav-tab-debug` class, so the test also asserts
 * that class is present whenever the link renders — otherwise the
 * "hide on mobile" rule would silently no-op.
 */
class NavbarDebugLinkSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def render(devMode: Boolean): String =
    views.html._navbar(activePage = "films", devMode = devMode,
      currentUser = None, oauthProviders = Set.empty).body

  "navbar partial" should "omit the Debug link entirely when devMode is false" in {
    val html = render(devMode = false)
    html should not include "/debug"
    html should not include "nav-tab-debug"
  }

  it should "render the Debug link with the `nav-tab-debug` class when devMode is true" in {
    val html = render(devMode = true)
    html should include ("""href="/poznan/debug"""")
    // The class lets the (max-width: 575px) / landscape CSS rules in
    // `_sharedStyles` hide the link on mobile viewports.
    html should include ("nav-tab-debug")
  }

  "shared styles" should "hide `.nav-tab-debug` on the mobile media queries" in {
    // Render `_sharedStyles` directly so the stylesheet block is in
    // the rendered HTML. The CSS minifier collapses ` : ` → `:`, so
    // assert on the minified shape that ships to the browser.
    val css = views.html._sharedStyles(devMode = false).body
    css should include (".nav-tab-debug{display:none}")
    // Anchor the rule under both mobile media queries so a future
    // tidy-up can't drop one arm of the OR.
    css should include ("max-width:575px")
    css should include ("max-height:500px) and (orientation:landscape")
  }
}
