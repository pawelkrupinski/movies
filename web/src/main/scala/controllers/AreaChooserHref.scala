package controllers

import models.City

/** The URL of a chooser city's metro pick screen — `/{city}/`, plus the flag
 *  that FORCES it.
 *
 *  The flag is what makes the choice changeable. `/{city}/` bounces a visitor
 *  who has already picked a metro straight to that metro's films
 *  (`MovieController.indexOrChooser`), so a bare link back to it would land on
 *  the page the link was clicked from. `?areas` says "ask me again".
 *
 *  Centralised here because both ends need the same spelling: the controller
 *  reads the parameter off the request, the navbar's change-area link writes it.
 *  It is a query flag rather than a route of its own so it can't collide with a
 *  metro slug — `/{city}/{area}/` is a wildcard, and `/{city}/areas/` would be
 *  ambiguous the day a state has an "Areas" metro. It also stays out of the
 *  shared gzip cache for free: that cache is skipped for any request carrying a
 *  query string, so the forced chooser never shares a blob with `/{city}/`. */
object AreaChooserHref {

  /** The query parameter that forces the chooser. Presence is the signal; the
   *  value is ignored, so `?areas` and `?areas=1` both work. */
  val ForceParam: String = "areas"

  def apply(city: City): String = s"/${city.slug}/?$ForceParam"
}
