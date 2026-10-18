package models

import java.util.Locale

/** The origins the deployments answer on, and the brand domain several of them share: what a
 *  request's host or origin says about where it is, for the front door, the sign-in relay and the
 *  cross-site write check. Which country a deployment serves is [[Country]]'s. */
object DeploymentOrigins {
  /** `url` with its scheme stripped — `kinowo.net`, `showtimes.cc/uk`. For the
   *  places that DISPLAY an address rather than link to it: the footer drawn
   *  into every Open Graph card ([[Country.shareHost]]), the button on a retired
   *  deployment's notice page. */
  def withoutScheme(url: String): String = url.stripPrefix("https://").stripPrefix("http://")

  /** THE BRAND FRONT DOOR: the bare domain that fronts several countries rather
   *  than serving one. `showtimes.cc/uk` and `showtimes.cc/de` are countries;
   *  `showtimes.cc/` itself is this — a picker listing every deployed country,
   *  Poland included as one link to its own domain (its places are not
   *  addressed under showtimes.cc at all: the front door 404s them).
   *
   *  It is a check on the REQUEST rather than a deployment of its own,
   *  deliberately: there is no fourth thing to keep running, the proxy simply
   *  points the apex root at a deployment that is mounted there. Since the
   *  Showtimes countries share this host, that can only be the country on its
   *  own domain — see [[Country.servesApex]]. */
  val apexHost: String = "showtimes.cc"

  /** The brand named on the front door — the non-Polish brand, since the apex is
   *  the Showtimes domain. Poland keeps its own brand on its own domain. */
  val apexBrandName: String = "Showtimes"

  /** THE ONE ORIGIN EVERY OAUTH PROVIDER REDIRECTS BACK TO.
   *
   *  Google and Facebook each match `redirect_uri` byte-for-byte against a list
   *  registered in their console, so the obvious shape — every deployment naming
   *  its own address — costs one console entry per country per provider, and
   *  makes "register two URLs by hand, in two consoles" a silent prerequisite for
   *  launching a country. It is silent because nothing fails until a real person
   *  tries to sign in on the new site.
   *
   *  So every country sends the provider HERE, and the deployment mounted at
   *  the apex ([[Country.servesApex]]) either finishes the flow — when the country that
   *  started it is on this same origin, so the browser is still sending the
   *  cookie holding its CSRF state — or relays it, unchanged, to the one that
   *  can. A country on its own domain is reached the second way.
   *
   *  The cost is a real dependency: the apex deployment is now in the path of
   *  every sign-in, including Poland's own. That is the trade the single entry
   *  buys, and it is why this is a constant rather than something each country
   *  could quietly point elsewhere. */
  val oauthCallbackOrigin: String = s"https://$apexHost"

  /** The origins this project actually answers on. Used to tell a REAL request
   *  from a local one: on a deployed origin the provider was handed
   *  [[oauthCallbackOrigin]] and the relay applies, while a developer on
   *  `http://localhost:9000` was handed their own address and must be left to
   *  finish where they started. Derived from the countries rather than listed,
   *  so a new deployment cannot be forgotten here. */
  lazy val deployedOrigins: Set[String] = Country.switchable.flatMap(_.webOrigin).toSet

  /** The OTHER domain this project is served from, seen from the origin a request
   *  actually ARRIVED on — the one a session has to be established on separately,
   *  because no cookie can reach it from here.
   *
   *  THE ORIGIN, NOT THE DEPLOYMENT'S COUNTRY, and the difference is the whole
   *  point: the process mounted at the apex serves `showtimes.cc` while ITS
   *  country is Poland, so asking what Poland's sibling is answers
   *  `showtimes.cc` — the domain the request is already on. A sign-in finishing
   *  there would pair that domain with itself and leave kinowo.net untouched.
   *
   *  `None` when there is not exactly one other: a request off a deployed origin
   *  (a developer on localhost) has no sibling to speak of and must not be sent
   *  to production to look for one, and a third domain would make "the other one"
   *  meaningless rather than merely unknown. */
  def siblingOfOrigin(origin: String): Option[String] = {
    if (!deployedOrigins.contains(origin)) None
    else {
      val others = (deployedOrigins - origin).toSeq
      if (others.sizeIs == 1) others.headOption else None
    }
  }

  /** Is this request host the brand domain? Accepts the `www.` spelling and an
   *  explicit port so a direct hit still works where the proxy's redirect is not
   *  in front of it (local dev, a stale cache).
   *
   *  Being the brand domain is NOT on its own being the front door — every
   *  Showtimes country now serves off it too. [[Country.servesApex]] is the
   *  question callers want. */
  def isApexHost(host: String): Boolean =
    host.toLowerCase(Locale.ROOT).takeWhile(_ != ':').stripPrefix("www.") == apexHost
}
