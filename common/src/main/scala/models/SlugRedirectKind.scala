package models

/** A slug a country 301s: the page it goes to, and the kind of old address it is. */
final case class SlugRedirect(to: String, kind: SlugRedirectKind)

/** Which kind of old address a slug a country 301s is ([[Country.redirect]]). */
enum SlugRedirectKind {
  /** A page's slug before it was retired or renamed (`/uk/kent/` → Chatham). */
  case Retired
  /** A slug a page was qualified to while another country held its name
   *  (`/us/birmingham-alabama/` → `/us/birmingham/`) — twelve, in the US and Spain. */
  case FormerlyQualified
  /** Another name of a place a page covers: a town on a cluster's page
   *  (`/uk/ilkley/`), a page's qualified slug (`/uk/york-north-yorkshire/`). */
  case Alias
}
