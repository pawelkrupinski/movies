package services.auth

/**
 * Result of an OAuth callback flow — normalised across providers so the
 * downstream `User` upsert doesn't care whether the source is Google's
 * `userinfo` endpoint or Facebook's `/me` graph call.
 *
 * `sub` is the provider's stable unique id (Google's `sub` JWT claim,
 * Facebook's `id`). `email` / `displayName` / `avatarUrl` are
 * `Option` because OAuth consent screens let users decline specific
 * scopes — most notably Facebook lets users refuse email sharing, and
 * some Google Workspace accounts ship without a picture.
 */
case class OauthProfile(
  sub:         String,
  email:       Option[String],
  displayName: Option[String],
  avatarUrl:   Option[String]
)

/**
 * Single contract every OAuth identity provider obeys. Two steps in
 * the authorization-code flow:
 *
 *   1. `authUrl` — where to redirect the user's browser so they can
 *      authorise the app. Carries the `state` we'll verify on
 *      callback (CSRF) and the `redirect_uri` the provider redirects
 *      back to.
 *
 *   2. `exchangeCode` — POST the returned `code` to the provider's
 *      token endpoint, then GET userinfo. Returns the normalised
 *      profile. Throws on any HTTP / parse failure; `AuthController`
 *      catches and renders a generic "couldn't sign you in" page.
 *
 * Implementations only depend on `HttpFetch` so tests can drop in a
 * fake (the test fakes the token + userinfo HTTP responses, the trait
 * surface is exercised end-to-end without touching the network).
 */
trait OauthProvider {
  /** Stable name persisted in `User.provider`. Must match the `:provider`
   *  path segment in the auth routes. */
  def name: String

  def authUrl(state: String, redirectUri: String): String

  def exchangeCode(code: String, redirectUri: String): OauthProfile
}
