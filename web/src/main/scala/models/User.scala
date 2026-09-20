package models

import java.time.Instant

/**
 * An authenticated user — created on first successful OAuth callback,
 * looked up on every subsequent request via the session cookie's
 * `userId` claim. `id` is the user's lowercased email, so signing in with
 * Google, Facebook, or Apple under the same email resolves to the same
 * account row — and the same `UserState`. `provider + providerSub` records
 * which upstream identity the most recent login came through.
 *
 * Optional fields (email / displayName / avatarUrl) are nullable because
 * provider consent flows let the user decline specific scopes — Facebook
 * users can refuse to share email, for example. We don't gate login on
 * those, just store whatever the provider sent.
 *
 * `sessionVersion` is the revocation counter every issued session cookie
 * carries a copy of (see `SignedInUser`). Bumping it invalidates every
 * cookie issued before that moment, on every device and every deployment —
 * "log out everywhere" needs nothing more than incrementing this one field,
 * since the shared `users` database is what every deployment's session check
 * already reads. Defaulted so every existing row (no such field yet) reads
 * as `0`, matching a freshly-issued cookie's own default (see
 * `SignedInUser.apply`) — a pre-existing session keeps working, unrevoked,
 * until its owner explicitly signs out everywhere.
 */
case class User(
  id:             String,
  provider:       String,          // "google" | "facebook"
  providerSub:    String,          // provider's stable user id
  email:          Option[String],
  displayName:    Option[String],
  avatarUrl:      Option[String],
  createdAt:      Instant,
  lastSeenAt:     Instant,
  sessionVersion: Int = 0
)
