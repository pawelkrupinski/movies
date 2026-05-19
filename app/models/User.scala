package models

import java.time.Instant

/**
 * An authenticated user — created on first successful OAuth callback,
 * looked up on every subsequent request via the session cookie's
 * `userId` claim. `provider + providerSub` is the unique key from the
 * upstream identity provider; `id` is our own UUID, stable across email
 * changes / display-name edits / provider-side data shifts.
 *
 * Optional fields (email / displayName / avatarUrl) are nullable because
 * provider consent flows let the user decline specific scopes — Facebook
 * users can refuse to share email, for example. We don't gate login on
 * those, just store whatever the provider sent.
 */
case class User(
  id:          String,
  provider:    String,          // "google" | "facebook"
  providerSub: String,          // provider's stable user id
  email:       Option[String],
  displayName: Option[String],
  avatarUrl:   Option[String],
  createdAt:   Instant,
  lastSeenAt:  Instant
)
