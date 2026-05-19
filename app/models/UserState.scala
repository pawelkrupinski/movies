package models

import java.time.Instant

/**
 * Per-user personalization state — the server-side mirror of the
 * anonymous-user localStorage keys (`favouriteMovies`,
 * `favouriteScreenings`, `hiddenFilms`, `disabledCinemas`). Logged-in
 * users replace localStorage with this server-canonical copy; logged-
 * out users keep using localStorage and see the "this won't sync"
 * nag toast.
 *
 * Sets, not Lists, because every consumer (favourites toggle, filter
 * panel) treats them as set-membership questions and order doesn't
 * matter. The wire codec normalizes to a stable order at write time
 * so a no-op write doesn't bump `updatedAt`.
 */
case class UserState(
  userId:              String,
  favouriteMovies:     Set[String],
  favouriteScreenings: Set[String],
  hiddenFilms:         Set[String],
  disabledCinemas:     Set[String],
  updatedAt:           Instant
)

object UserState {
  def empty(userId: String, now: Instant = Instant.now()): UserState =
    UserState(userId, Set.empty, Set.empty, Set.empty, Set.empty, now)
}
