package models

import java.time.Instant

// Multi-city note: `disabledCinemas` keys on globally-
// unique cinema display names ("Helios Posnania", "Cinema City Kinepolis"),
// so they never collide across cities — a future "Helios Wrocław" is a distinct
// Cinema with a distinct displayName. Cross-city keys are inert: a page only
// surfaces cinemas in its own city, so out-of-city entries are simply ignored.
// Hence user state needs no city dimension.
case class UserState(
  userId:          String,
  hiddenFilms:     Set[String],
  disabledCinemas: Set[String],
  updatedAt:       Instant
)
//
// `selectedMovies` and `favouriteRooms` lived here until the plan page was removed. Documents
// written before that still carry both, and this codec is derived straight from the case class —
// so the removal was gated on proving the decoder SKIPS an unknown field rather than throwing on
// it. `UserStateLegacyFieldsSpec` is that proof and stays: it decodes a /plan-era document, in two
// field orders, and asserts the retired names are never written back.

object UserState {
  def empty(userId: String, now: Instant = Instant.now()): UserState =
    UserState(userId, Set.empty, Set.empty, now)
}
