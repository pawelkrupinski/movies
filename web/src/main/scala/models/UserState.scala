package models

import java.time.Instant

// Multi-city note: `disabledCinemas` keys on globally-
// unique cinema display names ("Helios Posnania", "Cinema City Kinepolis"),
// so they never collide across cities — a future "Helios Wrocław" is a distinct
// Cinema with a distinct displayName. Cross-city keys are inert: a page only
// surfaces cinemas in its own city, so out-of-city entries are simply ignored.
// Hence user state needs no city dimension.
// `language`, unlike the two sets above, is a single explicit pick or
// nothing — `None` until the user has chosen one server-side, whether by
// picking on a page while logged in or by the one-time login migration
// adopting this device's own explicit local pick (see `UserStateController`'s
// wire-format note). Defaulted so every existing 4-arg call site (tests,
// mostly) keeps compiling.
case class UserState(
  userId:          String,
  hiddenFilms:     Set[String],
  disabledCinemas: Set[String],
  updatedAt:       Instant,
  language:        Option[String] = None
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
