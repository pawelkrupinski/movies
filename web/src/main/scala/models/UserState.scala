package models

import java.time.Instant

// Multi-city note: `disabledCinemas` keys on globally-
// unique cinema display names ("Helios Posnania", "Cinema City Kinepolis"),
// so they never collide across cities — a future "Helios Wrocław" is a distinct
// Cinema with a distinct displayName. Cross-city keys are inert: a page only
// surfaces cinemas in its own city, so out-of-city entries are simply ignored.
// Hence user state needs no city dimension. `hiddenFilms` is the opposite case:
// unlike a cinema name, a film TITLE is not globally unique across countries —
// two unrelated films in two countries can share a title — so the legacy,
// single global `hiddenFilms` set is being retired in favour of
// `hiddenFilmsByCountry`, keyed by `Country.code` ("pl", "us", …).
case class UserState(
  userId:               String,
  hiddenFilms:          Set[String],             // legacy — see `UserStateController.get()`/`put()`
  disabledCinemas:      Set[String],              // legacy — cinema-hiding is device-local now
  updatedAt:            Instant,
  hiddenFilmsByCountry: Map[String, Set[String]] = Map.empty
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
