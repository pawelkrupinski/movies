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
// `language`, unlike the sets above, is a single explicit pick or nothing —
// `None` until the user has chosen one server-side, whether by picking on a
// page while logged in or by the one-time login migration adopting this
// device's own explicit local pick (see `UserStateController`'s wire-format
// note). Both new fields are defaulted so every existing 4-arg call site
// (tests, mostly) keeps compiling.
case class UserState(
  userId:               String,
  hiddenFilms:          Set[String],             // legacy — see `UserStateController.get()`/`put()`
  disabledCinemas:      Set[String],              // legacy — cinema-hiding is device-local now
  updatedAt:            Instant,
  hiddenFilmsByCountry: Map[String, Set[String]] = Map.empty,
  language:             Option[String] = None
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

  /** The `updatedAt` a write stamps over a row last stamped `previous` — the
   *  row's version (`UserStateRepository.replaceIfUnchanged`) and the source of
   *  `Last-Modified` / `UserChangeTimeCache`, so it must move even when two
   *  writes land in the same millisecond (Mongo stores `updatedAt` as a
   *  millisecond BSON date): now, or one millisecond past `previous`, whichever
   *  is later. `MongoUserStateRepository.changeHiddenFilms` computes the same
   *  rule server-side, inside its atomic update. */
  def nextUpdatedAt(previous: Option[Instant], now: Instant = Instant.now()): Instant = {
    val tick = now.truncatedTo(java.time.temporal.ChronoUnit.MILLIS)
    previous.map(_.truncatedTo(java.time.temporal.ChronoUnit.MILLIS).plusMillis(1))
      .filter(_.isAfter(tick)).getOrElse(tick)
  }
}
