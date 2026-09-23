package services.users

import models.UserState

/** The fields a legacy `PUT /api/me/state` body carries — each `None` when the
 *  body left it out, so the stored value stays. `language = Some(None)` is an
 *  explicit `null`: clear the pick. What `UserStateRepository.patchLegacyState`
 *  sets on the row in one atomic step, touching nothing else — so a hide in
 *  another tab landing at the same moment survives it.
 *
 *  Validation (known language, bounded sets) is the caller's
 *  (`UserStateController`); [[applyTo]] is only what the store does with it,
 *  run directly by the in-memory store and stated as an update pipeline by
 *  `MongoUserStateRepository` (`UserStateWritesContract` holds both to it). */
final case class LegacyStatePatch(
  hiddenFilms:     Option[Set[String]]    = None,
  disabledCinemas: Option[Set[String]]    = None,
  language:        Option[Option[String]] = None
) {
  def applyTo(base: UserState): UserState = base.copy(
    hiddenFilms     = hiddenFilms.getOrElse(base.hiddenFilms),
    disabledCinemas = disabledCinemas.getOrElse(base.disabledCinemas),
    language        = language.getOrElse(base.language)
  )
}
