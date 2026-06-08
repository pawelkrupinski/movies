package services.cinemas

import models.Cinema

/** Shared test double for a deferred-detail cinema. The enqueuer and reaper only
 *  read `cinema`/`detailGroup`; the handler also drives `fetchFilmDetail`, which
 *  returns the configured `detail` and counts calls. */
class FakeDetailEnricher(
  val cinema:      Cinema,
  val detailGroup: String,
  detail:          Option[FilmDetail] = None
) extends DetailEnricher {
  var calls = 0
  override def fetchFilmDetail(ref: String): Option[FilmDetail] = { calls += 1; detail }
}
