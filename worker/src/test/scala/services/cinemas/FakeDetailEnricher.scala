package services.cinemas

import models.{Cinema, Source}
import services.cinemas.common.{DetailEnricher, FilmDetail}

/** Shared test double for a deferred-detail cinema. The enqueuer and reaper only
 *  read `cinema`/`detailGroup`; the handler also drives `fetchFilmDetail`, which
 *  returns the configured `detail` and counts calls. `target` / `uptimeOverride`
 *  exercise the chain case (detail written to a shared network source, health
 *  reported under one global name). */
class FakeDetailEnricher(
  val cinema:      Cinema,
  val detailGroup: String,
  detail:          Option[FilmDetail] = None,
  target:          Option[Source]     = None,
  uptimeOverride:  Option[String]     = None
) extends DetailEnricher {
  var calls = 0
  override def detailTarget: Source = target.getOrElse(cinema)
  override def enrichmentServiceOverride: Option[String] = uptimeOverride
  override def fetchFilmDetail(ref: String): Option[FilmDetail] = { calls += 1; detail }
}
