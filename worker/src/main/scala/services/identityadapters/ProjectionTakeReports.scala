package services.identityadapters

import models.ListingKey
import models.{FilmId, StoredMovieRecord}
import services.identity.*
import services.identity.measures.Resolution
/** How a written identity projection's take changes are told ([[IdentityProjection]]): to the metrics (the changes, and
 *  those that undid the listing's last change, `reversals`), the change feed, the log (a line each, then a summary) and the
 *  take history. Logs under the projection's `logger`. */
final class ProjectionTakeReports(metrics: IdentityProjectionMetrics, changeFeed: Seq[TakeChange] => Unit,
                                                    takeLines: TakeChangeLines, takeHistory: TakeHistory, reversals: TakeReversals,
                                                    logger: play.api.Logger) {

  /** The projection's CHANGE LIST ([[LiveProjectionIndex.written]]: each listing whose stored film the writes moved to
   *  another TMDB film, or placed on one for the first time — what the site shows for it, read off the store as written,
   *  so a film the store declined changes no take until a later projection writes it), each with the basis of the
   *  decision it replaced and of the one that holds it now, and why it moved ([[TakeCauses]]; `fresh`: the index was built
   *  afresh for this projection, so it knows no listing as re-published). Handed whole to the metrics and to `changeFeed`; its match
   *  changes ([[TakeChange.moved]]) logged one line each (the first [[IdentityProjection.TakeChangesLogged]]) for
   *  drill-down, and summed up in one line by kind and basis; the lines and their fields are [[TakeChangeLines]]'. The
   *  country, build and projection id are every worker log line's own fields. */
  def apply(changed: Seq[TakeChange], resolution: Resolution, stored: Seq[StoredMovieRecord], written: Seq[ProjectedFilm],
            live: LiveProjectionIndex, trigger: String, ownTakes: Boolean, fresh: Boolean): Unit = {
    reported(changed, resolution, stored, written, live, trigger, ownTakes, fresh)
    unplaced(live.unplaced, resolution, stored, live, trigger)
  }

  /** Each listing still published that the writes left on no film ([[LiveProjectionIndex.unplaced]]): no take change, so
   *  logged a line of its own, naming the listing and the film it left ([[TakeChangeLines.unplaced]]). */
  private def unplaced(unplaced: Seq[TakeChange.Unplaced], resolution: Resolution, stored: Seq[StoredMovieRecord],
                       live: LiveProjectionIndex, trigger: String): Unit =
    if (unplaced.nonEmpty) {
      val decisions = decisionsOf(resolution, unplaced.iterator.map(_.listing).toSet)
      val titles    = stored.iterator.map(r => r.id.value -> r.title).toMap
      unplaced.take(IdentityProjection.TakeChangesLogged).foreach(u =>
        log(takeLines.unplaced(u, titles.get(u.film), decisions.get(u.listing), live.listing(u.listing), Some(trigger))))
    }

  private def reported(changed: Seq[TakeChange], resolution: Resolution, stored: Seq[StoredMovieRecord], written: Seq[ProjectedFilm],
                       live: LiveProjectionIndex, trigger: String, ownTakes: Boolean, fresh: Boolean): Unit =
    if (changed.nonEmpty) {
      val decisions = decisionsOf(resolution, changed.iterator.map(_.listing).toSet)
      lazy val before = stored.iterator.map(r => r.id -> r).toMap
      val reading   = TakeCauses.Reading(id => before.get(FilmId(id)), k => live.listing(k).isDefined, live.publishedAt,
        Option.unless(fresh)(live.lastRepublished), k => decisions.get(k).fold(Seq.empty[ListingKey])(_.members), trigger, ownTakes)
      val changes   = changed.map { t =>
        val prior = live.priorTake(t.listing)
        val take  = t.copy(basisBefore = prior.map(_.basis), basisAfter = decisions.get(t.listing).map(_.basis), prior = prior,
          published = live.listing(t.listing), formerly = live.formerly(t.listing), trigger = Some(trigger))
        take.copy(cause = TakeCauses.of(take, reading))
      }
      metrics.takesChanged(changes)
      changeFeed(changes)
      val takes     = changes.filter(_.moved)
      metrics.takesReversed(reversals.note(takes, decisions.get))
      if (takes.nonEmpty) {
        val moved  = takes.iterator.map(_.listing).toSet
        val now    = written.iterator.flatMap(f => f.members.iterator.filter(moved).map(_ -> f)).toMap
        val lines  = takes.map { t =>
          (t, takeLines.of(t, t.previousFilm.flatMap(id => before.get(FilmId(id))).map(_.title),
            now.get(t.listing).map(f => f.id.value -> f.title), decisions.get(t.listing), live.listing(t.listing),
            live.priorTake(t.listing), Some(trigger)), decisions.get(t.listing))
        }
        val logged = lines.take(IdentityProjection.TakeChangesLogged)
        logged.foreach { case (_, line, _) => log(line) }
        log(takeLines.summary(takes, logged.size))
        takeHistory.kept(lines, live.listing)
      }
    }

  /** The decision naming each of `wanted`. */
  private def decisionsOf(resolution: Resolution, wanted: Set[ListingKey]): Map[ListingKey, services.identity.measures.ResolverDecision] =
    resolution.decisions.iterator.flatMap(d => d.members.iterator.filter(wanted).map(_ -> d)).toMap

  private def log(line: TakeChangeLines.Line): Unit = tools.LogFields.info(logger.underlyingLogger, line.message, line.fields)
}
