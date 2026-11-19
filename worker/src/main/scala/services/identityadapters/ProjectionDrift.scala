package services.identityadapters

import models.{LeanRecords, StoredMovieRecord}

import services.identity.*

import models.MovieRecord
import services.titles.TitleNormalizer

/** The drift a reconciling projection of the whole corpus finds ([[IdentityProjection]]): the films it wrote or retired
 *  that a scoped projection would have left wrong, counted and named. Logs under the projection's `logger`. */
final class ProjectionDrift(normalizer: TitleNormalizer, metrics: IdentityProjectionMetrics, logger: play.api.Logger) {

  /** A reconciling projection of the whole corpus, checked against the scope its changes alone gave it — closed over key
   *  collisions too, read off this projection's own keys, which the scope's drafts would have had — so a film it wrote
   *  or retired outside that scope is one a scoped projection would have left wrong: counted and named, each film written
   *  with what of it moved ([[ProjectionDrift.moved]]) — the input the scope did not see. */
  def apply(moved: ProjectionScope, freed: Set[String], index: ProjectionIndex, tick: ProjectionTick,
            stored: Seq[StoredMovieRecord]): Unit = tick.plan.foreach { plan =>
    def inside(scope: ProjectionScope, f: ProjectedFilm) = scope.films(f.id.value) || f.members.exists(scope.listings)
    @scala.annotation.tailrec
    def closed(scope: ProjectionScope): ProjectionScope =
      ProjectionScope.widened(index, scope, plan.films.filter(inside(scope, _)).map(_.key), freed, normalizer) match {
        case Some(wider) => closed(wider)
        case None        => scope
      }
    val scope  = closed(moved)
    val before = stored.iterator.filter(r => !scope.films(r.id.value)).map(r => r.id -> r).toMap
    // a film that moved its key alone names both keys: the key a projection derives is the drift to read then
    val missed = tick.changed.filterNot(inside(scope, _)).map(f => f.id.value -> before.get(f.id).fold("new")(s =>
      ProjectionDrift.moved(s.record, f.record) match {
        case "key" => s"key ${s.key(normalizer)} -> ${f.key}, titled '${s.title}' -> '${f.title}'"
        case what  => what
      })) ++ plan.retired.map(_.value).filterNot(scope.films).map(_ -> "retired")
    metrics.drifted(missed.size)
    if (missed.nonEmpty) logger.warn(s"identity projection drift: the whole projection changed ${missed.size} film(s) the scoped one " +
      s"would have left: ${missed.sortBy(_._1).take(20).map { case (id, what) => s"$id ($what)" }.mkString(", ")}")
  }
}

private[services] object ProjectionDrift {
  /** What of a film moved from `was` to `now`, briefly: its own fields (by name), and the sources whose slots differ
   *  ([[LeanRecords.slotsEqual]]), the first few named, else its key — what a drift line names each film it missed by. */
  def moved(was: MovieRecord, now: MovieRecord): String = {
    val fields  = differing(was, now, Set("data"))
    val sources = (was.data.keySet ++ now.data.keySet).toSeq.filterNot(s => (was.data.get(s), now.data.get(s)) match {
      case (Some(a), Some(b)) => LeanRecords.slotsEqual(a, b)
      case _                  => false
    }).map(_.toString).sorted
    val named   = fields ++ sources.take(3) ++ Option.when(sources.size > 3)(s"+${sources.size - 3} slots")
    if (named.isEmpty) "key" else named.mkString(" ")
  }

  /** The names of the fields `was` and `now` (one case class) differ in, `skipped` aside, in declaration order. */
  def differing(was: Product, now: Product, skipped: Set[String]): Seq[String] =
    was.productElementNames.zip(was.productIterator.zip(now.productIterator))
      .collect { case (name, (a, b)) if !skipped(name) && a != b => name }.toSeq
}
