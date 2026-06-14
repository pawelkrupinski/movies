package services.tasks

import models.Source
import services.movies.TitleNormalizer

/**
 * Dedup keys + payload shapes for the staging-incubation tasks the
 * [[services.staging.StagingReaper]] enqueues and the staging handlers consume.
 *
 * Every key is scoped to the film's `anchor` = `sanitize(title)` (the same
 * per-film key the staging pipeline groups on), so the queue's per-`dedupKey`
 * idempotency collapses N spellings / year-variants / repeat enqueues of one
 * film's step to a single task. Detail is additionally scoped per cinema (one
 * fetch per venue), fold per year-variant (each lands its own `movies` row).
 *
 * Payloads carry the raw `title` (so a handler can re-read the film's rows and
 * resolve against a real title), plus the cinema (detail) / year (fold) the step
 * needs. The anchor is always derived from the title, never stored separately,
 * so the two can't drift.
 */
object StagingTaskKeys {
  val TitleKey  = "title"
  val CinemaKey = "cinema"

  private def anchor(title: String): String = TitleNormalizer.sanitize(title)

  /** The `(film, cinema)` key, taking the already-sanitized `anchor` directly.
   *  Doubles as the FreshnessStore key for "this cinema's staging detail was
   *  fetched" — a successful fetch marks it fresh, and readiness reads it, so a
   *  detail that carries no synopsis/cast/director still counts as done (the
   *  signal is "fetched", not "has content"). */
  def detailKey(anchor: String, cinemaDisplayName: String): String = s"staging-detail|$anchor|$cinemaDisplayName"

  def detailDedup(title: String, cinemaDisplayName: String): String = detailKey(anchor(title), cinemaDisplayName)
  def resolveTmdbDedup(title: String): String = s"staging-tmdb|${anchor(title)}"

  /** The per-film IMDb-recovery key, taking the already-sanitized `anchor`.
   *  Doubles as the FreshnessStore key for "IMDb recovery was attempted for this
   *  film" — recovery is best-effort, so once attempted (found OR not) the film
   *  folds; without this marker a no-match would re-enqueue the step forever. */
  def imdbKey(anchor: String): String = s"staging-imdb|$anchor"
  def resolveImdbDedup(title: String): String = imdbKey(anchor(title))

  // Fold is GROUP-scoped (the whole sanitize group, settled together via
  // `foldGroup`) and idempotent — one task per film keyed on the anchor, no
  // per-year fan-out.
  def foldDedup(title: String): String = s"staging-fold|${anchor(title)}"

  def detailPayload(title: String, cinemaDisplayName: String): Map[String, String] =
    Map(TitleKey -> title, CinemaKey -> cinemaDisplayName)
  def titlePayload(title: String): Map[String, String] = Map(TitleKey -> title)

  def titleOf(payload: Map[String, String]): String = payload.getOrElse(TitleKey, "")
  def anchorOf(payload: Map[String, String]): String = anchor(titleOf(payload))
  def cinemaOf(payload: Map[String, String]): Option[Source] =
    payload.get(CinemaKey).flatMap(Source.byDisplayName.get)
}
