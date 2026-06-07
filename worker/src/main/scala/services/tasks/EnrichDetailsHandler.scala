package services.tasks

import models.Source
import play.api.Logging
import services.cinemas.DetailEnricher
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCache}

/** Builds the dedup key, freshness key, and payload for an `EnrichDetails` task.
 *  The dedup key encodes `(detailGroup, film)` so "enrich film F for group G"
 *  can exist at most once — the queue's unique index rejects a second. */
object EnrichDetailsTasks {
  val GroupKey  = "group"
  val RefKey    = "ref"
  val TitleKey  = "title"
  val YearKey   = "year"

  def dedupKey(group: String, key: CacheKey): String =
    s"detail|$group|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  def payload(enricher: DetailEnricher, key: CacheKey, ref: String): Map[String, String] =
    Map(
      GroupKey -> enricher.detailGroup,
      RefKey   -> ref,
      TitleKey -> key.cleanTitle,
      YearKey  -> key.year.map(_.toString).getOrElse("")
    )
}

/**
 * Handles an `EnrichDetails` task: fetch one film's detail (once per
 * `(detailGroup, film)` per 6h) and merge it into that cinema's `SourceData`
 * slot, preserving the slot's showtimes.
 *
 * Freshness-gated: skips if the detail was fetched inside the window. A failed
 * fetch (`fetchFilmDetail` → None) is reported `Done` without marking fresh, so
 * the cinema's next scrape re-enqueues it rather than the worker spinning.
 */
class EnrichDetailsHandler(
  enrichersByGroup: Map[String, DetailEnricher],
  cache:            MovieCache,
  freshness:        FreshnessStore
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.EnrichDetails

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (freshness.isFresh(key, FreshnessKind.DetailEnrich)) return Skipped

    enrichersByGroup.get(task.payload.getOrElse(EnrichDetailsTasks.GroupKey, "")) match {
      case None =>
        logger.warn(s"No detail enricher for task $key; dropping.")
        Done
      case Some(enricher) =>
        enricher.fetchFilmDetail(task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")) match {
          case None => Done // failed/absent — not marked fresh, the next scrape re-enqueues
          case Some(detail) =>
            val title = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
            val year  = task.payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
            cache.putIfPresent(cache.keyOf(title, year), current =>
              current.data.get(enricher.cinema) match {
                case Some(slot) => current.copy(data = current.data + ((enricher.cinema: Source) -> detail.mergeInto(slot)))
                case None       => current // slot dropped between scrape and enrich — leave alone
              })
            freshness.markFresh(key, FreshnessKind.DetailEnrich)
            Done
        }
    }
  }
}
