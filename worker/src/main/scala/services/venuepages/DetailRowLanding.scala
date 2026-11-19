package services.venuepages

import models.{CacheKey, SourceData}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.{MovieCache, ScreeningTokens}

/** What a read detail page changes on the film row an `EnrichDetails` task was asked for: the fields the page states
 *  differently from what the rows last took ([[LandedPages]]), merged into the enricher's target slot(s)
 *  ([[DetailTargets]]) — the row-writing half of [[EnrichDetailsHandler]], which decides when a page is read. */
class DetailRowLanding(
  cache:           MovieCache,
  // What the rows took of each page, a read measured against it.
  landed:          LandedPages,
  // The country's badge vocabulary, for the detail-page `format` merged below.
  screeningTokens: ScreeningTokens
) {

  /** What venue_pages held of `ref`'s page before a read replaces it: the last read and what the rows took of it. */
  def before(enricher: DetailEnricher, ref: String): LandedPages.Before = landed.before(enricher, ref)

  /** Merge a fetched page into its target slot(s) on the row the task (`payload`) was asked for. */
  def land(payload: Map[String, String], enricher: DetailEnricher, ref: String, before: LandedPages.Before,
           read: FilmDetail): Unit = {
    val detail = landed.asSlotHolds(read)
    val title  = payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
    val year   = payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
    // The row the task was asked for, by its stored key; a task queued before the key rode
    // along (one deploy's worth) falls back to the title and year.
    val rowKey  = payload.get(EnrichDetailsTasks.RowKey).fold(cache.keyOf(title, year))(CacheKey.stored(title, _))
    val targets = DetailTargets.of(enricher, cache.get(rowKey), cache.normalizer, title, ref)
    val changed = landed.authoritative(before, detail)
    // Merge into the target slot(s), creating one if absent: a chain's network
    // source has no slot from a listing scrape, so it must be added here;
    // a 1:1 cinema's slot already exists, so this preserves its showtimes.
    // Only those slots are written, each from what it held (`putSlotsIfPresent`): a no-op on a row that was
    // re-keyed between enqueue and pickup — and then the page is not landed, so its next read still overrules.
    // Nor is it landed when no slot names the page (`targets` empty): the row is there, but nothing took the
    // read, and recording it as taken would make the change look landed once a slot names the page again.
    val prior = cache.get(rowKey)
    val onRow = targets.nonEmpty && cache.putSlotsIfPresent(rowKey, targets)((_, held) =>
      detail.mergeInto(changed.refreshInto(held.getOrElse(SourceData()), screeningTokens), screeningTokens))
    // A detail page brings facts the rating sites search by (a director, an original title: MergeRetrigger), so what it
    // changed is asked again, as a merge's is -- Verify scrape recording #26's "Afrykanska Przygoda" was asked of Filmweb
    // before Cinema City's network page brought "Ben Stassen", missed, and only the next day's pass asked again.
    if (onRow) for (before <- prior; after <- cache.get(rowKey)) cache.retriggerAfterEnrichment(rowKey, before, after)
    // Landed against what the rows took alone.
    if (onRow) landed.taken(before, read)
  }
}
