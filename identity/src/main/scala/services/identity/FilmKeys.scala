package services.identity

import models.{CacheKey, FilmId, StoredMovieRecord}

import services.evidence.LatestTitleYear
import services.titles.TitleNormalizer

/** Step 4 of THE IDENTITY PROJECTION's decisions ([[IdentityProjectionPlan]]), once the details of a new film are
 *  fetched: the display title and year pick each film's key, and a fresh film's id is minted. */
object FilmKeys {
  /** Choose every film's title, year and key, and mint the id of every fresh one. `taken` says
   *  whether an id is live already. A fresh id is neither live nor one the FilmId map holds: the map
   *  keeps a retired film's id for good, so a film drafted again under its first key — which mints
   *  the very id it had — would get an entry the store refuses, and its id back on the old counter. */
  def finish(draft: ProjectionDraft, normalizer: TitleNormalizer, taken: FilmId => Boolean)(using LatestTitleYear): ProjectionPlan = {
    val titled = draft.drafts.sortBy(_.counter).map { d =>
      val title = d.record.displayTitle(d.anchor, normalizer)
      (d, title, d.record.resolvedYear)
    }
    // Two films one title and year name: the older keeps the plain key, the other its own.
    val plainKey = titled.map { case (d, title, year) => d.counter -> StoredMovieRecord.keyFor(title, year, normalizer) }.toMap
    val firstHolder = titled.groupBy { case (d, _, _) => plainKey(d.counter) }.map { case (k, ds) => k -> ds.map(_._1.counter).min }
    val minted = scala.collection.mutable.Set.empty[FilmId]
    val films = titled.map { case (d, title, year) =>
      val key = if (firstHolder(plainKey(d.counter)) == d.counter) plainKey(d.counter)
                else s"${normalizer.sanitize(title)}~${d.counter}|${year.fold("")(_.toString)}"
      val id = d.inherited.getOrElse {
        val fresh = FilmId.fresh(CacheKey.stored(title, key), id => taken(id) || minted(id) || draft.counters.counterOf(id.value).isDefined)
        minted += fresh
        fresh
      }
      ProjectedFilm(id, d.counter, title, year, key, d.record, d.members, d.touched)
    }
    val freshEntries = films.filter(f => draft.counters.filmIdOf(f.counter).isEmpty).map(f => FilmIdCounter(f.id.value, f.counter))
    ProjectionPlan(films, draft.retired, draft.additions ++ freshEntries, draft.regroupings)
  }
}
