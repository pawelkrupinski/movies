package services.enrichment

import models.CacheKey

import models.MovieRecord
import services.evidence.LatestTitleYear
import services.titles.TitleNormalizer

/** How the OMDb id backfill asks OMDb for a row's film: original (production, English) title first — OMDb is an
 *  English DB — then the cinema display title, with the row's year and directors to corroborate a hit. */
final class OmdbSearch(omdb: OMDbClient, normalizer: TitleNormalizer)(using LatestTitleYear) {

  /** The film's IMDb id as OMDb gives it, or None. A sweep's ask (`inSweep`) is bulk: leftover budget only, paced; a
   *  single film's is one somebody is waiting on. A read that FAILED throws ([[OMDbClient.findImdbId]]). */
  def imdbIdOf(key: CacheKey, row: MovieRecord, inSweep: Boolean): Option[String] =
    omdb.findImdbId((row.originalTitle.toSeq :+ label(key, row)).distinct, key.year, row.director.toSet,
      if (inSweep) OmdbAsk.Bulk else OmdbAsk.UserWaiting)

  /** The film's display title. */
  def label(key: CacheKey, row: MovieRecord): String = row.displayTitle(key.cleanTitle, normalizer)
}
