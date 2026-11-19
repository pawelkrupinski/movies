package controllers

import models.City

/** Each city listing's JSON-LD ([[StructuredData.cityPage]]), kept under what it is built from (origin, city and the
 *  films it names): rebuilding its JSON tree for every film was ~1.3 MB a New York render, and it changes only when
 *  the city's films do. */
final class CityStructuredData {
  private val held: com.github.benmanes.caffeine.cache.Cache[StructuredData.CityPageKey, String] =
    tools.BoundedCache.ofSize(512).build[StructuredData.CityPageKey, String]()

  /** The JSON-LD of `city`'s listing of `films` at `pageUrl`. */
  def of(pageUrl: String, city: City, films: Seq[FilmSchedule]): String =
    held.get(StructuredData.CityPageKey(StructuredData.originOf(pageUrl, city), city, StructuredData.cityEntries(films)),
      StructuredData.cityPage)

  /** How many listings' JSON-LD this is holding — for the spec. */
  private[controllers] def size: Long = { held.cleanUp(); held.estimatedSize() }
}
