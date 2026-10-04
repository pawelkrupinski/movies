package services.movies

import models.{MovieRecord, SourceData}

/**
 * [[ShowtimesDigest.leanEqual]] stopping at the slot it is handed twice. The identity projection writes the lean slots it
 * keeps and the cache keeps them as handed (`MovieCache.forCacheOver`), so a film it writes again at the same venues
 * holds, at every venue that did not move, the very object the cache does — a film at thousands of venues compared slot
 * by slot every time (every comparison building two cast sets) is compared at the few that moved.
 */
object LeanRecords {
  def equal(a: MovieRecord, b: MovieRecord): Boolean =
    (a eq b) || (a.copy(data = Map.empty) == b.copy(data = Map.empty) && a.data.keySet == b.data.keySet &&
      a.data.forall { case (source, slot) => b.data.get(source).exists(slotsEqual(slot, _)) })

  def slotsEqual(a: SourceData, b: SourceData): Boolean = (a eq b) || ShowtimesDigest.slotLeanEqual(a, b)
}
