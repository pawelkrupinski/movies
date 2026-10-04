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

  /** [[equal]] of two records known alike at every source but `at`: their own fields, and their slots at `at`. */
  def equalAt(a: MovieRecord, b: MovieRecord, at: Set[models.Source]): Boolean =
    (a eq b) || (a.copy(data = Map.empty) == b.copy(data = Map.empty) && at.forall { source =>
      (a.data.get(source), b.data.get(source)) match {
        case (Some(x), Some(y)) => slotsEqual(x, y)
        case (x, y)             => x.isEmpty && y.isEmpty
      }
    })

  /** `r` with only its slots at `at`: what a write of `r` over a record alike at every other source touches. */
  def only(r: MovieRecord, at: Set[models.Source]): MovieRecord =
    r.copy(data = at.iterator.flatMap(s => r.data.get(s).map(s -> _)).toMap)
}
