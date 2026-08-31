package services.movies

import models.MovieRecord

/**
 * Names the fields that actually differ between two [[MovieRecord]]s.
 *
 * The arrival-order determinism specs compare WHOLE records, but used to report
 * three hand-picked fields (`resolvedYear`, `readyToProject`, the cinema set).
 * A divergence anywhere else therefore printed three identical-looking pairs and
 * named nothing — which is exactly what a real 2026-08-31 failure did, leaving
 * "9 field diffs" that all read `Some(2023)/Some(2023) true/true Set()/Set()`.
 *
 * Walking the case class instead means whatever moved is what gets reported, so
 * the next occurrence is diagnosable from its log alone. Cheap because it only
 * ever runs on the failure path.
 */
object MovieRecordDiff {

  /** `field 0=<left> <otherIndex>=<right>` for every field that differs, where
   *  `otherIndex` is the replay iteration being compared against the first. */
  def describe(a: MovieRecord, b: MovieRecord, otherIndex: Int): String = {
    val differing = a.productElementNames.toSeq
      .zip(a.productIterator.zip(b.productIterator).toSeq)
      .collect { case (name, (x, y)) if x != y => s"$name 0=${render(x)} $otherIndex=${render(y)}" }
    if (differing.isEmpty)
      "records compare unequal yet every declared field matches — the difference is outside the constructor"
    else differing.mkString(", ")
  }

  /** Whole collections bury the one field that moved, so summarise the big ones.
   *  Deliberately no type match on the cinema key: a wrong `case _: Cinema` would
   *  match nothing and silently print less, which is the failure this replaces. */
  private def render(value: Any): String = value match {
    case m: Map[_, _] => s"Map[${m.size}](${truncate(m.keys.map(String.valueOf).toSeq.sorted.mkString(", "))})"
    case i: Iterable[_] if i.size > 6 => s"${i.getClass.getSimpleName}[${i.size}]"
    case other => truncate(String.valueOf(other))
  }

  private def truncate(s: String): String = if (s.length <= 200) s else s.take(200) + "…"
}
