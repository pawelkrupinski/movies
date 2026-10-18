package services.movies

/**
 * Normalised `(title, year)` lookup key. Diacritics stripped, lowercased,
 * whitespace squashed — so "Drzewo Magii" and "drzewo magii" don't end up as
 * two cache entries. Equality uses the normalised form; `cleanTitle` retains
 * the original casing for display in snapshots.
 */
private[services] case class CacheKey private (cleanTitle: String, year: Option[Int], normalized: String) {
  override def hashCode(): Int = (normalized, year).hashCode()
  override def equals(other: Any): Boolean = other match {
    case k: CacheKey => k.normalized == normalized && k.year == year
    case _           => false
  }

  /** The same title key at another year. Re-derive nothing from `cleanTitle`: on a
   *  key read from the store it is the row's DISPLAY title, which need not sanitize to
   *  the stored key's title ("La luz que imaginamos" labelling `la luz|2026`). */
  def atYear(other: Option[Int]): CacheKey = new CacheKey(cleanTitle, other, normalized)
}

private[services] object CacheKey {
  /** Build a key under ONE country's rules. `normalized` is computed here and
   *  carried, rather than derived in the constructor body as it used to be:
   *  identity depends on the rule set, so a key that normalises itself silently
   *  adopts whatever set happened to be global on the constructing thread. That
   *  is the mechanism that made a shared `TitleNormalizer` impossible to scope —
   *  keys are built on Mongo change-stream driver threads and inside
   *  `BoundedParallel`'s executor, neither of which any caller-side scope
   *  reaches. Taking the normalizer as a context parameter puts the choice back
   *  where the country is known, and leaves existing call sites unchanged
   *  wherever a `given` is already in scope. */
  def apply(cleanTitle: String, year: Option[Int], normalizer: TitleNormalizer): CacheKey =
    new CacheKey(cleanTitle, year, normalizer.sanitize(cleanTitle))

  /** The key a stored row answers to: its `key` field AS STORED (`normalized|year`), with
   *  the display title as its label. The stored key is the lookup identity; re-deriving
   *  it from the display title — which is a vote over the row's slots and moves as
   *  they do — used to make the cache and the store disagree about which key a row
   *  held (the drifted-id orphans of old). */
  def stored(cleanTitle: String, storedKey: String): CacheKey = {
    val sep = storedKey.lastIndexOf('|')
    val (normalized, year) =
      if (sep >= 0) (storedKey.substring(0, sep), storedKey.substring(sep + 1).toIntOption) else (storedKey, None)
    new CacheKey(cleanTitle, year, normalized)
  }
}
