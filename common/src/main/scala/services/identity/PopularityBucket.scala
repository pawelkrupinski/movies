package services.identity

/**
 * TMDB's popularity as the resolver reads it: `floor(log2 p)`, the `popularity.log2` measure's
 * value. The raw number moves daily (54 of 60 sampled re-fetches of a UK film changed nothing
 * else); its bucket almost never does, so a film's stored record keeps the bucket and a re-fetch
 * that only moved popularity changes nothing the model reads.
 */
object PopularityBucket {
  /** The bucket of `popularity` — the measure's own formula, clamped below at 1e-3. */
  def of(popularity: Double): Int = math.floor(math.log(math.max(popularity, 1e-3)) / math.log(2)).toInt

  /** A popularity inside `bucket`, mid-way in log space, so [[of]] gives the bucket back exactly
   *  (never a floating-point edge at a power of two). */
  def representative(bucket: Int): Double = 1.5 * math.pow(2, bucket)
}
