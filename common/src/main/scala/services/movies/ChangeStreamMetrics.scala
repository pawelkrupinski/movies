package services.movies

/**
 * Observability sink for the `movies` change stream — what kinds of changes the
 * worker's shared cursor ([[ChangeStreamFanout]]) actually decodes and fans out
 * to the cache + read-model projector. Answers, live on the dashboard, the
 * questions this session had to answer by hand-tailing the stream:
 *
 *  - `recordEvent(op)` — one change event, by operation (insert|update|replace|
 *    delete). `rate()` is the change-stream volume the projector reprojects from.
 *  - `recordUpdateKind(kind)` — for UPDATE events, which field KIND changed:
 *    `source_data` (a cinema slot / scrape write), `rating` (a rating value or
 *    its url), `identity` (tmdb/imdb id + resolution lifecycle), or
 *    `updated_at_only` — a write that touched nothing but `updatedAt`. The last
 *    is a REDUNDANT-WRITE CANARY: after the empty-patch guard it should stay ~0,
 *    so a climbing rate flags a caller re-introducing no-op writes.
 *
 * The worker wires the Prometheus-backed [[services.metrics.WorkerTaskMetrics]];
 * the web and unit tests use [[ChangeStreamMetrics.noop]]. Mirrors
 * [[services.readmodel.ReadModelProjectionMetrics]].
 */
trait ChangeStreamMetrics {
  def recordEvent(op: String): Unit
  def recordUpdateKind(kind: String): Unit
}

object ChangeStreamMetrics {
  object Op {
    val Insert = "insert"; val Update = "update"; val Replace = "replace"; val Delete = "delete"; val Other = "other"
  }
  val Ops: Seq[String] = Seq(Op.Insert, Op.Update, Op.Replace, Op.Delete, Op.Other)

  object Kind {
    val SourceData = "source_data"; val Rating = "rating"; val Identity = "identity"
    val UpdatedAtOnly = "updated_at_only"; val Other = "other"
  }
  val Kinds: Seq[String] = Seq(Kind.SourceData, Kind.Rating, Kind.Identity, Kind.UpdatedAtOnly, Kind.Other)

  // Stored field names as `patchToUpdate` emits them (NOT the domain names).
  private val RatingFields   = Set("imdbRating", "metascore", "filmwebRating", "rottenTomatoes", "filmwebUrl", "metacriticUrl", "rottenTomatoesUrl")
  private val IdentityFields = Set("imdbId", "tmdbId", "searchTitle", "tmdbNoMatch", "detailPending")

  /** Collapse any mongo op string to the fixed label set, so an unexpected op
   *  (invalidate / drop / rename) doesn't spawn a new series. */
  def normalizeOp(raw: String): String = if (Ops.contains(raw)) raw else Op.Other

  /** Categorise an UPDATE event's changed `$set`/`$unset` field paths into kinds.
   *  `updatedAt` always bumps, so classify on the OTHER fields: none left ⇒
   *  `updated_at_only` (the no-op canary). A multi-field update maps to every kind
   *  it touched (e.g. a scrape that also settled `detailPending` ⇒ source_data +
   *  identity). An unrecognised field falls to `other`. */
  def updateKinds(updatedFieldKeys: Set[String]): Set[String] = {
    val nonMeta = updatedFieldKeys.filter(_ != "updatedAt")
    if (nonMeta.isEmpty) Set(Kind.UpdatedAtOnly)
    else {
      val topLevel = nonMeta.map(_.takeWhile(_ != '.'))
      val kinds = Set.newBuilder[String]
      if (topLevel.contains("sourceData")) kinds += Kind.SourceData
      if (topLevel.exists(RatingFields))   kinds += Kind.Rating
      if (topLevel.exists(IdentityFields)) kinds += Kind.Identity
      val result = kinds.result()
      if (result.isEmpty) Set(Kind.Other) else result
    }
  }

  val noop: ChangeStreamMetrics = new ChangeStreamMetrics {
    def recordEvent(op: String): Unit        = ()
    def recordUpdateKind(kind: String): Unit = ()
  }
}
