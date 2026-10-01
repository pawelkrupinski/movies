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
 *    so a climbing rate flags a caller re-introducing no-op writes. A slot change
 *    no longer reaches this stream at all — the slots live in `movie_slots`, which
 *    has its own cursor and its own counter ([[SideCollectionChangeMetrics]]).
 *  - `recordCoalescedChange()` — one movies-doc event that rode an apply already
 *    queued for its film (by an earlier movies event, or by the screenings/
 *    movie_slots cursors — all three now share one pending set) instead of buying
 *    its own. `dropCinemaSlots` is the common source: a dropped venue writes
 *    `retainedSynopses` to `movies` in the SAME tick it deletes that venue's
 *    `screenings`/`movie_slots` rows, so one logical event used to cost the film
 *    TWO re-projections (one bought by this cursor, one by the coalesced side
 *    burst) instead of one. `coalesced / (coalesced + readmodel_project_calls)`
 *    is the share this cursor now folds into an already-queued apply.
 *
 * The worker wires the Prometheus-backed [[services.metrics.WorkerTaskMetrics]];
 * the web and unit tests use [[ChangeStreamMetrics.noop]]. Mirrors
 * [[services.readmodel.ReadModelProjectionMetrics]].
 */
trait ChangeStreamMetrics {
  def recordEvent(op: String): Unit
  def recordUpdateKind(kind: String): Unit
  def recordCoalescedChange(): Unit
  /** One film's apply: from the changed venues alone, or by re-reading the whole film, and why
   *  ([[ChangeStreamMetrics.Apply]]). */
  def recordApply(path: String, reason: String): Unit = ()
  /** A listener declined a film's venues alone, and why ([[ChangeStreamMetrics.VenueDecline]]). */
  def recordVenueDecline(reason: String): Unit = ()
}

object ChangeStreamMetrics {
  /** Why a listener declined a film's venues alone — `ReadModelProjector.onVenueSlots` and
   *  `MovieCache.applyVenueSlots`. Each is a whole-film re-read the venue path could not spare. */
  object VenueDecline {
    val ProjectorRowUnprojected = "projector_row_unprojected"
    val ProjectorTitleGroup     = "projector_title_group"
    val ProjectorTwoSlots       = "projector_two_slots"
    val ProjectorVenueAppears   = "projector_venue_appears"
    val ProjectorVenueVanishes  = "projector_venue_vanishes"
    val ProjectorUnionedVenue   = "projector_unioned_venue"
    val ProjectorCardHeld       = "projector_card_held"
    val ProjectorCardUnpublished = "projector_card_unpublished"
    val CacheNotResident        = "cache_not_resident"
    val CacheSlotsDiffer        = "cache_slots_differ"
    val All: Seq[String] = Seq(ProjectorRowUnprojected, ProjectorTitleGroup, ProjectorTwoSlots, ProjectorVenueAppears,
      ProjectorVenueVanishes, ProjectorUnionedVenue, ProjectorCardHeld, ProjectorCardUnpublished,
      CacheNotResident, CacheSlotsDiffer, ChangeStreamFanout.NoPartHandler, ChangeStreamFanout.PartFailed)
  }

  /** How a film's apply went — `MovieChangeStream.applyVenues`. A `venues` apply read only the
   *  changed venues' rows; a `film` apply re-read the whole film, for the `reason` given. */
  object Apply {
    val Venues = "venues"; val Film = "film"
    object Reason {
      /** Read from the changed venues alone. */
      val Applied        = "applied"
      /** A `movies` or `movie_slots` change rode the burst (or a showtime change at an unknown venue). */
      val NotShowtimes   = "not_showtimes"
      /** The burst touched more venues than one apply reads alone. */
      val TooManyVenues  = "too_many_venues"
      /** The film is failing a re-read, whose retry must read it whole. */
      val Failing        = "failing"
      /** The venues' rows could not be read alone (a failed read, or a row the venue read cannot place). */
      val VenueReadFailed = "venue_read_failed"
      /** A listener could not take the venues alone (see `ReadModelProjector.onVenueSlots`). */
      val Declined       = "declined"
      /** The store cannot read venues alone. */
      val Unsupported    = "unsupported"
      /** A listener was still not ready for the venues when the wait for it ran out. */
      val WaitExpired    = "wait_expired"
    }
    /** Every (path, reason) pair, for pre-registering the series. */
    val Series: Seq[(String, String)] = Seq(Venues -> Reason.Applied) ++
      Seq(Reason.NotShowtimes, Reason.TooManyVenues, Reason.Failing, Reason.VenueReadFailed, Reason.Declined, Reason.Unsupported,
        Reason.WaitExpired).map(Film -> _)
  }

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
  private val IdentityFields = Set("key", "imdbId", "tmdbId", "tmdbBasis", "wikidataId", "searchTitle", "tmdbAttempt", "tmdbNoMatch", "detailPending")

  /** Collapse any mongo op string to the fixed label set, so an unexpected op
   *  (invalidate / drop / rename) doesn't spawn a new series. */
  def normalizeOp(raw: String): String = if (Ops.contains(raw)) raw else Op.Other

  /** Categorise an UPDATE from BOTH its `$set` (`updatedFields`) and `$unset`
   *  (`removedFields`) paths. A removed cinema slot (`$unset sourceData.X`, e.g. a
   *  cinema stopped showing the film) lands `updatedAt` in `updatedFields` and the
   *  slot in `removedFields` — a REAL source_data change, not a no-op — so both
   *  must be considered or a prune reads as `updated_at_only`. */
  def updateKinds(updatedFieldKeys: Set[String], removedFieldKeys: Set[String]): Set[String] =
    updateKinds(updatedFieldKeys ++ removedFieldKeys)

  /** Categorise an UPDATE event's changed field paths into kinds.
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
      // `retainedSynopses` is slot CONTENT kept after a slot was pruned — the same kind
      // of change as a `sourceData` write, and it reaches the wire the same way now that
      // `MovieRecordPatch` carries it.
      if (topLevel.contains("sourceData") || topLevel.contains("retainedSynopses")) kinds += Kind.SourceData
      if (topLevel.exists(RatingFields))   kinds += Kind.Rating
      if (topLevel.exists(IdentityFields)) kinds += Kind.Identity
      val result = kinds.result()
      if (result.isEmpty) Set(Kind.Other) else result
    }
  }

  val noop: ChangeStreamMetrics = new ChangeStreamMetrics {
    def recordEvent(op: String): Unit        = ()
    def recordUpdateKind(kind: String): Unit = ()
    def recordCoalescedChange(): Unit        = ()
  }
}
