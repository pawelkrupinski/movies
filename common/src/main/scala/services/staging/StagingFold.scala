package services.staging

import models.MovieRecord
import services.movies.{CacheKey, FilmCanonicalizer, MovieRecordMerge, StoredMovieRecord, TitleNormalizer, FilmId}

/**
 * The PURE decision half of folding a newcomer's staging rows into `movies`,
 * shared by every `StagingFolder` impl.
 *
 * It folds the WHOLE `sanitize(title)` GROUP — every year-variant at once — and
 * runs the SAME `clusterByFilm`/`canonical` collapse the cache's
 * `canonicalizeBySanitize` runs, so the folded `movies` state is already the
 * settled steady state: ±1-year variants merged into one row, each resolved
 * cluster re-keyed to its TMDB year, the canonical spelling chosen. This replaces
 * the old fold-per-year + periodic-settle split — Cinema City reports a film at
 * `zawodowcy|2025` while everyone else reports `zawodowcy|2026`, and both variants
 * now collapse HERE instead of landing as two `movies` rows for a separate settle
 * pass to merge later.
 */
object StagingFold {

  /** What [[MongoStagingFolder]] does after one transaction attempt. */
  sealed trait Next
  object Next {
    /** The body ran — commit, and report these new promotions. */
    case class Commit(newPromotions: Seq[(CacheKey, MovieRecord)]) extends Next
    /** A transient transaction error with retries left — abort and go round again. */
    case class Retry(cause: Throwable) extends Next
    /** Out of retries, or a failure retrying cannot help — abort and RAISE. */
    case class Abandon(cause: Throwable) extends Next
  }

  /** Decide what one transaction attempt's outcome means. Pure, and split out from the
   *  I/O for one reason: the distinction between `Commit(Seq.empty)` and `Abandon` is the
   *  whole bug.
   *
   *  A failed fold used to return `Seq.empty` — byte-identical to a clean fold that
   *  promoted nothing. `StagingFoldHandler` therefore marked the task Done while the
   *  staging rows were still sitting there, `StagingReaper` re-enqueued the same fold on
   *  its next tick, and `pending_movies` grew without bound. That is how the 2026-07-27
   *  `Missing field: sourceData` decode bug ran for hours behind nothing louder than a
   *  WARN: the failure had no channel to surface on. `StagingFoldHandler` documents a
   *  THROWN fold as the reschedule signal, so a failure has to reach the caller as one. */
  def nextAfterAttempt(
    outcome:    scala.util.Try[Seq[(CacheKey, MovieRecord)]],
    attempt:    Int,
    maxRetries: Int
  ): Next = outcome match {
    case scala.util.Success(newPromotions) => Next.Commit(newPromotions)
    case scala.util.Failure(e: com.mongodb.MongoException)
      if e.hasErrorLabel(com.mongodb.MongoException.TRANSIENT_TRANSACTION_ERROR_LABEL) && attempt < maxRetries =>
      Next.Retry(e)
    case scala.util.Failure(e) => Next.Abandon(e)
  }

  /** What to write to bring `movies` to its folded+settled state, and which
   *  staging rows were consumed. `moviesDeletes` are existing `movies` rows in
   *  the group whose key the collapse retired (re-keyed to a TMDB year, or
   *  merged into a sibling). `newPromotions` is the subset of `moviesUpserts`
   *  that is a BRAND-NEW film — a cluster no pre-existing `movies` row joined —
   *  so the folder can schedule its first-time rating enrichment; a fold that
   *  merely merges into an existing `movies` row is not listed (that row
   *  already carries its ratings). */
  case class Plan(
    /** Each surviving film: the id it is stored under, the key it now answers to, and
     *  the merged record. A yearless newcomer whose year TMDB concluded keeps its id and
     *  changes key — a RETITLE, not a new document (see [[FilmId]]). */
    moviesUpserts:  Seq[(FilmId, CacheKey, MovieRecord)],
    moviesDeletes:  Seq[FilmId],
    /** The staging rows this plan actually consumes. NOT necessarily all of the
     *  group's rows — a cluster deferred by [[deferred]] keeps its own staging
     *  rows unconsumed, so the NEXT fold attempt sees them again. */
    stagingDeletes: Seq[StagingRecord],
    newPromotions:  Seq[(CacheKey, MovieRecord)],
    /** Each retired `movies` id paired with the id it folded INTO — the same rows as
     *  `moviesDeletes`, but attributed, so a caller can migrate the loser's side-collection
     *  rows onto the winner instead of orphaning them. Attribution has to happen here
     *  because only `planGroup` knows which cluster a loser belonged to; a group that
     *  produces several surviving rows has several different winners. */
    retirements:    Seq[(FilmId, FilmId)] = Nil,
    /** A cluster this fold refused to promote THIS round because its concluded key
     *  is already taken by an unrelated film — see [[resolveKeyCollisions]]. Purely
     *  informational (nothing here is written); a caller logs it so the deferral is
     *  loud rather than a silent no-op. */
    deferred:       Seq[DeferredIdentityCollision] = Nil
  ) {
    /** The surviving rows by key, for callers that hand them on to the cache. */
    def folded: Seq[(CacheKey, MovieRecord)] = moviesUpserts.map { case (_, k, r) => k -> r }
  }

  /** Two clusters that both concluded `key`, but whose own resolved identities
   *  (tmdbId/imdbId) disagree — `clusterByFilm` already refused to merge them, so
   *  `resolveKeyCollisions` keeps `kept`'s promotion and defers `deferred`'s rather
   *  than picking a display title/poster/synopsis for two different real films. */
  case class DeferredIdentityCollision(
    key: CacheKey,
    keptTmdbId: Option[Int], keptImdbId: Option[String],
    deferredTmdbId: Option[Int], deferredImdbId: Option[String]
  )

  /** The WARN an operator sees for a [[DeferredIdentityCollision]] — one place so
   *  every `StagingFolder` impl reports the same shape, the way `"Folded staging
   *  group '…'"` already is (duplicated per impl, matched on message text). */
  def deferredCollisionWarning(d: DeferredIdentityCollision): String = {
    def identity(tmdbId: Option[Int], imdbId: Option[String]) =
      s"tmdbId=${tmdbId.map(_.toString).getOrElse("—")}/imdbId=${imdbId.getOrElse("—")}"
    s"Staging fold: two different films both concluded key '${StoredMovieRecord.keyFor(d.key)}' " +
    s"(kept ${identity(d.keptTmdbId, d.keptImdbId)}; deferred ${identity(d.deferredTmdbId, d.deferredImdbId)}) " +
    "— the deferred film's cinemas stay in staging until the ambiguity resolves on its own."
  }

  /** The TMDB ids carried by a group's rows. A folder loads existing `movies` rows
   *  with these ids under ANY title and feeds them to `planGroup` too, so a
   *  cross-LANGUAGE duplicate already in `movies` under its other-language title
   *  (same id — the Polish "Gwiezdne wojny: Mandalorian i Grogu" already promoted
   *  when the English "The Mandalorian and Grogu" folds) collapses onto one row
   *  HERE, at fold time, not only on the next periodic settle. `groupByFilm`'s
   *  bare-title tmdbId edge does the actual cross-title merge. */
  def reconcileTmdbIds(stagingRows: Seq[StagingRecord], moviesRows: Seq[StoredMovieRecord]): Set[Int] =
    (stagingRows.flatMap(_.record.tmdbId) ++ moviesRows.flatMap(_.record.tmdbId)).toSet

  /** The staging rows that belong to ONE fold group: every loaded row whose
   *  (re-derived) display title sanitizes to the same key as `cleanTitle`. THE
   *  one selection both `StagingFolder` impls share, so the real (Mongo) and fake
   *  (in-memory) folders can't disagree about which rows a fold consumes.
   *
   *  Keyed on `sanitize(r.title)` — NOT the sanitized middle segment baked into
   *  the row's `_id` at creation. The two DRIFT: `StagingRecord.fromStorage`
   *  re-derives `title` via `chooseDisplay` on every read, and a decoration strip
   *  can re-expose a trailing numeral that `sanitize` then romanizes (the prod
   *  "Toy Story 5- dubbing" → display "Toy Story 5" → `toystoryv`, vs the `_id`
   *  middle `toystory5`). Matching on the `_id` middle (what `MongoStagingFolder`
   *  used to do) then selected nothing, so the row never folded and the reaper
   *  re-enqueued the fold forever (the 30-min staging "fold" gauge rectangle). The
   *  reaper groups + the gauge count on this SAME `sanitize(r.title)` key, so the
   *  fold now consumes exactly the rows the reaper classified as ready. Deletes
   *  still go through each row's persisted `id` (drift-proof), per StagingRecord. */
  def selectStagingGroup(rows: Seq[StagingRecord], cleanTitle: String, normalizer: TitleNormalizer): Seq[StagingRecord] = {
    val key = normalizer.sanitize(cleanTitle)
    rows.filter(r => normalizer.sanitize(r.title) == key)
  }

  /** `stagingRows` are every per-cinema row of ONE `sanitize(title)` group (all its
   *  year-variants); `moviesRows` are the existing `movies` rows in that group PLUS
   *  any cross-title same-tmdbId siblings the folder pulled in (see
   *  [[reconcileTmdbIds]]). `groupByFilm` then collapses within AND across titles by
   *  shared tmdbId — the same partition the cache `canonicalizeBySanitize` settle
   *  runs over the whole corpus, applied here to the fold's neighbourhood. */
  def planGroup(stagingRows: Seq[StagingRecord], moviesRows: Seq[StoredMovieRecord],
                normalizer: TitleNormalizer, extraCinemaTitles: Seq[String] = Nil,
                // The id for a BRAND-NEW film. A caller with a store checks the candidate is
                // not a live id there (`FilmId.fresh`'s `taken`); the pure default cannot.
                fresh: CacheKey => FilmId = FilmId.fresh(_, _ => false)): Plan = {
    // Union the per-cinema staging rows to ONE row per (sanitize, year) key FIRST,
    // restoring the one-row-per-key invariant `clusterByFilm` assumes. Without it,
    // N separate YEARLESS cinema rows would each become a rule-4 singleton cluster
    // that then collapses onto the same `(sanitize, None)` key and clobbers all but
    // one — dropping every cinema's slot but one for the all-yearless events
    // (Maraton Horrorów, Filmowe Poranki).
    val stagingByKey = stagingRows.groupBy(r => CacheKey(r.title, r.year, normalizer)).toSeq.map {
      case (key, rows) => key -> MovieRecordMerge.unionAll(rows.map(_.record))
    }
    val moviesByKey = moviesRows.map(r => CacheKey(r.title, r.year, normalizer) -> r.record)
    val moviesKeys  = moviesByKey.map(_._1).toSet
    // The id behind each existing key; two documents under one key (a legacy duplicate)
    // resolve to the lower id, deterministically, and the other retires into it.
    val idsByKey: Map[CacheKey, Seq[FilmId]] =
      moviesRows.groupBy(r => CacheKey(r.title, r.year, normalizer)).view.mapValues(_.map(_.id).sortBy(_.value)).toMap
    // Union ACROSS the staging↔movies boundary too: `CacheKey` is case-insensitive,
    // so a staging "iron maiden" row and an already-promoted movies "Iron Maiden"
    // row share a key but sit in SEPARATE entries above. Left un-unioned they each
    // become a rule-4 singleton cluster that re-collapses onto the one
    // (sanitize, None) key and clobbers a cinema's slot — the same loss the
    // all-staging union guards against, across the boundary (a yearless event whose
    // first cinema promoted before the rest arrived). Merge by key, canonicalRank
    // order so the union base is deterministic.
    val byKey = (stagingByKey ++ moviesByKey).groupBy(_._1).toSeq.map { case (_, entries) =>
      val sorted = entries.sortBy { case (k, _) => FilmCanonicalizer.canonicalRank(k) }
      sorted.head._1 -> MovieRecordMerge.unionAll(sorted.map(_._2))
    }
    val plannedByCluster: Seq[ClusterPlan] =
      FilmCanonicalizer.groupByFilm(byKey, normalizer).flatMap(FilmCanonicalizer.clusterByFilm(_, normalizer)).map { cluster =>
        val (canonKey, merged) = FilmCanonicalizer.canonical(cluster, normalizer, extraCinemaTitles)
        // A cluster is a brand-new promotion iff no existing `movies` row joined it
        // (all members came from staging) — a merge into an existing row, or a
        // re-key of one, does NOT count: that row already owns its ratings.
        val isNewFilm = !cluster.exists { case (k, _) => moviesKeys.contains(k) }
        // Drop the staging-only `searchTitle`: a `movies` row queries external
        // services off its canonical title, so it never carries the (order-pinned)
        // staging search title — movies stay a deterministic function of the corpus.
        // The existing `movies` rows this cluster folds INTO `canonKey`. Recorded per
        // cluster because that is the only place the loser→winner pairing is known.
        // The surviving id: the row already stored under the canonical key; else the
        // best-ranked existing member's — its year or spelling changed, the film did not;
        // else this is a brand-new film. Every other existing member retires into it.
        val members  = cluster.map(_._1).distinct.filter(moviesKeys.contains)
          .flatMap(k => idsByKey(k).map(k -> _))
        val winnerId = FilmCanonicalizer.survivor(members, canonKey).getOrElse(fresh(canonKey))
        val retired  = members.map(_._2).filterNot(_ == winnerId).distinct
        ClusterPlan(winnerId, canonKey, merged.copy(searchTitle = None), isNewFilm, retired.map(_ -> winnerId),
          ownRawKeys = cluster.map(_._1).distinct, existingIds = members.map(_._2).distinct)
      }
    // `clusterByFilm` correctly keeps two distinct-tmdbId clusters apart — that is
    // its whole job, and the case where it should NOT (a shared imdbId) is already a
    // union-find edge above it. But `canonKey` is computed AFTER that split, by each
    // cluster's OWN `canonical()` vote, and nothing before this line has visibility
    // across clusters — so two rows the corpus genuinely treats as different films
    // (Poland, 2026-09-08: 'Lalka' tmdbId 1321666 beside an unrelated tmdbId 1309396,
    // both bare-titled "Lalka", both TMDB year 2026 — a common one-word title with an
    // ambiguous search match, not a merge candidate) can still both conclude the
    // identical `(sanitize, year)` key. `movies` has exactly one row per key, so two
    // upserts at it is unwritable — the second `replaceOne` hits the `key` unique
    // index as an E11000 (prod PL, 'Lalka', this incident) — but the two clusters are
    // NOT one film, so `resolveKeyCollisions` does not merge them (that would show one
    // film's screenings under the other's title/poster/tmdbId, which is worse than the
    // crash it replaces): it keeps the winner untouched and DEFERS the loser instead —
    // see [[resolveKeyCollisions]].
    val (kept, deferred) = resolveKeyCollisions(plannedByCluster)
    val upserts       = kept.map(cp => (cp.filmId, cp.key, cp.record))
    val newPromotions = kept.collect { case cp if cp.isNewFilm => cp.key -> cp.record }
    val survivors     = upserts.map(_._1).toSet
    // A deferred cluster's OWN pre-existing `movies` row (if it had one — a legacy
    // duplicate this fold would otherwise have retired) is left exactly as it is:
    // not upserted (nothing here decided its content changed), not deleted either.
    val protectedIds  = deferred.flatMap(_.loserExistingIds).toSet
    val moviesDeletes = moviesRows.map(_.id).distinct.filterNot(id => survivors.contains(id) || protectedIds.contains(id))
    // Derived from `moviesDeletes`, never widening it: the deletes stay exactly what they
    // were, and this only says where each one's cinemas should go. A deferred cluster's
    // own internal retirements are dropped, not applied — nothing about it moves this round.
    val retiredSet    = moviesDeletes.toSet
    val retirements   = kept.flatMap(_.retirements).filter { case (loser, _) => retiredSet.contains(loser) }.distinct
    // A deferred cluster's staging rows stay UNCONSUMED, so the next fold attempt sees
    // the same rows and re-detects (and re-defers) the same collision — cheaply, and
    // without ever raising, so nothing retries under backoff or reschedules; it simply
    // waits for the ambiguity to resolve on its own (one side re-resolving its tmdbId,
    // or a human merging by hand).
    val deferredRawKeys = deferred.flatMap(_.loserOwnRawKeys).toSet
    val consumedStaging = stagingRows.filterNot(r => deferredRawKeys.contains(CacheKey(r.title, r.year, normalizer)))
    Plan(upserts, moviesDeletes, consumedStaging, newPromotions, retirements,
      deferred = deferred.map(_.collision))
  }

  /** One cluster's plan before cross-cluster collision resolution: the id/key/record
   *  it would upsert, whether that is a brand-new promotion, the retirements ITS OWN
   *  legacy duplicates would need, and enough of its own shape (`ownRawKeys`,
   *  `existingIds`) for [[resolveKeyCollisions]] to leave it alone untouched when it
   *  loses a collision instead of guessing what to do with it. */
  private case class ClusterPlan(
    filmId: FilmId, key: CacheKey, record: MovieRecord, isNewFilm: Boolean,
    retirements: Seq[(FilmId, FilmId)],
    ownRawKeys: Seq[CacheKey], existingIds: Seq[FilmId]
  )

  /** One cluster that lost a key collision: what a caller needs to leave it alone
   *  (its own raw keys, so its staging rows stay unconsumed; its own existing ids, so
   *  they stay undeleted) and what it needs to log the deferral. */
  private case class DeferredCluster(loserOwnRawKeys: Seq[CacheKey], loserExistingIds: Seq[FilmId], collision: DeferredIdentityCollision)

  /** Resolve every key two or more clusters concluded down to ONE promoted cluster —
   *  `movies` can hold only one row per key — WITHOUT merging clusters `clusterByFilm`
   *  kept apart because their resolved identities (tmdbId/imdbId) disagree: that would
   *  attribute one film's cinemas/screenings to the other's title, poster, synopsis and
   *  tmdbId, a silent wrong-movie-shown bug worse than the crash it would replace (see
   *  the call site). Two clusters colliding here are never a genuine duplicate of ONE
   *  film — `idsByKey` in [[planGroup]] already resolves that case (two literal
   *  `movies` documents at one key) inside a single cluster, before this ever runs.
   *
   *  The winner is an EXISTING row over a freshly-minted one, and the lower id on a
   *  tie — deterministic, order-independent. Every other cluster at that key is
   *  DEFERRED: not written, not merged into the winner, not retired — simply left out
   *  of this round, exactly as if its staging rows had not concluded yet. The next
   *  fold attempt re-runs this same decision from the same (unconsumed) staging rows,
   *  so a later tick where one side's identity resolves differently — or a human
   *  merges the two films by hand — is what actually converges it, not a guess made
   *  here. A no-op — one cluster per key, returned as-is — for every ordinary fold,
   *  which is all of them bar a same-title-same-year collision between two films the
   *  corpus otherwise correctly keeps apart. */
  private def resolveKeyCollisions(planned: Seq[ClusterPlan]): (Seq[ClusterPlan], Seq[DeferredCluster]) = {
    val byKey = planned.groupBy(_.key)
    val seen  = scala.collection.mutable.Set.empty[CacheKey]
    val kept     = Seq.newBuilder[ClusterPlan]
    val deferred = Seq.newBuilder[DeferredCluster]
    planned.foreach { cp =>
      if (!seen(cp.key)) {
        seen += cp.key
        val colliding = byKey(cp.key)
        if (colliding.sizeIs == 1) kept += cp
        else {
          val ranked = colliding.sortBy(c => (c.isNewFilm, c.filmId.value))
          val winner = ranked.head
          kept += winner
          ranked.tail.foreach { loser =>
            deferred += DeferredCluster(loser.ownRawKeys, loser.existingIds,
              DeferredIdentityCollision(cp.key, winner.record.tmdbId, winner.record.imdbId,
                loser.record.tmdbId, loser.record.imdbId))
          }
        }
      }
    }
    (kept.result(), deferred.result())
  }
}
