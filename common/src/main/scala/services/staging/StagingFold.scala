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
    // A DIFFERENT title-group's fold can resolve to this SAME tmdbId and win the race to
    // create the film's `movies` row first — nothing serializes two `StagingFold` tasks
    // against each other (see `MongoStagingFolder`'s doc comment), so a decorated
    // spelling that concludes an identity another concurrent/prior fold already promoted
    // has its own insert collide with the partial UNIQUE index on `tmdbId` (prod PL,
    // 2026-09-08: 'Lalka' tmdbId 1321666, folded from a dozen decorated spellings —
    // several concluded the identity independently, and whichever committed second hit
    // `E11000 … index: tmdbId_1`). The winner's write is majority-committed by the time
    // the loser's fails, so a bare retry re-reads with it already visible, and THIS
    // attempt's own cluster then merges into it through `reconcileTmdbIds`'s sibling
    // lookup instead of re-inserting — the same "two writers, one wins, the other
    // reconciles" shape the transient-error retry above already handles, just tripped by
    // a write conflict on identity rather than the transaction machinery.
    //
    // A `key_1` collision gets the SAME treatment now, for a corrected reason. An
    // earlier version of this comment (round 2, a42086081) reasoned that "the same two
    // clusters conclude the same collision every time, so retrying it cannot help" — true
    // ONLY of a collision `resolveKeyCollisions` already sees INSIDE one `planGroup` call,
    // which it resolves deterministically before ever reaching Mongo. Round 4 (prod PL,
    // 2026-09-08, poland/convergence run 34285923158) showed a `key_1` E11000 actually
    // reaching Mongo dozens of times for five decorated 'Lalka' spellings — a DIFFERENT
    // situation `resolveKeyCollisions` structurally cannot see: each spelling is its own
    // fold group, loaded via its own sanitize prefix and its own tmdbId, and the OTHER
    // 'Lalka' film (a different tmdbId, a different sanitize prefix, already holding the
    // plain key from an earlier fold) is invisible to that load — the same two clusters
    // are NEVER even in the same call, so the old reasoning's premise never held for this
    // shape. `planGroupProbingContestedKeys` closes that blind spot (a caller probes for
    // the literal key a plan wants to write and folds any occupant it finds back into the
    // group before the write), so the common case now resolves without ever raising; a
    // `key_1` E11000 that still reaches here means the probe's OWN read still missed a
    // SIMULTANEOUS committer — retrying is exactly right there, the same "two writers, one
    // wins, the other's next read sees it" shape as the tmdbId case, just with the caller's
    // probe (not `reconcileTmdbIds`) as the sibling lookup that converges it. Message-
    // matched (not a bare `isDuplicateKey`) so this stays scoped to the two error shapes
    // this reasoning actually covers.
    case scala.util.Failure(e: com.mongodb.MongoWriteException)
      if services.MongoErrors.isDuplicateKey(e) && (tmdbIdCollision(e) || keyCollision(e)) && attempt < maxRetries =>
      Next.Retry(e)
    case scala.util.Failure(e) => Next.Abandon(e)
  }

  /** True when a duplicate-key write error names the `tmdbId` unique index specifically.
   *  The driver's `WriteError` carries no structured index name, only the server's text
   *  (`"... index: tmdbId_1 dup key: { tmdbId: 1321666 }"`), so this is matched on the
   *  message rather than a code alone — `code == 11000` alone would also swallow a
   *  `key_1` collision, matched separately by [[keyCollision]] (see the call site in
   *  [[nextAfterAttempt]]). */
  private def tmdbIdCollision(e: com.mongodb.MongoWriteException): Boolean =
    Option(e.getError).flatMap(err => Option(err.getMessage)).exists(_.contains("tmdbId"))

  /** True when a duplicate-key write error names the `key` unique index specifically —
   *  the other retryable shape, see [[nextAfterAttempt]]'s doc comment. */
  private def keyCollision(e: com.mongodb.MongoWriteException): Boolean =
    Option(e.getError).flatMap(err => Option(err.getMessage)).exists(_.contains("key_1"))

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
    /** A cluster this fold could not promote at all THIS round because its concluded
     *  key is already taken by an unrelated film AND it carries no stable identity
     *  (no tmdbId, no imdbId) of its own to disambiguate the stored key with — see
     *  [[resolveKeyCollisions]]. Purely informational (nothing here is written); a
     *  caller logs it so the deferral is loud rather than a silent no-op. The common
     *  case — both sides identified — is [[disambiguated]] instead: that cluster IS
     *  promoted, under a suffixed key. */
    deferred:       Seq[DeferredIdentityCollision] = Nil,
    /** A cluster whose concluded key collided with an unrelated, already-identified
     *  film's, and was promoted anyway under a key suffixed with its OWN tmdbId/
     *  imdbId — see [[resolveKeyCollisions]]. Purely informational; a caller logs it
     *  the same way as [[deferred]]. */
    disambiguated:  Seq[DisambiguatedIdentityCollision] = Nil
  ) {
    /** The surviving rows by key, for callers that hand them on to the cache. */
    def folded: Seq[(CacheKey, MovieRecord)] = moviesUpserts.map { case (_, k, r) => k -> r }
  }

  /** Two clusters that both concluded `key`, but whose own resolved identities
   *  (tmdbId/imdbId) disagree — `clusterByFilm` already refused to merge them, so
   *  `resolveKeyCollisions` keeps `kept`'s promotion and defers `deferred`'s rather
   *  than picking a display title/poster/synopsis for two different real films.
   *  Reached only when the losing cluster carries NEITHER a tmdbId nor an imdbId —
   *  nothing stable to disambiguate its stored key with — which is rare: by the time
   *  two clusters both conclude the identical key, `clusterByFilm` has almost always
   *  told them apart BY their tmdbIds, so [[DisambiguatedIdentityCollision]] is the
   *  common outcome and this one the fallback. */
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

  /** Two clusters that both concluded `key` — see [[DeferredIdentityCollision]] — but
   *  here the loser carries a stable tmdbId/imdbId of its own, so instead of being
   *  held back it is promoted straight away under `disambiguatedKey`: the SAME shared
   *  `normalized` prefix (so `CorpusIndex` still buckets it beside the film that kept
   *  the plain key, and every future listing of either film still reaches
   *  `ScrapeLanding.chooseConcluded`'s runtime/venue disambiguation), with a suffix
   *  derived from the loser's OWN tmdbId (or imdbId, when it has no tmdbId) — never
   *  its `FilmId`, which is minted from arrival order and is the exact non-determinism
   *  this collision-handling exists to remove (see `resolveKeyCollisions`). */
  case class DisambiguatedIdentityCollision(
    key: CacheKey, disambiguatedKey: CacheKey,
    keptTmdbId: Option[Int], keptImdbId: Option[String],
    disambiguatedTmdbId: Option[Int], disambiguatedImdbId: Option[String]
  )

  /** The WARN an operator sees for a [[DisambiguatedIdentityCollision]] — the
   *  promoted-anyway counterpart of [[deferredCollisionWarning]]. */
  def disambiguatedCollisionWarning(d: DisambiguatedIdentityCollision): String = {
    def identity(tmdbId: Option[Int], imdbId: Option[String]) =
      s"tmdbId=${tmdbId.map(_.toString).getOrElse("—")}/imdbId=${imdbId.getOrElse("—")}"
    s"Staging fold: two different films both concluded key '${StoredMovieRecord.keyFor(d.key)}' " +
    s"(kept ${identity(d.keptTmdbId, d.keptImdbId)} at the plain key; " +
    s"${identity(d.disambiguatedTmdbId, d.disambiguatedImdbId)} promoted instead under " +
    s"'${StoredMovieRecord.keyFor(d.disambiguatedKey)}')."
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
    // `r.cacheKey(normalizer)` — the row's KEY AS STORED — not a bare re-derivation
    // from `(r.title, r.year)`. The two agree for every ordinary row (a stored key's
    // prefix always sanitizes back to the display title), so this is a no-op there;
    // they DIVERGE for a row `resolveKeyCollisions` previously disambiguated, whose
    // stored key carries a suffix a bare re-derivation would silently drop. Dropping
    // it here would re-collide this row with its rival's in `byKey` below on every
    // SUBSEQUENT fold — unioning two different films' records, the exact corruption
    // this whole mechanism exists to prevent — instead of this cluster re-concluding
    // the same disambiguated key it already holds.
    val moviesByKey = moviesRows.map(r => r.cacheKey(normalizer) -> r.record)
    val moviesKeys  = moviesByKey.map(_._1).toSet
    // The id behind each existing key; two documents under one key (a legacy duplicate)
    // resolve to the lower id, deterministically, and the other retires into it.
    val idsByKey: Map[CacheKey, Seq[FilmId]] =
      moviesRows.groupBy(r => r.cacheKey(normalizer)).view.mapValues(_.map(_.id).sortBy(_.value)).toMap
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
    // upserts at the SAME key is unwritable — the second `replaceOne` hits the `key`
    // unique index as an E11000 (prod PL, 'Lalka', this incident) — but the two
    // clusters are NOT one film, so `resolveKeyCollisions` does not merge them (that
    // would show one film's screenings under the other's title/poster/tmdbId, worse
    // than the crash it replaces): it keeps ONE cluster's promotion at the plain key
    // and gives every other colliding cluster a DIFFERENT stored key instead, derived
    // from its own tmdbId/imdbId — see [[resolveKeyCollisions]].
    val (kept, deferred, disambiguated) = resolveKeyCollisions(plannedByCluster, fresh)
    val upserts       = kept.map(cp => (cp.filmId, cp.key, cp.record))
    val newPromotions = kept.collect { case cp if cp.isNewFilm => cp.key -> cp.record }
    val survivors     = upserts.map(_._1).toSet
    // A deferred cluster's OWN pre-existing `movies` row (if it had one — a legacy
    // duplicate this fold would otherwise have retired) is left exactly as it is:
    // not upserted (nothing here decided its content changed), not deleted either.
    // A DISAMBIGUATED cluster needs no such protection: it IS upserted (under its
    // suffixed key), so its own id is already in `survivors`.
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
    // or a human merging by hand). A DISAMBIGUATED cluster's staging rows are NOT held
    // back — it promoted this round — so only the true defers narrow `consumedStaging`.
    val deferredRawKeys = deferred.flatMap(_.loserOwnRawKeys).toSet
    val consumedStaging = stagingRows.filterNot(r => deferredRawKeys.contains(CacheKey(r.title, r.year, normalizer)))
    Plan(upserts, moviesDeletes, consumedStaging, newPromotions, retirements,
      deferred = deferred.map(_.collision), disambiguated = disambiguated)
  }

  /** [[planGroup]], but re-run once more against any row this group's OWN read missed
   *  because a same-key collision's OTHER side shares neither this group's sanitize
   *  prefix nor its tmdbId/imdbId — the two things a caller's `moviesRows` load is
   *  scoped by (`MongoStagingFolder.foldOnce`'s `groupRows`/`siblings` queries;
   *  `InMemoryStagingFolder`'s equivalent filters).
   *
   *  Two "Lalka" films (tmdbId 1321666, 1309396) share only their CONCLUDED spelling,
   *  never a sanitize prefix ("kinonaobcasachlalka" vs "lalka") or a tmdbId/imdbId — so
   *  a decorated-title fold group never loads the plain-key occupant
   *  [[resolveKeyCollisions]] would need to see, and either (a) writes a brand-new
   *  document straight onto an already-taken key, or worse (b) RE-KEYS an existing
   *  sibling it DID load (found by tmdbId) back onto the plain key, because
   *  `FilmCanonicalizer.canonical` recomputes a cluster's key from its cinema/TMDB
   *  votes with NO memory of a prior fold's disambiguation — only `resolveKeyCollisions`
   *  remembers that, and only for clusters present in the SAME call. Both shapes hit
   *  Mongo's `key_1` unique index (prod PL, 2026-09-08, poland/convergence run
   *  34285923158: five decorated 'Lalka' spellings, each its own fold group, each blind
   *  to the sibling 'Lalka' film a PRIOR fold had already disambiguated onto a suffixed
   *  key, each re-concluding the plain 'lalka|2026' — round 4 of the same incident
   *  `resolveKeyCollisions` (round 3, a784d5d68) did not by itself fix, because that
   *  fix's cross-cluster visibility only ever reached as far as one `planGroup` call's
   *  own `moviesRows`).
   *
   *  `probe` is the caller's own I/O: given the literal stored-key strings this group's
   *  first pass wants to write that no already-loaded row explains, look them up for
   *  real (`movies.key ∈ …`, scoped to the caller's own session/transaction so the read
   *  is consistent with the write that follows). Any hit is folded into `moviesRows` and
   *  the group re-planned — now WITH the contested key's occupant present,
   *  `resolveKeyCollisions` runs its usual deterministic tie-break and the common case
   *  (the occupant already committed) resolves to a disambiguated key with no error ever
   *  reaching Mongo. Only a genuinely SIMULTANEOUS pair — neither side's probe finds the
   *  other yet — can still race to the unique index; `nextAfterAttempt` retries a `key_1`
   *  failure for exactly this reason now, and the retry's OWN probe (run again, fresh)
   *  finds the now-committed winner. */
  def planGroupProbingContestedKeys(
    stagingRows: Seq[StagingRecord], moviesRows: Seq[StoredMovieRecord],
    normalizer: TitleNormalizer, extraCinemaTitles: Seq[String] = Nil,
    fresh: CacheKey => FilmId = FilmId.fresh(_, _ => false)
  )(probe: Set[String] => Seq[StoredMovieRecord]): Plan = {
    val tentative = planGroup(stagingRows, moviesRows, normalizer, extraCinemaTitles, fresh)
    val knownKeys = moviesRows.map(r => StoredMovieRecord.keyFor(r.cacheKey(normalizer))).toSet
    val uncovered = tentative.moviesUpserts.map { case (_, k, _) => StoredMovieRecord.keyFor(k) }.toSet -- knownKeys
    if (uncovered.isEmpty) tentative
    else {
      val knownIds   = moviesRows.map(_.id).toSet
      val discovered = probe(uncovered).filterNot(r => knownIds.contains(r.id))
      if (discovered.isEmpty) tentative
      // `extraCinemaTitles` is held fixed, not re-derived from `discovered`: a newly
      // found occupant is a DIFFERENT film (`clusterByFilm` already keeps it apart by
      // tmdbId/imdbId), so its cinema titles are not a vote on THIS cluster's spelling —
      // only `resolveKeyCollisions`'s tie-break needs to see it at all.
      else planGroup(stagingRows, moviesRows ++ discovered, normalizer, extraCinemaTitles, fresh)
    }
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

  /** One cluster that lost a key collision and carries no stable identity of its own
   *  (see [[DeferredIdentityCollision]]): what a caller needs to leave it alone (its
   *  own raw keys, so its staging rows stay unconsumed; its own existing ids, so they
   *  stay undeleted) and what it needs to log the deferral. */
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
   *  THE WINNER, deterministically: the cluster with the lower tmdbId (or, lacking
   *  one, the lower imdbId), never `isNewFilm`/`FilmId` — an id is minted from the key
   *  a row was FIRST created under, i.e. from ARRIVAL ORDER, and ranking on it is
   *  exactly the bug this replaces (poland/convergence run 34271323339: the same two
   *  clusters flipped which one "won" four times in one boot as more decorated
   *  siblings landed, because by the time BOTH already had their own pre-existing
   *  `movies` row the old `(isNewFilm, FilmId)` tie-break degraded to comparing
   *  `FilmId` strings alone). tmdbId/imdbId are real, externally-assigned identities,
   *  fixed regardless of when this corpus first saw the film, so this pick is the same
   *  whatever order the corpus replays in.
   *
   *  EVERY OTHER cluster at that key is PROMOTED TOO, not held back, provided it
   *  carries a tmdbId or an imdbId of its own: [[CacheKey.disambiguated]] suffixes its
   *  stored key with that identity — content-derived and just as order-independent as
   *  the winner pick — while `CorpusIndex` still buckets it under the shared plain
   *  title (`CacheKey.lookupBase`), so every future listing of either film keeps
   *  reaching `ScrapeLanding.chooseConcluded`'s runtime/venue disambiguation exactly as
   *  it does today. Only a loser with NEITHER a tmdbId NOR an imdbId — nothing stable
   *  to suffix with — falls back to the old behaviour: DEFERRED, not written, not
   *  merged, not retired, its staging rows left unconsumed so the next fold attempt
   *  re-detects (and re-defers) the same collision until an identity resolves or a
   *  human merges the two films by hand.
   *
   *  A no-op — one cluster per key, returned as-is — for every ordinary fold, which is
   *  all of them bar a same-title-same-year collision between two films the corpus
   *  otherwise correctly keeps apart.
   *
   *  `fresh` (the same one [[planGroup]] mints brand-new ids with) re-mints a
   *  disambiguated loser's id when it is itself brand new: every `ClusterPlan.filmId`
   *  was computed BEFORE this function ever saw the group, from `survivor(members,
   *  canonKey)` — and for two colliding brand-new clusters, `canonKey` is the SAME
   *  contested key for both, so both already hold the IDENTICAL freshly-minted id.
   *  Promoting the loser under that id, alongside the winner's, would upsert two
   *  documents under one `_id` — Mongo just silently keeps whichever write lands
   *  last — so a loser with no pre-existing row of its own gets a genuinely fresh id
   *  derived from ITS OWN disambiguated key instead. A loser that already had a
   *  `movies` row keeps that row's real id unconditionally: this is a RETITLE, not a
   *  new document. */
  private def resolveKeyCollisions(planned: Seq[ClusterPlan], fresh: CacheKey => FilmId): (Seq[ClusterPlan], Seq[DeferredCluster], Seq[DisambiguatedIdentityCollision]) = {
    // Lower tmdbId first (absent sorts last), then lower imdbId, both real identities
    // fixed independently of this corpus's arrival order. `isNewFilm`/`filmId.value`
    // only break a tie neither id can — which, since `clusterByFilm` already split
    // these clusters apart BY tmdbId/imdbId, means at least one of BOTH sides has
    // neither: nothing content-based is left to rank them by, so this residual
    // arrival-order tie-break is unavoidable there (and moot for the disambiguation
    // below, which needs no winner/loser order among rows with no identity at all).
    def rank(c: ClusterPlan) =
      (c.record.tmdbId.isEmpty, c.record.tmdbId.getOrElse(Int.MaxValue),
       c.record.imdbId.isEmpty, c.record.imdbId.getOrElse(""),
       c.isNewFilm, c.filmId.value)
    // The loser's OWN stable identity to suffix its stored key with — never a
    // `FilmId` (arrival-order-derived, see above). `None` only for a loser with
    // neither a tmdbId nor an imdbId, which falls back to deferring.
    def stableSuffix(c: ClusterPlan): Option[String] =
      c.record.tmdbId.map(id => s"tmdb$id").orElse(c.record.imdbId.map(id => s"imdb$id"))
    val byKey = planned.groupBy(_.key)
    val seen  = scala.collection.mutable.Set.empty[CacheKey]
    val kept          = Seq.newBuilder[ClusterPlan]
    val deferred      = Seq.newBuilder[DeferredCluster]
    val disambiguated = Seq.newBuilder[DisambiguatedIdentityCollision]
    planned.foreach { cp =>
      if (!seen(cp.key)) {
        seen += cp.key
        val colliding = byKey(cp.key)
        if (colliding.sizeIs == 1) kept += cp
        else {
          val ranked = colliding.sortBy(rank)
          val winner = ranked.head
          kept += winner
          ranked.tail.foreach { loser =>
            stableSuffix(loser) match {
              case Some(suffix) =>
                val disambiguatedKey = CacheKey.disambiguated(cp.key, suffix)
                // A brand-new loser's `filmId` was minted from the SAME contested
                // `cp.key` the winner's was — re-mint from its own disambiguated key
                // so the two promotions don't collide on `_id` (see the doc comment).
                // An existing row's id is its real, permanent identity: kept as-is.
                val filmId = if (loser.isNewFilm) fresh(disambiguatedKey) else loser.filmId
                kept += loser.copy(filmId = filmId, key = disambiguatedKey)
                disambiguated += DisambiguatedIdentityCollision(cp.key, disambiguatedKey,
                  winner.record.tmdbId, winner.record.imdbId, loser.record.tmdbId, loser.record.imdbId)
              case None =>
                deferred += DeferredCluster(loser.ownRawKeys, loser.existingIds,
                  DeferredIdentityCollision(cp.key, winner.record.tmdbId, winner.record.imdbId,
                    loser.record.tmdbId, loser.record.imdbId))
            }
          }
        }
      }
    }
    (kept.result(), deferred.result(), disambiguated.result())
  }
}
