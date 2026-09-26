package services.movies

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import play.api.Logging
import services.events.{CinemaMovieAdded, EventBus, StagingNewcomerDiverted}
import services.resolution.YearWindow

/**
 * The corpus surface the scrape-time landing reads and writes through — the MINIMUM
 * of `CaffeineMovieCache` that [[ScrapeLanding]] needs, so the landing depends on a
 * seam and not on the cache. Every member is one the cache already exposes to
 * `services.*`; the trait only names which of them the landing is entitled to.
 *
 * Reads: the resident row at a key (`get`), the corpus's derived views
 * (`corpusIndex`, read-only), the country's `normalizer` and `keyOf`, and the
 * cache-or-store read that says whether it SUCCEEDED (`storedChecked`). Writes go
 * through the cache's own funnels — `put` (the tmdbId identity gate), `putIfPresent`
 * (the `$set`-diff for a resident row) and its one-slot form `putSlotIfPresent`, `rekey`
 * (a retitle) — under `withTitleLock`, the per-title lock the cache shares with its
 * settle paths. `residentCount` and `rehydrate` serve the cold-mirror guard;
 * `skippedUnreadable` is the cache's own counter of writes it declined, which the
 * landing increments for the same reason.
 */
private[movies] trait LandingStore {
  def normalizer: TitleNormalizer
  private[movies] def corpusIndex: CorpusIndexReader
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey
  private[services] def get(key: CacheKey): Option[MovieRecord]
  /** How many rows are resident — zero is the cold mirror the first scrape guards. */
  private[services] def residentCount: Long
  private[services] def storedChecked(key: CacheKey): (Option[MovieRecord], Boolean)
  private[services] def put(key: CacheKey, e: MovieRecord): WriteOutcome
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean
  /** `putIfPresent` of a write that sets ONE cinema slot, in work independent of how many
   *  other slots the row carries — see `CaffeineMovieCache.putSlotIfPresent`. */
  private[services] def putSlotIfPresent(key: CacheKey, source: Source, slot: SourceData): Boolean
  private[services] def rekey(oldKey: CacheKey, newKey: CacheKey, update: MovieRecord => MovieRecord, reason: RekeyReason): Unit
  private[services] def withTitleLock[A](cleanTitle: String)(body: => A): A
  def rehydrate(): Int
  private[services] def skippedUnreadable: java.util.concurrent.atomic.AtomicLong
}

/**
 * Scrape-time landing: where one cinema's fresh listing lands in the corpus, and
 * what its arrival displaces — `MovieCache.recordCinemaScrape`, as a class of its own.
 *
 * The cache's three responsibilities are the resident corpus with its write-through
 * and identity gate (`CaffeineMovieCache`), the periodic settle that self-heals it
 * (`canonicalizeBySanitize` and its kin), and THIS: the per-listing decision that
 * puts a screening in the right place the first time, so the settle has nothing to
 * fold (`docs/stable-film-id.md`). It asks the six landing questions through
 * [[ListingLanding]], walks the settle's own year window for a concluded row
 * (`concludedKeyFor`), redirects a spelling variant onto the row that knows it
 * (`redirectToExistingVariant`, the one retitle a scrape may cause), builds the
 * venue's slot, writes it under the per-title lock, drops the same slot from any
 * other row, prunes what the venue stopped listing, diverts a genuine newcomer to
 * staging, and announces the first-time arrivals on the bus.
 *
 * Everything it touches in the cache goes through [[LandingStore]]; the cache
 * constructs one of these and delegates `recordCinemaScrape` to it, so no caller
 * knows the seam exists.
 */
private[movies] final class ScrapeLanding(
  store:      LandingStore,
  repository: MovieRepository,
  // A genuinely-NEW film (one whose `sanitize(title)` group isn't already in
  // `movies`) is diverted to this staging sink — one row per `cinema|title|year`
  // — to incubate until TMDB concludes, instead of landing in the merged
  // `movies` cache; known films keep the direct path. `None` disables diversion —
  // every scrape lands in `movies`.
  staging:    Option[services.staging.StagingRepository],
  bus:        EventBus,
  // The country's badge vocabulary — one token in it, the voice-over version, is
  // the country's own to spell (`LEK` in Poland, `LEC` in the English-speaking
  // deployments).
  screeningTokens: ScreeningTokens,
  // The deployment's language, used to canonicalise cinema-reported production
  // countries into the deployment's own (see `CountryNames.canonical`).
  enrichmentLanguage: java.util.Locale,
  // How many consecutive thin ticks EITHER guard holds before accepting a
  // degraded listing — see `ScrapeHealth.maxRejectionsFor` for why this can't stay
  // one constant everywhere: the constant's wall-clock hold scales with this
  // deployment's own scrape cadence, from a 3h hold in Poland to a 42h one in the
  // US. Defaults to the OLD flat constant so every existing single-country
  // construction (including tests) is unchanged; `CaffeineMovieCache` wires the
  // cadence-aware value. Shared between the depth and breadth guards (renamed
  // from `maxConsecutiveDepthRejections` 2026-09-13 when the breadth guard grew
  // the same grace) — both express the same "how many ticks of a looks-degraded
  // fetch does this venue's own cadence buy before it stops being a guess".
  maxConsecutiveGuardRejections: Int = ScrapeHealth.MaxConsecutiveDepthRejections,
  // Guard-verdict + silent-write-skip counters — see `ScrapeLandingMetrics` for why
  // these exist. `CaffeineMovieCache` wires the Prometheus-backed instance; every
  // other construction (tests included) gets the noop.
  metrics: ScrapeLandingMetrics = ScrapeLandingMetrics.noop,
  // Where the guards keep their per-venue state between ticks — see
  // `ScrapeGuardLedger` for why it must outlive the process. In-memory by default,
  // so every construction that doesn't care (tests included) is unchanged;
  // `CaffeineMovieCache` forwards the worker's durable one.
  guardLedger: ScrapeGuardLedger = new InMemoryScrapeGuardLedger,
  // "Now" for the depth guard, which measures only showtimes still ahead of the
  // venue's own city clock — see `upcomingShowtimes`.
  clock: java.time.Clock = java.time.Clock.systemUTC(),
  // Where fresh slot strings are interned — see `CinemaSlotBuilder`. The worker hands every
  // country's cache the process's one pool; a lone construction gets its own.
  stringPool: StringPool = new StringPool
) extends Logging {

  import store.{corpusIndex, normalizer}

  // Fires the cold-mirror sync at most once, on the FIRST scrape (see
  // `recordCinemaScrape`). One-shot so the sync can't re-trigger at an
  // arrival-order-dependent later scrape — the mirror only goes cold at boot, and a
  // later mirror-empty-while-repo-non-empty state is reachable only in a harness
  // with no change stream (its mirror lags the repo), where re-checking would make
  // the staging path order-dependent (StagingOrderDeterminismSpec).
  private val coldMirrorSyncArmed = new java.util.concurrent.atomic.AtomicBoolean(true)
  // While the sync's corpus read keeps FAILING: when it may be tried again, and the wait
  // after that. The read is the whole stitched corpus — the costliest read the worker makes —
  // and every venue's scrape asked for it again while Mongo was not answering.
  private var coldMirrorRetry: Option[(java.time.Instant, scala.concurrent.duration.FiniteDuration)] = None
  private val coldMirrorLock  = new AnyRef

  /** How a scraped row becomes this venue's slot — shared with the identity projection. */
  private val cinemaSlots = new CinemaSlotBuilder(enrichmentLanguage, stringPool)

  /** Write the venue's guard state only when it changed — a healthy tick of an
   *  unchanged venue, the overwhelmingly common case, costs the store nothing. */
  private def persistGuardState(cinema: Cinema, before: ScrapeGuardState, after: ScrapeGuardState): Unit =
    if (after != before) guardLedger.put(cinema, after)

  /** The slot key for one cinema's report of a film under a given shown title.
   *  Every cinema slot is keyed by `(cinema, sanitize(title))` so a venue can hold
   *  several title-variant slots of one film (the original + a dubbed/decorated
   *  edition) without collision — the read-model split renders a card each, and
   *  the same (cinema, title) ALWAYS produces the same key so a re-scrape / fold
   *  merges in place instead of duplicating. */
  private def cinemaSlotKey(cinema: Cinema, title: String): Source =
    CinemaShowing.keyFor(cinema, title, normalizer)

  /** The cold-mirror sync (see its call in [[recordCinemaScrape]]): false when the tick must be
   *  discarded because the corpus could not be read. The whole check-read-rehydrate runs under
   *  `coldMirrorLock`, re-checking the latch inside it: two venues' first scrapes landing together
   *  both saw it armed, and both read the corpus and rehydrated — once per racer, not once. */
  private def syncColdMirror(cinema: Cinema): Boolean = coldMirrorLock.synchronized {
    if (!coldMirrorSyncArmed.get()) true
    else if (store.residentCount != 0) { coldMirrorSyncArmed.set(false); true }
    else {
      val now = clock.instant()
      if (coldMirrorRetry.exists { case (at, _) => now.isBefore(at) }) false   // backing off: discarded unread
      else {
        val (corpus, complete) = repository.findAllChecked()
        if (!complete) {
          val next = coldMirrorRetry.fold(ScrapeLanding.ColdMirrorRetryMin)(_._2 * 2).min(ScrapeLanding.ColdMirrorRetryMax)
          coldMirrorRetry = Some(now.plusMillis(next.toMillis) -> next)
          logger.warn(s"${cinema.displayName}: scrape discarded — the movies mirror is cold and the corpus " +
            s"could not be read to warm it; scrapes are discarded unread for ${next.toSeconds}s, then the sync is retried.")
          false
        } else {
          coldMirrorRetry = None
          coldMirrorSyncArmed.set(false)
          if (corpus.nonEmpty) store.rehydrate()
          true
        }
      }
    }
  }

  /** Apply one cinema's fresh scrape to the cache: for every CinemaMovie in
   *  `movies`, find-or-create the matching record and replace that cinema's
   *  slot in `data`. After processing, prune the cinema's slot
   *  from any record that previously held it but didn't appear this tick.
   *
   *  Variant redirect: when the cinema-reported `(title, year)` doesn't have
   *  a row at its primary key, but exactly one existing row already knows
   *  this raw title (via `cinemaTitles`), we write to THAT row's key instead
   *  of creating a fresh one. Returns one `(CinemaMovie, CacheKey, isNew)`
   *  triple per input movie:
   *
   *    - `CacheKey` is the *canonical* key the slot actually landed on
   *      (post-redirect). `CinemaScrapeRunner` publishes `MovieDetailsComplete`
   *      against this so two cinemas reporting different `year` values for
   *      the same film land on a single TMDB-stage event, no phantom row.
   *    - `isNew` is true when the `(cinema, raw title, raw year)` tuple is
   *      landing on this row for the first time; false on repeat ticks.
   *      `CinemaScrapeRunner` skips the bus publish for `isNew == false` so
   *      already-enriched rows don't churn event dispatches every scrape.
   *
   *  Doesn't touch enrichment-side fields (imdbId, ratings, URLs, …) — the
   *  TMDB / IMDb / MC / RT / Filmweb stages own those and run independently.
   *  Records whose cinema-side slots become empty are pruned daily by
   *  `UnscreenedCleanup`; a film that returns after the prune ran will
   *  re-pay the full enrichment cost on its next scrape. */
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie],
                         listingIsComplete: Boolean = true,
                         sourceKey: Option[String] = None,
                         viaFallback: Boolean = false): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, proxy 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return Seq.empty

    // The guards' state for this venue — two independent counters, since a venue can
    // be thin on showtimes (depth) while its film/slot count still looks plausible
    // (breadth), or vice versa (Kino Aurum, 2026-09-13), so one guard's grace must
    // never reset the other's; plus the source its stored listing came from. A tick
    // from a DIFFERENT source is a rewire, and both guards stand aside for it: they
    // measure against the old source's rows, which say nothing about the new one.
    // A venue with no recorded key is judged by where its stored rows' film links
    // point instead (see `ScrapeHealth.isRewire`) — only then are the sites computed.
    //
    // A FALLBACK-served listing is none of this (see `MovieCache.recordCinemaScrape`): not a
    // rewire, not judged by either guard, never a prune, and it leaves the guards' state —
    // the primary's baseline — exactly as it found it.
    // An UNREADABLE ledger is judged as Fresh — conservative, no rewire inferred — but its
    // state is never written back: that would replace the stored count with this tick's.
    val ledgerRead = guardLedger.get(cinema)
    val guardState = ledgerRead.getOrElse(ScrapeGuardState.Fresh)
    def saveGuardState(cinema: Cinema, before: ScrapeGuardState, after: ScrapeGuardState): Unit =
      if (ledgerRead.isDefined) persistGuardState(cinema, before, after)
    def sites(urls: Iterator[Option[String]]): Set[String] = urls.flatten.flatMap(ScrapeHealth.siteOf).toSet
    lazy val storedSites = sites(corpusIndex.slotsOf(cinema).iterator.map(_._3.filmUrl))
    val rewired = !viaFallback && ledgerRead.isDefined && (guardState.sourceKey match {
      case Some(_) => ScrapeHealth.isRewire(guardState.sourceKey, sourceKey)
      case None    => ScrapeHealth.isRewire(None, sourceKey, storedSites, sites(movies.iterator.map(_.filmUrl)))
    })
    if (rewired) RemovalAudit.scrapeRewired(cinema.displayName,
      guardState.sourceKey.orElse(Some(s"rows linking to ${storedSites.toSeq.sorted.mkString("+")}")), sourceKey)

    // DEPTH guard — the same "trust what we have" bail as above, on the axis
    // neither that check nor the breadth guard below can see (a chunked fetch that
    // lost whole dates). The verdict is `ScrapeHealth.depth`'s; this keeps the count
    // of consecutive rejections per venue, which is the only state involved.
    //
    // Count via `upcomingShowtimeCount`, NOT `showtimes.size`: under the read-split every
    // resident slot has been through `stripForCache` and carries `Nil` showtimes, so a
    // direct `.size` reads 0 for every cinema and the floor never engages — which made
    // this guard dead code in production while the specs, wiring no screenings
    // repository, kept their lists resident and passed regardless.
    //
    // And only showtimes still AHEAD of the venue's own clock, on both sides: a passed
    // showtime is nothing the guard can protect, and counting it let a venue stuck on
    // a stale listing (Braniewo's Baszta, still measured against another town's
    // programme) keep a baseline that had long since run out.
    val now                  = models.City.localNow(cinema, clock)
    val knownCinemaShowtimes = corpusIndex.slotsOf(cinema).map { case (_, _, sd) =>
      ShowtimesDigest.upcomingShowtimeCount(sd, now) }.sum
    val batchShowtimes       = movies.iterator.map(_.showtimes.count(_.dateTime.isAfter(now))).sum
    val depthVerdict =
      if (rewired || viaFallback) ScrapeHealth.Depth.Healthy
      else ScrapeHealth.depth(knownCinemaShowtimes, batchShowtimes, guardState.depthRejections,
        maxConsecutiveGuardRejections)
    depthVerdict match {
      case ScrapeHealth.Depth.Reject(consecutive) =>
        // The tick is discarded, so nothing else about the venue changes — least of
        // all its recorded source.
        saveGuardState(cinema, guardState, guardState.copy(depthRejections = consecutive))
        metrics.recordGuardVerdict(ScrapeLandingMetrics.Guard.Depth, ScrapeLandingMetrics.Verdict.Reject)
        RemovalAudit.scrapeDepthGuarded(cinema.displayName, batchShowtimes, knownCinemaShowtimes, consecutive)
        return Seq.empty
      case ScrapeHealth.Depth.AcceptDegraded(consecutive) =>
        // Sustained across several ticks — stop treating it as a bad fetch and let
        // the smaller board land, rather than serving showtimes that no longer exist.
        metrics.recordGuardVerdict(ScrapeLandingMetrics.Guard.Depth, ScrapeLandingMetrics.Verdict.Accept)
        RemovalAudit.scrapeDepthAccepted(cinema.displayName, batchShowtimes, knownCinemaShowtimes, consecutive)
      case ScrapeHealth.Depth.Healthy => ()
    }

    // Cold-mirror guard (only matters when diversion is wired). The newcomer test
    // further down reads the in-memory mirror (`corpusIndex`, and through it the
    // sanitized titles, aliases and cinema slots of every row) to tell a genuinely-new
    // film from a known one. A COLD mirror
    // — empty because a post-reboot `bootHydrate` `findAll()` came back empty while Mongo
    // was still coming up, and the change stream only carries post-boot writes (see
    // `bootHydrate`) — makes EVERY known film look new, so a scrape landing in that window
    // re-diverts the whole corpus into staging and the fold then drags it back over
    // ~30 min: the panel-36 `kinowo_worker_corpus_movies` boot flap (observed 2026-06-28,
    // 812→670→814 after a brief worker restart; ColdMirrorReDivertSpec). Sync the mirror
    // from the repository before deciding, restoring the prod invariant that the mirror
    // reflects `movies`. Cheap in steady state (the `estimatedSize` check short-circuits);
    // the `findAll` runs only while the mirror is genuinely cold, and `rehydrate` only
    // when the corpus actually has rows — a genuinely-empty corpus (a fresh deploy, where
    // a brand-new film SHOULD incubate) skips it and diverts as before. ONE-SHOT, on the
    // first scrape: at a real boot the repo already holds the corpus when the first scrape
    // lands, so the sync fires and warms the mirror for the rest; a fresh harness starts
    // with an empty repo, so the first scrape no-ops and the latch disarms — the sync can
    // never fire at a later, arrival-order-dependent scrape (StagingOrderDeterminismSpec).
    //
    // A corpus read that FAILED is neither: it says nothing about whether the corpus is
    // empty. The tick is discarded — landing it on a cold mirror would divert every known
    // film — and the latch stays armed, so the next scrape tries the sync again.
    if (staging.isDefined && coldMirrorSyncArmed.get() && !syncColdMirror(cinema)) return Seq.empty

    // The listing as this cache records it — titles cleaned by the venue's rules,
    // every screening badged through the shared vocabulary, the venue's several rows
    // for one film folded onto their one slot — in a deterministic order, so the
    // scrape→record step is reproducible whatever order the scraper emitted rows in
    // (`ScrapeListing`). The freshly-written `SourceData` slot rides alongside the
    // public tuple below so the prune can identify "written this tick" by slot
    // reference rather than by cache key — keys can shift mid-tick when a TMDB-stage
    // rekey moves a row out from under us.
    val prepared = ScrapeListing.prepare(cinema, movies, normalizer, screeningTokens)
    val deduped  = prepared.movies
    val cleaned  = prepared.cleaned

    // No `CollectionConverters` here any more: this method no longer walks Caffeine's
    // Java map at all — every question it used to answer by scanning it is a point
    // query on `corpusIndex` now.
    // Partial-scrape guard: count the cinema's slots held BEFORE this tick's writes,
    // then decide whether the fresh batch is implausibly small (a degraded fetch).
    // If so, the prune below is skipped — the films this tick failed to mention keep
    // their slots until a healthy tick, instead of flickering off the site. Generic
    // across cinemas; Multikino (Cloudflare + session wall) is the recurring victim.
    // The BREADTH half of the pair; the depth half bails at the top of this method.
    //
    // Stateful since 2026-09-13 (`ScrapeHealth.breadth`, not the bare `looksPartial`):
    // sustained across `maxConsecutiveGuardRejections` ticks, the prune finally runs
    // anyway — otherwise a venue whose accumulated slot-key count has permanently
    // outgrown what it currently lists (old decorated-title variants, past runs,
    // never pruned — Kino Aurum, 57 slot-keys against an 11-film board) wedges here
    // FOREVER: the ratio can never clear the floor while the very thing that would
    // shrink `knownCinemaSlots` back down is the prune this guard keeps skipping.
    //
    // Only slots the venue's LISTING wrote count: a detail-only slot (see
    // `ScrapeLanding.isDetailOnly`) is no evidence of what the venue lists, and a pile of
    // them held this guard shut over the very prune that clears them.
    val knownCinemaSlots = corpusIndex.slotsOf(cinema).count { case (_, _, sd) => !ScrapeLanding.isDetailOnly(sd) }
    //
    // A rewire skips it like the depth guard: the old source's films are exactly what
    // the prune must retire, however few the new source lists.
    val breadthVerdict =
      if (rewired || viaFallback) ScrapeHealth.Breadth.Healthy
      else ScrapeHealth.breadth(knownCinemaSlots, deduped.size, listingIsComplete,
        guardState.breadthRejections, maxConsecutiveGuardRejections, depthVerdict)
    val breadthRejections = breadthVerdict match {
      case ScrapeHealth.Breadth.Reject(consecutive) =>
        metrics.recordGuardVerdict(ScrapeLandingMetrics.Guard.Breadth, ScrapeLandingMetrics.Verdict.Reject)
        consecutive
      case ScrapeHealth.Breadth.AcceptDegraded(_) =>
        metrics.recordGuardVerdict(ScrapeLandingMetrics.Guard.Breadth, ScrapeLandingMetrics.Verdict.Accept)
        0
      case ScrapeHealth.Breadth.Healthy => 0
    }
    // The listing lands from here on, so its source becomes the venue's recorded one —
    // including on a legacy venue that had none. A scrape that reports no key keeps
    // whatever was recorded.
    if (!viaFallback) saveGuardState(cinema, guardState, ScrapeGuardState(
      sourceKey = sourceKey.orElse(guardState.sourceKey), depthRejections = 0, breadthRejections = breadthRejections))
    // Only `Reject` skips the prune below — `AcceptDegraded` means the guard gave
    // up on treating this as a bad fetch, so it prunes exactly like `Healthy` does. A
    // fallback listing never prunes: it only adds to what the primary last listed.
    val scrapeLooksPartial = viaFallback || breadthVerdict.isInstanceOf[ScrapeHealth.Breadth.Reject]
    // The divert gate's four questions, all POINT queries against `corpusIndex`.
    //
    // Each was a full walk of the corpus, rebuilt on every venue — see [[CorpusIndex]]
    // for what that cost. The questions themselves are unchanged, and so are the
    // reasons they are asked:
    //
    //   - `holdsTitle` — is this film's `sanitize(title)` group already in `movies`?
    //     A genuinely-NEW film is diverted to staging to incubate; a known one keeps
    //     the direct path.
    //   - `holdsAlias` — is it a known film listed under another language? A CONCLUDED,
    //     BARE row's TMDB aliases (Polish + original title) count as known, so a
    //     cinema's original-language listing ("Tangled") lands on the resolved row
    //     instead of incubating a parallel newcomer. Gated on `isBareFilmTitle` so a
    //     decorated edition carrying the base title as an alias can't make a genuinely
    //     new bare film look known.
    //   - `holdsCinemaSlot` / `keysForCinemaSlot` — does a row already hold THIS
    //     cinema's slot, even under a decoration no title rule strips ("Zzz Nonexistent
    //     Fest: Toy Story 5")? Without it a folded row keyed off the BARE display form
    //     never matches the cinema's DECORATED scrape key, so a known film re-diverts
    //     into staging every tick: the served-count flap (Trójmiasto / GCF), reproduced
    //     rule-free in `UnknownBannerReDivertSpec`. The land path uses the same index to
    //     route the scrape onto the row that already holds the slot, so a same-cinema
    //     dub/decorated edition updates in place instead of re-spawning a title-keyed
    //     row and re-folding every tick.
    //   - `rowsFor` — EVERY row a sanitized title currently lives on, for the
    //     different-film check below. Picking one (lowest `canonicalRank`) was
    //     deterministic but asked the wrong question whenever two genuinely different
    //     films share a title, one row per year: a cinema screening the LATER film was
    //     checked against the EARLIER row, correctly told "different film", and diverted
    //     to staging — where the fold put it straight back on the row it belonged to,
    //     for the next scrape to divert again. Forever. Germany, 2026-08-30: "Die
    //     einfachen Dinge" is both a 1953 film (tmdbId 67600) and a 2023 one (1000572).
    //     A divert is only justified when the listing differs from ALL of them.
    //
    // The index moves WITH the writes, which is why this is an index and not a
    // once-per-tick snapshot: each venue's scrape writes rows the next venue must see.
    val diverting = staging.isDefined
    // The canonical key among the rows holding a slot — the ranking stays here, with
    // the one definition of `canonicalRank`, rather than inside the index.
    def keyHoldingCinemaSlot(norm: String, own: Option[SourceData => Boolean]): Option[CacheKey] = {
      val keys = own.fold(corpusIndex.keysForCinemaSlot(cinema, norm))(isOwn =>
        corpusIndex.keysForCinemaSlot(cinema, norm).filter(k => store.get(k).exists(_.cinemaShowings.exists {
          case (cin, slot) => cin == cinema && slot.title.exists(t => normalizer.sanitize(t) == norm) && isOwn(slot)
        })))
      if (keys.isEmpty) None else Some(keys.minBy(FilmCanonicalizer.canonicalRank))
    }
    /** The release year THIS venue already records for this title, wherever its
     *  slot currently sits. A deferred-detail client ships its listing with no
     *  year at all — `IluzjonClient`, `NoweHoryzontyClient`, `KinoPodBaranamiClient`
     *  and `PionierClient` all read `releaseYear` off the per-film DETAIL page — so
     *  the listing tick that decides placement carries none, and the year lands on
     *  the slot a beat later, after the key is settled. Every subsequent tick then
     *  re-derives the same yearless placement, which is why such a split never
     *  heals on its own: prod's `lalka|1968` held Kino Pod Baranami, Iluzjon and
     *  Nowe Horyzonty, all three of them publishing 2026 on their own slots.
     *
     *  `chooseConcluded` already makes exactly this fallback for RUNTIME ("Multikino
     *  sends 0 and its detail page fills the runtime in a beat later — so fall back
     *  to what the venue's own slot already records"); the year had no equivalent.
     *  Ordered by `canonicalRank` so the answer can't depend on iteration order. */
    def venueSlotYear(norm: String): Option[Int] =
      corpusIndex.keysForCinemaSlot(cinema, norm).toSeq.sortBy(FilmCanonicalizer.canonicalRank).iterator
        .flatMap(k => store.get(k).iterator.flatMap(_.cinemaShowings.collectFirst {
          case (cin, slot) if cin == cinema && slot.title.exists(t => normalizer.sanitize(t) == norm) => slot
        }))
        .flatMap(_.releaseYear)
        .nextOption()
    /** Drop `slotKeys` from the row at `key`, retaining each dropped slot's
     *  synopsis (longest-seen, keyed by its source) so the displayed blurb stays
     *  sticky once that slot is gone — see `MovieRecord.retainedSynopses`. Under
     *  the per-title lock via `putIfPresent`, so a concurrent sibling-slot write
     *  isn't clobbered. Shared by the stale-slot prune below and the write loop's
     *  duplicate-slot cleanup (the same (cinema, title) held twice — on another
     *  row, or twice on the row being written). An empty `slotKeys` leaves the
     *  record untouched. */
    def dropCinemaSlots(key: CacheKey, slotKeysOf: MovieRecord => Set[Source]): Unit =
      store.putIfPresent(key, cur => {
        val slotKeys = slotKeysOf(cur)
        // Nothing to drop — the common case now that the write loop asks this of the
        // row it just wrote. Leave the record exactly as it is rather than running
        // the orphaned-chain-detail sweep over a row this drop has no claim on.
        if (slotKeys.isEmpty) cur
        else {
          val captured = slotKeys.iterator.flatMap { s =>
            cur.data.get(s).flatMap(_.synopsis).filter(_.nonEmpty).map(s -> _)
          }.toMap
          val kept = cur.data -- slotKeys
          cur.copy(
            data             = kept -- orphanedChainDetail(kept),
            retainedSynopses = MovieRecordMerge.mergeRetainedSynopses(cur.retainedSynopses, captured))
        }
      })

    /** A chain's shared detail slot once the last venue of that chain has left the
     *  row. `CinemaCityChain` & co carry ONE network-wide detail for every venue of
     *  the chain (`Cinema.chainDetailVenues`); nothing scrapes them, so the scrape
     *  prune — which only ever touches the scraping cinema's own slot — cannot
     *  reach them, and a same-title split that moves the venues away strands the
     *  detail on the row they left. It then keeps describing the other film:
     *  `zaproszenie|1986`, Wanda Jakubowska's war drama, rendered "Reżyseria:
     *  Olivia Wilde" off precisely such an orphan. */
    def orphanedChainDetail(data: Map[Source, SourceData]): Set[Source] = {
      val venuesPresent = data.keySet.flatMap(Source.cinemaOf)
      Cinema.chainDetailVenues.collect {
        case (chain, venues) if data.contains(chain) && !venues.exists(venuesPresent.contains) => chain
      }.toSet
    }
    // This cinema's staging rows, for the prior-slot carry-forward and the staging
    // prune below. Cinema-SCOPED: the inline `findAll().collect { _.cinema == cinema }`
    // it replaces decoded every staged document in the country to keep a handful, which
    // is the same quadratic the corpus walks above carried — and the same one
    // `findByAnchor` was added to fix for the reaper.
    val priorStagingRows: Map[String, services.staging.StagingRecord] =
      staging.fold(Map.empty[String, services.staging.StagingRecord]) {
        _.findByCinema(cinema).iterator.map(r => normalizer.sanitize(r.title) -> r).toMap
      }
    /** This venue's diverted rows, staged in one batch once the loop has finished
     *  deciding which they are. */
    val divertsToStage = scala.collection.mutable.ArrayBuffer.empty[(Source, String, Option[Int], MovieRecord)]
    val divertedSanitized = scala.collection.mutable.Set.empty[String]
    // Titles NEW TO STAGING — no row under their anchor from this cinema or any other —
    // the newcomers whose initial step StagingReaper should kick off an event, rather
    // than the periodic backstop. A re-divert of an already-incubating film (prior row
    // present) is NOT collected, so we don't republish every tick; nor is a venue
    // JOINING a film another venue already staged. Its chain is running, and every
    // step it finishes re-reads the whole group, this venue's row included. Kicking
    // it per venue made the reaper decode the film's whole group once per venue — for
    // a blockbuster staged at 2,441 US venues, quadratic in its showtimes (the US
    // sample leg's scrape tick went from 27s to 1,419s on 2026-09-23).
    val newlyDiverted = scala.collection.mutable.ArrayBuffer.empty[String]

    /** Titles this venue LISTED this tick whose write did not land — a concurrent
     *  `rekey` invalidated the key between the read and the `putIfPresent`, or the
     *  stored row could not be read. `resolved` only collects LANDED writes, so
     *  these titles are absent from `touchedSlots` below and the end-of-tick prune
     *  would drop their existing slots — deleting showtimes the scrape actually
     *  SAW, on the strength of a write that failed for an unrelated reason. The
     *  prune's question is "did the venue stop listing this title", and a skipped
     *  write is no evidence either way. Diverted titles are deliberately NOT spared:
     *  there the listing belongs to a different film, which is a real answer. */
    val listedButNotWritten = scala.collection.mutable.Set.empty[String]
    /** Titles this venue lists as SEVERAL films — "Belle (2013)" beside "Belle (2021)", kept
     *  apart by `ScrapeListing`. Each lands on its own film's row, so a same-title slot on
     *  another row is a stale copy only when it is THIS listing's own (`ownCopy`); dropping
     *  the sibling's slot before the sibling lands rewrote both rows on every tick. */
    val multiFilmTitles: Set[String] =
      deduped.groupBy(cm => normalizer.sanitize(cleaned(cm))).collect { case (norm, cms) if cms.sizeIs > 1 => norm }.toSet
    /** Whether a stored slot `sd` of this venue's title `norm` is `cm`'s own: always for a
     *  title the venue lists once, else only when it records `cm`'s year and credits no
     *  director apart from `cm`'s — `ScrapeListing` keeps the films apart by exactly those,
     *  the year filled from the title's bracket where the venue gave none. */
    /** May `cm` land on the row at `key`? Always, for a title the venue lists once. For a title
     *  it lists as SEVERAL films, not on a row whose film this listing's own credit or year
     *  denies — the venue has just said the title names two films, so a credit that is not the
     *  row's film's is decisive here, where alone it would not be (a pseudonym, a co-director).
     *  Marion Theatre Ocala, US 2026-09-25: Schaffner's undated "Planet of the Apes" (112 min)
     *  landed on Burton's 2001 row — the only resolved row of that title — and the two listings
     *  took turns rewriting its slot on every identical tick. */
    def admits(cm: CinemaMovie, norm: String)(key: CacheKey): Boolean =
      !multiFilmTitles.contains(norm) || store.get(key).flatMap(_.data.get(models.Tmdb)).forall { film =>
        ListingConstraints.venueCreditsApart(cm.director, film.director, normalizer).isEmpty &&
          !YearWindow.contradicts(ScrapeListing.yearOf(cm), film.releaseYear, YearWindow.ProductionToRelease)
      }
    def ownCopy(cm: CinemaMovie, norm: String)(sd: SourceData): Boolean =
      !multiFilmTitles.contains(norm) || (
        ScrapeListing.yearOf(sd) == ScrapeListing.yearOf(cm) &&
          ListingConstraints.venueCreditsApart(sd.director, cm.director, normalizer).isEmpty)

    val resolved: Seq[((CinemaMovie, CacheKey, Boolean), SourceData)] =
      deduped.sortBy(cm => (cleaned(cm), cm.movie.releaseYear.getOrElse(Int.MinValue))).flatMap { cm =>
      val displayTitle = cleaned(cm)
      val norm         = normalizer.sanitize(displayTitle)
      // The year that decides placement: this listing's, else the one it wrote into
      // its own TITLE (see `EmbeddedYear`), else the one this venue's own slot
      // already records (see `venueSlotYear`). Without the last a deferred-detail
      // venue is placed as yearless on every tick, for ever.
      //
      // The title arm is what separates two films a repertory listing distinguishes
      // ONLY by the year it brackets. `sanitize` strips that annotation — rightly,
      // it is not part of a film's identity for a merge key — so "It (1990)" and
      // "It (2017)" both reduce to "it", and a listing arriving with no year of its
      // own lands on whichever of them is already concluded. kinowo_us held exactly
      // that: five venues screening Tommy Lee Wallace's 168-minute miniseries on the
      // row resolved to Muschietti's 135-minute film, which `MixedFilmSplitter` then
      // re-diverted on every settle and the fold put straight back.
      val primary      = store.keyOf(displayTitle,
        cm.movie.releaseYear.orElse(EmbeddedYear.of(displayTitle)).orElse(venueSlotYear(norm)))
      // A newcomer: `staging` is wired and this film's sanitize group isn't in
      // `movies` yet — AND it isn't a known film listed under another language (an
      // alias of a concluded row). (Same-tick spelling variants already collapsed
      // in `deduped`.)
      // …or a film this cinema is showing that is NOT the film the row already
      // describes. Without this, a row keyed by TITLE alone absorbs any same-titled
      // film — "Joanna d'Arc" ends up carrying both Besson's 1999 picture and
      // Pálmason's 2025 one and resolves to neither. Diverting sends it down the
      // newcomer path, where it resolves on its own hints and folds into its own
      // row. Needs a differing original title CORROBORATED by runtime or year, so a
      // cinema that merely prints the Polish title in `originalTitle` — common on
      // the smaller sites — is waved through (see `MixedFilmDetector`).
      // The corpus's answers for this listing — every question the settle would ask a
      // tick later, asked now (see `ListingLanding`).
      val landing = ListingLanding.ask(corpusIndex, store.get,
        ListingLanding.Listing(displayTitle, cinema, cm.movie.originalTitle, cm.movie.runtimeMinutes,
          cm.movie.releaseYear, cm.director), normalizer, diverting)
      val sameTitledRows = landing.sameTitledRows
      val aDifferentFilm = landing.aDifferentFilm
      val divert         = landing.divert(diverting)
      store.withTitleLock(primary.cleanTitle) {
        if (divert) {
          // NEWCOMER → staging. Build the slot off this cinema's PRIOR staging
          // slot (preserves two-stage detail fields + the year fallback), write
          // one `cinema|title|year` row, and DON'T touch `movies` — it stays held
          // out of the read model until it resolves and folds in (the promoter +
          // folder own that). Excluded from `resolved`, so no movies-side
          // prune/publish fires for it.
          val priorSlot     = priorStagingRows.get(norm).flatMap(_.record.data.get(cinemaSlotKey(cinema, displayTitle)))
          val effectiveYear = cm.movie.releaseYear.orElse(priorSlot.flatMap(_.releaseYear))
          val slot          = cinemaSlots.build(cm, displayTitle, priorSlot, effectiveYear)
          // COLLECTED, not written here: the venue's diverts go out together below, in
          // two round trips rather than three per listing. Nothing later in this loop
          // reads staging back — `priorStagingRows` was captured before it, and the
          // prune at the end works off `divertedSanitized` — so the rows are the same
          // rows, written at the end of the venue instead of one at a time through it.
          //
          // Outside `withTitleLock` by consequence, and that costs nothing: the lock
          // serialises read-modify-write on the MOVIES row, and staging's other writer
          // (`StagingFolder`) never takes it, so it has never serialised this collection.
          // A staging row is keyed by `cinema|title|year` and one cinema is scraped by
          // one thread, so no second scrape can be writing the rows this venue owns.
          divertsToStage += ((cinema, displayTitle, cm.movie.releaseYear, MovieRecord(
            searchTitle = Some(normalizer.apiQuery(normalizer.recase(displayTitle))),
            data        = Map(cinemaSlotKey(cinema, displayTitle) -> slot)
          )))
          divertedSanitized += norm
          if (!priorStagingRows.contains(norm) && !staging.exists(_.holdsAnchor(norm))) newlyDiverted += displayTitle
          None
        } else {
          // Land the slot on the *canonical* key for this film, chosen by
          // `canonicalRank` (NOT arrival order): when this cinema's primary key
          // out-ranks the existing variant's (a year where the row was created
          // yearless, a lower year, or a casing that sorts first), promote the
          // whole row onto it before writing. Keeps the stored `(cleanTitle,
          // year)` a pure function of the reported variants. A scrape of an
          // already-concluded film lands straight on the resolved row; falls back
          // to the unique-match redirect for not-yet-concluded films.
          val key = concludedKeyFor(primary, cm.movie.runtimeMinutes.filter(_ > 0), cinema, admits(cm, norm)).getOrElse {
            redirectToExistingVariant(primary).filter(admits(cm, norm)) match {
              case Some(existingKey) =>
                // A RESOLVED row's key is authoritative — TMDB's title + year,
                // settled when the film concluded — so never re-key it onto a
                // cinema's raw spelling. The promote below is for not-yet-resolved
                // variants only (yearless→yeared, a lower year, a casing that sorts
                // first). Without this gate, a cinema that lists a resolved film
                // ALL-CAPS at a production year ≥2 off TMDB's release year (so
                // `concludedKeyFor`'s ±1 window misses it) would promote the row to
                // that all-caps / lower-year key — the "PÓŁNOC, PÓŁNOCNY ZACHÓD"
                // (1957) re-casing of "Północ, północny zachód" (TMDB 1959). The
                // cinema slot still merges in; only the row's own key is preserved.
                val existingRow      = store.get(existingKey)
                val existingResolved = existingRow.exists(_.tmdbId.isDefined)
                // Nor onto a year the row's own venues DISPUTE. A yearless row whose
                // slots bracket two different years ("It (1990)" beside "IT (2017)",
                // folded together before either resolved) is yearless because the
                // settle's `backfillEmbeddedYears` reads all its titles and finds no one
                // year; promoting it on ONE listing's bracket re-keyed it on every
                // identical rescrape (the US hard-cluster churn). The same reading here
                // keeps the landing and the settle agreeing on where the row lives.
                val yearDisputed = existingKey.year.isEmpty && EmbeddedYear.of(displayTitle).isDefined &&
                  EmbeddedYear.ofAll(existingRow.toSeq.flatMap(EmbeddedYear.slotTitles) :+ displayTitle).isEmpty
                if (existingResolved || yearDisputed) existingKey
                else {
                  val canonical = Seq(primary, existingKey).minBy(FilmCanonicalizer.canonicalRank)
                  if (canonical != existingKey) store.rekey(existingKey, canonical, identity, RekeyReason.ScrapeVariant)
                  canonical
                }
              // No title/year match — but if some row ALREADY holds this exact
              // (cinema, title) slot (a same-cinema dub/decorated edition folded
              // onto the base film under a different display title), land on THAT
              // row and update the slot in place, instead of spawning a new
              // title-keyed row that re-resolves and re-folds every tick.
              // …then the resolved film this listing decorates, ranked the way the
              // settle would rank the fold's survivor; only a listing nothing holds
              // starts a row of its own.
              // A title the venue lists as several films lands only on a row holding THIS
              // film's slot: the sibling's row is the other film ("Sinn und Sinnlichkeit"
              // 1995 and 2026 at Cinema-Arthouse shared one row, the later overwriting the
              // earlier's showtimes).
              case None => keyHoldingCinemaSlot(norm, Option.when(multiFilmTitles.contains(norm))(ownCopy(cm, norm))).orElse(landing.fallbackKey).getOrElse(primary)
            }
          }
          val existingOpt   = store.get(key)
          val existing      = existingOpt.getOrElse(MovieRecord())
          val priorSlot     = existing.data.get(cinemaSlotKey(cinema, displayTitle))
          val effectiveYear = cm.movie.releaseYear.orElse(priorSlot.flatMap(_.releaseYear))
          val slot          = cinemaSlots.build(cm, displayTitle, priorSlot, effectiveYear)
          // `isNew` controls whether to publish `MovieDetailsComplete`. Dedup
          // against the prior slot for this cinema so the same `(title, year)`
          // reported tick after tick doesn't churn downstream listeners.
          val isNew = !priorSlot.exists(s => s.title.contains(displayTitle) && s.releaseYear == effectiveYear)
          // Existing rows go through `putIfPresent` (a `$set`-diff that preserves
          // out-of-band edits); first-time scrapes `put` (keeps the tmdbId
          // identity gate live).
          val slotKey = cinemaSlotKey(cinema, displayTitle)
          // Whether the slot actually LANDED on `key`, which is what makes the move
          // below safe. I deleted this once, reasoning that a Caffeine miss implies no
          // row holds the slot because every key here comes from `corpusIndex`. That is
          // false for one arm: the redirect above returns `canonical`, which can be
          // `primary` — a key `redirectToExistingVariant` has just proved is a MISS —
          // and it returns it whether or not the `rekey` landed. `rekey` defers
          // silently on an unreadable row and on a `moveFilm` that fails, so under a
          // degraded Mongo the loop stands on a key that is in neither cache nor index
          // while `keysForCinemaSlot` still names the row that holds the slot. Dropping
          // it there deletes the venue's showtimes and puts them nowhere.
          val landed = existingOpt match {
            case Some(_) =>
              // Its RESULT, not `true`. `putSlotIfPresent` answers false when the key is no
              // longer in Caffeine by the time it computes — a concurrent `rekey` of a
              // DIFFERENT title invalidates keys without holding this title's lock — or
              // when the repository write itself failed for a row the cache still holds.
              // Assuming the write landed is what lets the move below strip a slot that
              // was never replaced. The gate is only worth having if it reads the write.
              // (Both failure shapes are METERED — see `ScrapeLandingMetrics` — at the
              // one place inside the cache's resident write that actually knows which
              // happened, not here: this call site cannot tell them apart.)
              store.putSlotIfPresent(key, slotKey, slot)
            case None =>
              // A cache MISS is not proof of first-time: a restart/eviction/re-key
              // can leave a fully-rated Mongo row unseen by Caffeine. Build the
              // full-record `put` on the STORED record (`stored` = cache-or-Mongo),
              // NOT the bare `existing` (empty on a miss) — else the `replaceOne`
              // nulls the ratings the `*Ratings` refreshers own. We `put` here (not
              // `putIfPresent`) deliberately: `putIfPresent` is a `computeIfPresent`
              // no-op on a cold key, so it would drop the slot write; and `put`
              // keeps the tmdbId identity gate live for a genuine first-time scrape
              // (where `stored` is empty and this collapses to the old behaviour).
              // `searchTitle` stays absent — a row landing straight in `movies`
              // carries none, so movies ingestion stays a pure function of the
              // canonical title, no scrape-order dependence.
              // …and a cache miss whose Mongo read FAILED is not proof of anything at all.
              // Treating it as first-time builds the record from scratch, so it carries
              // only the cinema being scraped right now, and `MovieRepository.upsert`
              // writes that as the whole film — `screenings.replaceFilm` then prunes every
              // OTHER cinema's showtimes. One unreadable row costs that film its board;
              // a cold cache after a restart routes the entire corpus down this branch,
              // which is how 2026-07-27 lost ~60% of the showtimes in every country while
              // the film counts stayed flat. Skip the film instead: the slot is simply not
              // recorded this tick, the stored rows are left exactly as they are, and the
              // cinema's next scrape (or the cache's next hydrate) picks it up.
              store.storedChecked(key) match {
                case (_, false) =>
                  logger.warn(s"Skipping '${key.cleanTitle}' (${key.year.getOrElse("—")}) from " +
                    s"${cinema.displayName}: its stored row could not be READ, and rebuilding it from " +
                    "this scrape alone would prune every other cinema's showtimes.")
                  store.skippedUnreadable.incrementAndGet()
                  metrics.recordWriteSkipped(ScrapeLandingMetrics.SkipReason.UnreadableRow)
                  false
                case (row, true) =>
                  val base = row.getOrElse(MovieRecord())
                  // Its RESULT, not `true` — the same reason as `putIfPresent` above: a write
                  // that did not LAND left the slot nowhere new, and the move below would then
                  // strip it off the row that still holds it. Only `Written` landed: a DECLINED
                  // write threw nothing but stored nothing either — the cache refusing a key
                  // another film holds, an unreadable key or a fold whose side rows could not be
                  // carried, and `movies` refusing an identity another document holds.
                  store.put(key, base.copy(data = base.data + (slotKey -> slot))) == WriteOutcome.Written
              }
          }
          // This tick just decided which film this (cinema, title) belongs to, so
          // any OTHER slot for the same (cinema, title) is stale by construction —
          // drop it, wherever it sits. The write above lands the slot; without this
          // it only ever COPIED, and the sole removal was the end-of-tick prune, which
          // stands down on a partial scrape and only sees rows this cinema's own
          // index lists. That is how "Zaproszenie" served the Kinepolis 21:40 and
          // Kino Malta showtimes under BOTH `zaproszenie|2026` and
          // `zaproszenie|1986` — one screening, two films, on the live site.
          // Unlike the prune this is safe on a degraded tick: it removes a slot
          // only because we just OBSERVED this venue listing this title, never
          // because a fetch failed to mention it.
          // The slots to drop are read off the row rather than assumed to be
          // `slotKey`: the same venue's slot for this film can sit there under a
          // different Source spelling (a bare `Cinema` from an older write, a
          // `CinemaShowing` keyed off the title an earlier derivation produced —
          // the staging fold keys the slot by the STAGED row's title, the detail
          // merge by the row's), and dropping only `slotKey` would miss exactly the
          // stranded copies this is here to clear.
          // Including the row just written (`+ key`), which is where the venue's
          // OWN duplicate sits — one venue listing one title is one slot, so a
          // second slot on this row for the same (cinema, sanitized title) is stale
          // by the same argument. Prod 2026-09-08: Cinema City Wolność held
          // `terminator2dziensadu35rocznica` (spent) beside `terminator2dziensadu`
          // (3 future showtimes) on `terminator2dziensadu|1991`, both titled
          // "Terminator 2: Dzień sądu 35. Rocznica"; the read model composed one
          // `web_screenings` id from the pair and served the spent half in nine
          // cities for days. `- key` left it there for ever: the end-of-tick prune
          // is the only other remover and it stands down on every chunked venue.
          // The slot just written is excluded on that row (and only there — the
          // SAME source key on another row is exactly what this drop retires), so
          // this can never remove the showtimes it just recorded.
          // Gated on the write above: see `landed`.
          // …and of a title this venue lists as several films, only the copies of THIS
          // listing (see `multiFilmTitles`).
          if (landed) (corpusIndex.keysForCinemaSlot(cinema, norm) + key).foreach { row =>
            val justWritten: Set[Source] = if (row == key) Set(slotKey) else Set.empty
            // The venue's sources on the row from the index when it holds this very
            // record — not a walk of the row's every slot, which on a film shown at N
            // venues made each listing O(N) and the tick O(N²) — else from the record.
            dropCinemaSlots(row, cur =>
              corpusIndex.sourcesAt(row, cinema, cur)
                .getOrElse(cur.data.keysIterator.filter(Source.cinemaOf(_).contains(cinema)).toSet)
                .filter(src => cur.data.get(src).exists(sd => sd.title.exists(t => normalizer.sanitize(t) == norm) && ownCopy(cm, norm)(sd))) -- justWritten)
          }
          // Gated on the write having LANDED. A skipped write leaves Caffeine
          // without the row, so announcing it as new sends `MovieDetailsComplete` /
          // `classify`'s `detailPending` write at a key nothing holds — and
          // `MovieService` then finds no row and dispatches a full TMDB re-resolve
          // against an empty one. Nothing was recorded this tick, so nothing
          // downstream should be told that something was.
          // Spare this title's existing slot from the prune below: we observed the
          // venue listing it, so the only thing the failed write proves is that the
          // write failed.
          if (!landed && !divert) listedButNotWritten += norm
          Option.when(landed)(((cm, key, isNew), slot))
        }
      }
    }

    // The venue's newcomers, in ONE read and ONE bulk write. Every divert decision is
    // made by now, and nothing between here and there read staging back.
    staging.foreach(_.upsertAll(divertsToStage.toSeq))

    // Prune (movies): any of THIS cinema's slots that existed before but weren't
    // touched this tick → drop that slot. With per-(cinema,title) slots a venue can
    // hold several (original + dub); a tick scrapes them all, so an untouched slot
    // means that specific title stopped screening (or the dub did) — drop just it,
    // keeping the venue's other slots. The record itself stays.
    //
    // Identify "touched" by the slot's `SourceData` reference rather than by cache
    // key. The prune runs OUTSIDE the per-title lock, so a concurrent `cache.rekey`
    // can move a row to a year-keyed sibling — carrying our just-written slot along.
    // Slot-identity tracking survives that move because `cache.rekey` preserves the
    // SourceData reference verbatim.
    // …BUT skip the prune entirely when this tick looks like a partial/degraded
    // scrape (see `scrapeLooksPartial`): deleting slots a broken fetch merely
    // failed to list is what makes a still-playing film flicker off the site. A
    // healthy tick (full board) prunes normally, dropping whatever genuinely
    // stopped screening.
    val touchedSlots: Set[SourceData] = resolved.iterator.map(_._2).toSet
    // This cinema's slots, straight from the index — the seventh full-corpus walk
    // this method used to make per venue, and the one that ran on every healthy tick.
    // Touched by reference, OR named by a listing whose write was skipped — see
    // `listedButNotWritten`. A slot in neither set is one the venue genuinely
    // stopped listing, which is the only thing this prune is entitled to act on.
    // Drop every untouched slot of this cinema's that `stale` picks, and put the batch on the
    // record — the signal the served-films sawtooth needed: which cinema dropped how many
    // slots off how many still-known films this tick, and why.
    def pruneSlots(reason: String)(stale: SourceData => Boolean): Unit = {
      val toPrune = corpusIndex.slotsOf(cinema).iterator
        .collect { case (k, s, sd) if !touchedSlots.contains(sd) && stale(sd) => k -> s }
        .toList.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toList
      toPrune.foreach { case (k, staleKeys) => dropCinemaSlots(k, _ => staleKeys) }
      RemovalAudit.scrapePruned(cinema.displayName, films = toPrune.size,
        slots = toPrune.iterator.map(_._2.size).sum,
        sampleFilmIds = toPrune.map { case (k, _) => s"${k.cleanTitle} (${k.year.getOrElse("—")})" },
        reason = reason)
    }
    def runPrune(): Unit = {
      val spared = listedButNotWritten.toSet
      pruneSlots("scrape-prune")(sd => !sd.title.exists(t => spared.contains(normalizer.sanitize(t))))
    }
    // A fallback-served listing never prunes — it only adds (see `scrapeLooksPartial`).
    if (!viaFallback) breadthVerdict match {
      case ScrapeHealth.Breadth.Reject(consecutive) =>
        // The guard skipped the prune — log the decision (and what it spared) so a
        // degraded-tick episode is on the record even though nothing was removed.
        RemovalAudit.scrapePruneSkipped(cinema.displayName, batchFilms = deduped.size,
          knownSlots = knownCinemaSlots, consecutive, reason = "partial-scrape-guard")
        // What the guard spares is the venue's LISTED films a thin tick failed to mention.
        // A detail-only slot was never listed, so a thin tick is no reason to keep it.
        pruneSlots("detail-only-slot")(ScrapeLanding.isDetailOnly)
      case ScrapeHealth.Breadth.AcceptDegraded(consecutive) =>
        // Sustained across enough ticks to stop being a bad-fetch guess — the prune
        // finally runs, so log that it is about to let go of whatever this venue's
        // accumulated slot bloat turns out to be.
        RemovalAudit.scrapePruneAccepted(cinema.displayName, batchFilms = deduped.size,
          knownSlots = knownCinemaSlots, consecutive)
        runPrune()
      case ScrapeHealth.Breadth.Healthy =>
        runPrune()
    }

    // Prune (staging): drop this cinema's staging rows it no longer lists this
    // tick — the staging analogue of the movies prune. A row that graduated to
    // `movies` was deleted by the folder (so it's absent from `priorStagingRows`);
    // one that simply stopped screening is removed here. Skipped on a partial
    // scrape for the same reason as the movies prune above.
    if (!scrapeLooksPartial) staging.foreach { s =>
      (priorStagingRows.keySet -- divertedSanitized).foreach { stale =>
        s.deleteRow(priorStagingRows(stale))
      }
    }

    // Publish CinemaMovieAdded for each `movies` row we just first-scraped onto,
    // AFTER the slot put + prune so a handler reading the cache immediately sees
    // the freshly-written slot. Gated on `isNew`. Diverted newcomers don't fire
    // this — their enrichment is driven by the staging promoter.
    resolved.foreach { case ((cm, key, isNew), _) =>
      if (isNew) bus.publish(CinemaMovieAdded(cinema, key.cleanTitle, key.year, cm.filmUrl))
    }

    // Kick each first-time newcomer's staging chain immediately. Published AFTER
    // the staging `upsert`s above so a synchronous StagingReaper handler reading
    // `pending_movies` sees the freshly-written row. Distinct: a same-tick spelling
    // variant can't fire the same film's anchor twice.
    newlyDiverted.distinct.foreach(t => bus.publish(StagingNewcomerDiverted(t)))

    resolved.map(_._1)
  }

  /** The canonical key of an already TMDB-concluded row this scrape matches —
   *  same normalised title, and a matching year when the scrape carries one. A
   *  later scrape of a known film lands its slot straight on the
   *  resolved/concluded row, so the enrichment + TMDB trigger is skipped
   *  (`CinemaScrapeRunner.classify` short-circuits on `tmdbConcluded`) and no
   *  held-back variant is spawned beside it. None when no concluded match exists.
   *
   *  Year matching mirrors `clusterByFilm`'s rule 2 — the same
   *  `YearWindow.ProductionToRelease` — so the duplicate `canonicalizeBySanitize`
   *  would later fold is never spawned: a yearless scrape lands on any concluded
   *  same-title row; a year-bearing scrape prefers the concluded row at the SAME
   *  year, else the nearest within the window (a cinema reporting the production
   *  year 2025 lands on the row TMDB resolved to the release year 2026 — the "two
   *  copies of Kumotry" bug). Ties break on `canonicalRank`.
   *
   *  `listingRuntime` is the runtime THIS listing published, and `cinema` the venue
   *  publishing it. Both are only consulted when the candidates turn out to be more
   *  than one FILM — see [[chooseConcluded]]. */
  private def concludedKeyFor(
    primary:        CacheKey,
    listingRuntime: Option[Int],
    cinema:         Cinema,
    admits:         CacheKey => Boolean
  ): Option[CacheKey] = {
    val norm = primary.normalized
    // A scrape lands on a concluded row when its title matches that row's key, OR
    // one of the row's TMDB aliases (its Polish / original title). The alias arm
    // lands a cinema's original-language listing of a film ("Tangled") straight on
    // the existing resolved row ("Zaplątani") instead of spawning a translation
    // duplicate for the next settle to merge.
    //
    // The alias arm is gated on `isBareFilmTitle` — the SAME predicate
    // `FilmCanonicalizer.groupByFilm` uses to decide which same-tmdbId rows may
    // fold. A decorated edition ("Plenerowe Pałacowe: Ścieżki życia") enriched off
    // the base film via the apiQuery programme-prefix strip carries the BASE title
    // ("Ścieżki życia") as a TMDB alias; without the gate a bare "Ścieżki życia"
    // scrape would match that decorated row through the alias and (FilmCanonicalizer.canonicalRank's
    // 'P' < 'Ś' tiebreak) land on it — splitting the bare film and re-diverting it
    // every tick. Gating keeps the scrape path's redirect consistent with the
    // settle's fold: only a row that is itself a bare presentation of the film is
    // a valid alias target. The decorated→own-row direction is already self-gating
    // (its sanitize matches no alias).
    //
    // BOTH ARMS ARE INDEX LOOKUPS. This walked the whole `positive` map — running
    // `isBareFilmTitle` and a `sanitize` per alias on every row — once per LANDED
    // LISTING, which is O(listings x corpus) and the reason a cold United States pass
    // cost 72ms a listing against Germany's 8.7ms on the same code. The corpus grows as
    // the listings land, so the price of a fixed chunk of venues climbs through the
    // tick; `CorpusIndex` exists for exactly this shape and already carried both
    // derivations, one of them (the alias set) in a form that could only answer
    // `holdsAlias`.
    //
    // The candidates are a SET, not a sequence: `chooseConcluded` below is "a pure
    // function of the row set plus this listing", so replacing an arbitrary Caffeine
    // iteration order with an index lookup cannot change which key is chosen.
    // Prefer a row whose OWN key IS this title over one that matches only via a
    // TMDB alias. The alias arm exists to land an original-language listing that
    // has no row yet ("Tangled") onto the resolved row ("Zaplątani"); but once a
    // row keyed by the scraped title exists, it must win — otherwise a same-tmdbId
    // sibling under a different-language key ("Denʹ istyny - UA", its TMDB alias
    // also "Dzień objawienia") steals the scrape whenever its cleanTitle sorts
    // first in the FilmCanonicalizer.canonicalRank tie-break ("De" < "Dz"), splitting the Polish
    // film across two ever-growing rows the sanitize-keyed canonicalize can't
    // re-merge. So resolve key-matches first, alias-only matches only as fallback.
    val keyMatches = corpusIndex.entriesFor(norm).collect { case (k, e) if e.tmdbConcluded && admits(k) => k }
    // `keysForAlias` is already gated on concluded-AND-bare (the index's own predicate),
    // so the only arm left to apply is the partition's: a key that matches by its own
    // normalised form belongs to `keyMatches`, never here.
    val aliasOnly  = corpusIndex.keysForAlias(norm).filterNot(_.normalized == norm).filter(admits).toSeq
    // Nearest year first, out to `YearWindow.ProductionToRelease` — the settle's own
    // window (`FilmCanonicalizer.clusterByFilm` rule 2), read from the same place so
    // the two cannot drift: a venue reporting the PRODUCTION year two years before
    // TMDB's release year ("Zawieście czerwone latarnie", 1989 vs 1991) used to land
    // as its own row for the settle to attach a tick later.
    def nearest(cands: Seq[CacheKey]): Option[CacheKey] = primary.year match {
      case None    => chooseConcluded(cands, listingRuntime, cinema, norm)
      case Some(y) =>
        (0 to YearWindow.ProductionToRelease).iterator.map(distance =>
          chooseConcluded(cands.filter(_.year.exists(YearWindow.distance(_, y) == distance)), listingRuntime, cinema, norm))
          .collectFirst { case Some(k) => k }
    }
    // A BARE listing — no year, no minutes — that this venue already holds on a RESOLVED row
    // stays there when the rows keyed by its title are not that film. It says nothing that
    // names another film; its home is where its own resolution put it (the fold), and moving
    // it to a same-titled row the settle then takes it back from is churn on every tick. PL,
    // 2026-09-25: Kino Amok's bare "Samson i Dalila" sits on DeMille's 1949 film (keyed by
    // another venue's spelling); the only row KEYED "Samson i Dalila" is the Met's 2026
    // broadcast, a different film by its own venue's year and director.
    def incumbent: Option[CacheKey] =
      if (!ListingConstraints.keepsIncumbentHome(primary.year, listingRuntime)) None
      else corpusIndex.keysForCinemaSlot(cinema, norm).toSeq
        .filter(k => store.get(k).exists(_.tmdbId.isDefined))
        .filterNot(k => keyMatches.exists(m => m != k && store.get(m).flatMap(_.tmdbId) == store.get(k).flatMap(_.tmdbId)))
        .minByOption(FilmCanonicalizer.canonicalRank)
    nearest(keyMatches) match {
      case Some(k) if store.get(k).exists(_.tmdbId.isEmpty) => incumbent.orElse(Some(k))
      case Some(k)                                         => Some(k)
      case None                                            => nearest(aliasOnly)
    }
  }

  /** Pick the concluded row a listing belongs to, from candidates the year has
   *  already narrowed as far as it can.
   *
   *  When they are all rows of ONE film — the overwhelming case, several spellings
   *  or year-variants of the same tmdbId — any of them is the right answer and
   *  `canonicalRank` picks the canonical one, exactly as before.
   *
   *  When they are DIFFERENT FILMS, `canonicalRank` is the wrong instrument. It is a
   *  canonicalisation order for rows of one film — year-bearing first, then the
   *  LOWER year — so across two films it means "the older film always wins".
   *  Production, 2026-08-14: `tylkojednanoc` was held by both Antonioni's "La notte"
   *  (1961) and the "One Night Only" romcom (2026), and every yearless listing of the
   *  romcom — 32 of the 33 listings in the archived corpus publish no year — was
   *  routed to the 1961 film. The venues' screenings ended up split across the two
   *  rows, the read model projected a card for each, and `/poznan` showed the same
   *  film twice under one slug with the same booking links on both cards.
   *
   *  So ask the listing instead, in order of how much the answer can be trusted:
   *
   *   1. THE RUNTIME IT PUBLISHED, against each candidate film's own — see
   *      [[RuntimeCorroboration]]. The venues that print no year do print minutes.
   *   2. THE FILM MORE VENUES ARE SCREENING. Runtime cannot speak for a venue that
   *      publishes none — 34 of this film's slots on prod were in exactly that state,
   *      enough to keep the duplicate card alive on their own. A title shared by a
   *      current release and an old picture is overwhelmingly the release when a venue
   *      says nothing else, and counting the venues says which is which. This
   *      deliberately outranks step 3: a venue with no evidence that stays where it
   *      happens to sit keeps a split alive forever, because nothing else will ever
   *      move it. The count is monotone as venues migrate, so the corpus converges
   *      rather than flapping. It only decides what step 1 could not, so a repertory
   *      house that publishes its minutes still reaches the old film.
   *   3. WHERE THIS VENUE'S LISTING ALREADY SITS — the last resort before an arbitrary
   *      pick, for a group where even the venue counts tie.
   *
   *  Only then `canonicalRank`, which stays the tie-break within one film. Every step
   *  is a pure function of the row set plus this listing, so the answer cannot depend
   *  on arrival order.
   *
   *  Deliberately NOT `Verdict.of`, though both weigh a listing's minutes against a
   *  film's own. The verdict is a yes/no VETO on one candidate — minutes a category
   *  apart deny it, anything nearer passes — and here every candidate would pass:
   *  105 minutes is plausible for both the 102-minute romcom and Antonioni's 121.
   *  This is a CHOICE among rivals, which needs the nearest, and it must go on to
   *  venue counts and incumbency when the minutes cannot choose; the verdict's crew
   *  arm has nothing to read because a same-titled rival's director is never on the
   *  listing's own row. `SameFilmVocabulariesSpec` pins the difference. */
  private def chooseConcluded(
    candidates:     Seq[CacheKey],
    listingRuntime: Option[Int],
    cinema:         Cinema,
    norm:           String
  ): Option[CacheKey] = {
    def rowAt(key: CacheKey): Option[MovieRecord] = store.get(key)
    def films(keys: Seq[CacheKey]): Int = keys.flatMap(rowAt(_).flatMap(_.tmdbId)).distinct.size
    /** This venue's own slot for this title on a candidate row, if it holds one. */
    def venueSlot(key: CacheKey): Option[SourceData] =
      rowAt(key).flatMap(_.cinemaShowings.collectFirst {
        case (cin, slot) if cin == cinema && slot.title.exists(t => normalizer.sanitize(t) == norm) => slot
      })
    if (candidates.sizeIs <= 1 || films(candidates) <= 1) candidates.minByOption(FilmCanonicalizer.canonicalRank)
    else {
      // The minutes THIS venue gives the film. A listing tick often carries none —
      // Multikino sends 0 and its detail page fills the runtime in a beat later — so
      // fall back to what the venue's own slot already records. `minOption` rather than
      // "the first one found": the candidates come out of a Caffeine map whose iteration
      // order is not guaranteed, and this answer must not depend on it.
      val published = listingRuntime.orElse(candidates.flatMap(venueSlot).flatMap(_.runtimeMinutes).minOption)
      // Each candidate film's OWN runtime comes off its `Tmdb` slot, not the merged
      // `runtimeMinutes`: a row that has been absorbing another film's listings reports
      // THEIR minutes through the merge, and that row is precisely what is in question.
      // …falling back to the IMDb slot when TMDB carries none. TMDB routinely has
      // no runtime for a film it has not released yet, and `strictNearest` only
      // compares candidates that carry one — so the OTHER film became the sole
      // entrant and won by walkover, however far off the published minutes it was.
      // Prod: the 2026 "Lalka" has no TMDB runtime, so Has's 1968 row (152) took
      // every venue publishing the new film's 162. IMDb has that 162. The IMDb slot
      // is as film-specific and as free of cinema-merge contamination as the TMDB
      // one, which is the property this needs.
      def ownRuntime(k: CacheKey): Option[Int] =
        rowAt(k).flatMap(r => r.data.get(models.Tmdb).flatMap(_.runtimeMinutes)
          .orElse(r.data.get(models.Imdb).flatMap(_.runtimeMinutes)))
      val byRuntime = RuntimeCorroboration.strictNearest(published, candidates.map(k => k -> ownRuntime(k)))
      val byIncumbency = candidates.filter(venueSlot(_).isDefined)
      val byVenueCount = candidates.groupBy(k => rowAt(k).map(_.cinemaShowings.size).getOrElse(0))
        .maxByOption(_._1).map(_._2).getOrElse(Nil)
      Seq(byRuntime.toSeq, byVenueCount, byIncumbency)
        .find(narrowed => narrowed.nonEmpty && films(narrowed) == 1)
        .getOrElse(candidates)
        .minByOption(FilmCanonicalizer.canonicalRank)
    }
  }


  /** If `primary` doesn't currently exist in the cache, look for an existing
   *  row that already knows `primary.cleanTitle` (via its `cinemaTitles`
   *  set). Returns that row's key when there's exactly one such match —
   *  ambiguous (cross-film collision) and zero-match cases fall through to
   *  None and the caller creates a fresh row.
   *
   *  Match uses `normalizer.sanitize` so an Arabic/Roman /
   *  punctuation / case variant ("Mortal Kombat 2") redirects onto a row
   *  that only knows the canonical form ("Mortal Kombat II") — without
   *  this, the FIRST cinema's raw spelling would pin the row's
   *  `cinemaTitles` and every other cinema's variant of the same film
   *  would create a separate row at its own key. */
  private def redirectToExistingVariant(primary: CacheKey): Option[CacheKey] = {
    if (store.get(primary).isDefined) return None
    // Match by cleanTitle normalisation. Cross-script titles produce
    // different normalised forms (sanitize keeps Unicode letters), so a
    // Cyrillic row can't be matched by a Latin scrape and vice versa.
    //
    // `rowsByNormalized` is keyed by exactly this, and holds one entry per `CacheKey`,
    // so the lookup replaces both the whole-corpus filter — the SECOND such walk on the
    // per-listing path, run for every listing `concludedKeyFor` missed, which cold is
    // nearly all of them — and the `distinctBy` that deduplicated its result.
    val candidates = corpusIndex.entriesFor(primary.normalized)
    candidates match {
      case Seq((key, record)) if settleWouldMerge(primary, key, record) => Some(key)
      case _                                                           => None
    }
  }

  /** Would the SETTLE put a fresh scrape keyed `primary` on the row at `key`?
   *
   *  Asked of `FilmCanonicalizer` itself — the same `groupByFilm` → `clusterByFilm`
   *  composition `canonicalizeBySanitize` runs over the whole corpus and
   *  `StagingFold.planGroup` runs over a fold's neighbourhood — rather than
   *  re-stated here, because a second opinion about what one film is doesn't stay a
   *  second opinion: it becomes a loop between the two.
   *
   *  It was one. This redirect matched on the sanitized title ALONE, while
   *  `clusterByFilm` only joins a year-bearing unresolved row to a resolved cluster
   *  within ±2 of its TMDB year. Poland's corpus, 2026-09-02: thirteen Cinema City
   *  venues list "Ktoś całkiem obcy" at releaseYear 2024 (Brandt Andersen's *The
   *  Strangers' Case*), and TMDB — which does not carry that film's Polish title —
   *  answers a bare-title search with the 2007 *Perfect Stranger*. The settle kept
   *  the two rows apart, seventeen years being rather more than two; this redirect
   *  handed all thirteen Cinema City slots to the 2007 row anyway, one tick after
   *  the corpus was declared settled, and the convergence leg failed on the 26
   *  writes it took.
   *
   *  A candidate can only DIFFER from `primary` in its year — an equal key would
   *  have been found by `getIfPresent` above — so this is exactly the year question
   *  and nothing else. The incoming scrape is represented by an empty record: what
   *  the rules read from it is the year in its key, and claiming any more than that
   *  for a row that does not exist yet would be inventing evidence. */
  private def settleWouldMerge(primary: CacheKey, key: CacheKey, record: MovieRecord): Boolean = {
    val pair = Seq(primary -> MovieRecord(), key -> record)
    FilmCanonicalizer.groupByFilm(pair, normalizer)
      .flatMap(FilmCanonicalizer.clusterByFilm(_, normalizer))
      .exists(cluster => cluster.exists(_._1 == primary) && cluster.exists(_._1 == key))
  }
}

private[movies] object ScrapeLanding {
  /** A venue slot its LISTING never wrote: every listing write sets `title` (the shown
   *  title), and the only other writer of a venue slot — a detail merge landing on a row
   *  that holds no slot of that venue — starts from an empty `SourceData`, so it has none.
   *  Such a slot is no evidence of what the venue lists. A shared detail group wrote one
   *  onto a bilety24 venue for every bilety24 film (fixed in 7b225cab2); the venue's next
   *  scrape drops them. */
  def isDetailOnly(slot: SourceData): Boolean = slot.title.isEmpty

  /** The cold-mirror sync's corpus read, while it keeps failing, is retried after this long
   *  at first, doubling to [[ColdMirrorRetryMax]]. */
  val ColdMirrorRetryMin: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(30, "seconds")
  val ColdMirrorRetryMax: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")
}
