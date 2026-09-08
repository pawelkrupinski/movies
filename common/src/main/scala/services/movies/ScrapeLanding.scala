package services.movies

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord, Source, SourceData}
import play.api.Logging
import services.cinemas.CountryNames
import services.events.{CinemaMovieAdded, EventBus, StagingNewcomerDiverted}
import services.resolution.YearWindow
import tools.{PersonName, TextNormalization}

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
 * (the `$set`-diff for a resident row), `rekey` (a retitle) — under `withTitleLock`,
 * the per-title lock the cache shares with its settle paths. `residentCount` and
 * `rehydrate` serve the cold-mirror guard; `skippedUnreadable` is the cache's own
 * counter of writes it declined, which the landing increments for the same reason.
 */
private[movies] trait LandingStore {
  def normalizer: TitleNormalizer
  private[movies] def corpusIndex: CorpusIndexReader
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey
  private[services] def get(key: CacheKey): Option[MovieRecord]
  /** How many rows are resident — zero is the cold mirror the first scrape guards. */
  private[services] def residentCount: Long
  private[services] def storedChecked(key: CacheKey): (Option[MovieRecord], Boolean)
  private[services] def put(key: CacheKey, e: MovieRecord): Unit
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean
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
  enrichmentLanguage: java.util.Locale
) extends Logging {

  import store.{corpusIndex, normalizer}

  // The thresholds behind the two scrape-health guards live with the guards in
  // `ScrapeHealth`; this cache keeps only the one piece of state they need — how
  // many ticks running the depth guard has rejected a venue.
  private val depthRejections = scala.collection.concurrent.TrieMap.empty[String, Int]

  // Fires the cold-mirror sync at most once, on the FIRST scrape (see
  // `recordCinemaScrape`). One-shot so the sync can't re-trigger at an
  // arrival-order-dependent later scrape — the mirror only goes cold at boot, and a
  // later mirror-empty-while-repo-non-empty state is reachable only in a harness
  // with no change stream (its mirror lags the repo), where re-checking would make
  // the staging path order-dependent (StagingOrderDeterminismSpec).
  private val coldMirrorSyncArmed = new java.util.concurrent.atomic.AtomicBoolean(true)

  /** The slot key for one cinema's report of a film under a given shown title.
   *  Every cinema slot is keyed by `(cinema, sanitize(title))` so a venue can hold
   *  several title-variant slots of one film (the original + a dubbed/decorated
   *  edition) without collision — the read-model split renders a card each, and
   *  the same (cinema, title) ALWAYS produces the same key so a re-scrape / fold
   *  merges in place instead of duplicating. */
  private def cinemaSlotKey(cinema: Cinema, title: String): Source =
    CinemaShowing.keyFor(cinema, title, normalizer)

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
                         listingIsComplete: Boolean = true): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    // Empty `movies` is almost always a silent scraper failure (Cloudflare
    // challenge, parser regex mismatch, proxy 503, blank HTML), not a
    // cinema that's genuinely showing zero films right now. Without this
    // safeguard, the prune step below would wipe EVERY slot that cinema
    // holds across the cache, and the next successful tick would re-add them
    // — producing the visible "row appears and disappears" flicker. Bail out
    // and trust the slot data we have until the next non-empty tick.
    if (movies.isEmpty) return Seq.empty

    // DEPTH guard — the same "trust what we have" bail as above, on the axis
    // neither that check nor the breadth guard below can see (a chunked fetch that
    // lost whole dates). The verdict is `ScrapeHealth.depth`'s; this keeps the count
    // of consecutive rejections per venue, which is the only state involved.
    //
    // Count via `slotShowtimeCount`, NOT `showtimes.size`: under the read-split every
    // resident slot has been through `stripForCache` and carries `Nil` showtimes, so a
    // direct `.size` reads 0 for every cinema and the floor never engages — which made
    // this guard dead code in production while the specs, wiring no screenings
    // repository, kept their lists resident and passed regardless.
    val knownCinemaShowtimes = corpusIndex.slotsOf(cinema).map { case (_, _, sd) =>
      ShowtimesDigest.slotShowtimeCount(sd) }.sum
    val batchShowtimes       = movies.iterator.map(_.showtimes.size).sum
    ScrapeHealth.depth(knownCinemaShowtimes, batchShowtimes, depthRejections.getOrElse(cinema.displayName, 0)) match {
      case ScrapeHealth.Depth.Reject(consecutive) =>
        depthRejections.put(cinema.displayName, consecutive)
        RemovalAudit.scrapeDepthGuarded(cinema.displayName, batchShowtimes, knownCinemaShowtimes, consecutive)
        return Seq.empty
      case ScrapeHealth.Depth.AcceptDegraded(consecutive) =>
        // Sustained across several ticks — stop treating it as a bad fetch and let
        // the smaller board land, rather than serving showtimes that no longer exist.
        RemovalAudit.scrapeDepthAccepted(cinema.displayName, batchShowtimes, knownCinemaShowtimes, consecutive)
        depthRejections.remove(cinema.displayName)
      case ScrapeHealth.Depth.Healthy =>
        depthRejections.remove(cinema.displayName)
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
    if (staging.isDefined && coldMirrorSyncArmed.getAndSet(false)
        && store.residentCount == 0 && repository.findAll().nonEmpty) store.rehydrate()

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
    val knownCinemaSlots   = corpusIndex.slotsOf(cinema).size
    val scrapeLooksPartial = ScrapeHealth.looksPartial(knownCinemaSlots, deduped.size, listingIsComplete)
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
    def keyHoldingCinemaSlot(norm: String): Option[CacheKey] = {
      val keys = corpusIndex.keysForCinemaSlot(cinema, norm)
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
     *  isn't clobbered. Shared by the stale-slot prune below and the
     *  same-slot-on-two-rows cleanup in the write loop. */
    def dropCinemaSlots(key: CacheKey, slotKeysOf: MovieRecord => Set[Source]): Unit =
      store.putIfPresent(key, cur => {
        val slotKeys = slotKeysOf(cur)
        val captured = slotKeys.iterator.flatMap { s =>
          cur.data.get(s).flatMap(_.synopsis).filter(_.nonEmpty).map(s -> _)
        }.toMap
        val kept = cur.data -- slotKeys
        cur.copy(
          data             = kept -- orphanedChainDetail(kept),
          retainedSynopses = MovieRecordMerge.mergeRetainedSynopses(cur.retainedSynopses, captured))
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
    // Titles diverted into staging for the FIRST time this cinema (no prior row) —
    // the newcomers whose initial step StagingReaper should kick off an event,
    // rather than the periodic backstop. A re-divert of an already-incubating film
    // (prior row present) is NOT collected, so we don't republish every tick.
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
          val slot          = buildCinemaSlot(cm, displayTitle, priorSlot, effectiveYear)
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
          if (!priorStagingRows.contains(norm)) newlyDiverted += displayTitle
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
          val key = concludedKeyFor(primary, cm.movie.runtimeMinutes.filter(_ > 0), cinema).getOrElse {
            redirectToExistingVariant(primary) match {
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
                val existingResolved = store.get(existingKey).exists(_.tmdbId.isDefined)
                if (existingResolved) existingKey
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
              case None => keyHoldingCinemaSlot(norm).orElse(landing.fallbackKey).getOrElse(primary)
            }
          }
          val existingOpt   = store.get(key)
          val existing      = existingOpt.getOrElse(MovieRecord())
          val priorSlot     = existing.data.get(cinemaSlotKey(cinema, displayTitle))
          val effectiveYear = cm.movie.releaseYear.orElse(priorSlot.flatMap(_.releaseYear))
          val slot          = buildCinemaSlot(cm, displayTitle, priorSlot, effectiveYear)
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
              // Its RESULT, not `true`. `putIfPresent` answers false when the key is no
              // longer in Caffeine by the time it computes — a concurrent `rekey` of a
              // DIFFERENT title invalidates keys without holding this title's lock — and
              // assuming the write landed is what lets the move below strip a slot that
              // was never replaced. The gate is only worth having if it reads the write.
              store.putIfPresent(key, current => current.copy(data = current.data + (slotKey -> slot)))
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
                  false
                case (row, true) =>
                  val base = row.getOrElse(MovieRecord())
                  store.put(key, base.copy(data = base.data + (slotKey -> slot)))
                  true
              }
          }
          // This tick just decided which film this (cinema, title) belongs to, so
          // any OTHER row still holding the same slot is stale by construction —
          // drop it there. The write above lands the slot; without this it only
          // ever COPIED, and the sole removal was the end-of-tick prune, which
          // stands down on a partial scrape and only sees rows this cinema's own
          // index lists. That is how "Zaproszenie" served the Kinepolis 21:40 and
          // Kino Malta showtimes under BOTH `zaproszenie|2026` and
          // `zaproszenie|1986` — one screening, two films, on the live site.
          // Unlike the prune this is safe on a degraded tick: it removes a slot
          // only because we just OBSERVED this venue listing this title, never
          // because a fetch failed to mention it.
          // The slots to drop are read off the OTHER row rather than assumed to be
          // `slotKey`: the same venue's slot for this film can sit there under a
          // different Source spelling (a bare `Cinema` from an older write, a
          // `CinemaShowing` for a decorated edition), and dropping only `slotKey`
          // would miss exactly the stranded copies this is here to clear.
          // Gated on the write above: see `landed`.
          if (landed) (corpusIndex.keysForCinemaSlot(cinema, norm) - key).foreach { other =>
            dropCinemaSlots(other, _.data.collect {
              case (src, sd) if Source.cinemaOf(src).contains(cinema) &&
                                sd.title.exists(t => normalizer.sanitize(t) == norm) => src
            }.toSet)
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
    if (scrapeLooksPartial)
      // The guard skipped the prune — log the decision (and what it spared) so a
      // degraded-tick episode is on the record even though nothing was removed.
      RemovalAudit.scrapePruneSkipped(cinema.displayName, batchFilms = deduped.size,
        knownSlots = knownCinemaSlots, reason = "partial-scrape-guard")
    else {
      // This cinema's slots, straight from the index — the seventh full-corpus walk
      // this method used to make per venue, and the one that ran on every healthy tick.
      // Touched by reference, OR named by a listing whose write was skipped — see
      // `listedButNotWritten`. A slot in neither set is one the venue genuinely
      // stopped listing, which is the only thing this prune is entitled to act on.
      val spared = listedButNotWritten.toSet
      def wasListed(sd: SourceData): Boolean =
        sd.title.exists(t => spared.contains(normalizer.sanitize(t)))
      val toPrune = corpusIndex.slotsOf(cinema).iterator
        .collect { case (k, s, sd) if !touchedSlots.contains(sd) && !wasListed(sd) => k -> s }
        .toList.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toList
      toPrune.foreach { case (k, staleKeys) => dropCinemaSlots(k, _ => staleKeys) }
      // The batch signal the served-films sawtooth needed: which cinema dropped how
      // many slots off how many still-known films this tick.
      RemovalAudit.scrapePruned(cinema.displayName, films = toPrune.size,
        slots = toPrune.iterator.map(_._2.size).sum,
        sampleFilmIds = toPrune.map { case (k, _) => s"${k.cleanTitle} (${k.year.getOrElse("—")})" },
        reason = "scrape-prune")
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
    cinema:         Cinema
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
    val keyMatches = corpusIndex.entriesFor(norm).collect { case (k, e) if e.tmdbConcluded => k }
    // `keysForAlias` is already gated on concluded-AND-bare (the index's own predicate),
    // so the only arm left to apply is the partition's: a key that matches by its own
    // normalised form belongs to `keyMatches`, never here.
    val aliasOnly  = corpusIndex.keysForAlias(norm).filterNot(_.normalized == norm).toSeq
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
    nearest(keyMatches).orElse(nearest(aliasOnly))
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

  /** Build one cinema's `SourceData` slot for a scraped film — shared by the
   *  `movies` write and the staging divert so both apply the same rules:
   *    - two-stage detail preservation: a deferred cinema (e.g. Kino Muza) ships
   *      `posterUrl`/`synopsis`/`trailerUrl` AND the detail fields
   *      (cast/director/runtime/originalTitle/countries/genres) as None/empty on
   *      the listing tick — keep whatever the detail refresher already wrote
   *      (`priorSlot` carry-forward); else a listing tick WIPES the enrichment;
   *    - year fallback (`effectiveYear`): keep the prior year when a tick drops it
   *      (Helios' REST year flakes), treating a dropped year as loss not a change;
   *    - cast/director cased for display (`displayNames`: ALL CAPS down for
   *      Cinema City, all-lowercase up for Flicks), runtime-zero squashed to
   *      None, and country names canonicalised. */
  private def buildCinemaSlot(
    cm:            CinemaMovie,
    displayTitle:  String,
    priorSlot:     Option[SourceData],
    effectiveYear: Option[Int]
  ): SourceData =
    SourceData(
      title          = Some(displayTitle),
      // Verbatim upstream title, kept so the merge key is re-derivable when the
      // per-cinema rules change. A rule-driven client carries the pre-strip
      // string in `movie.rawTitle`; others leave it None and `title` is raw.
      rawTitle       = cm.movie.rawTitle.orElse(Some(cm.movie.title)),
      originalTitle  = cm.movie.originalTitle.orElse(priorSlot.flatMap(_.originalTitle)),
      // Collapse a blurb the cinema CMS pasted N× into one description field
      // (Bilety24's Kino Piast shipped the "Ojczyzna" synopsis 9× glued together)
      // at the ingestion boundary, so we never store the duplicate — not just hide
      // it at read time. See tools.SynopsisMarkdown.collapseRepeats.
      // Intern so a film's N cinema slots carrying the same chain-wide blurb share ONE
      // String instead of N byte-identical copies (see StringPool). Same applies to the
      // cast/director/country/genre fields below — only the FRESH branch needs interning;
      // the prior-slot carry-forward already holds interned instances.
      synopsis       = cm.synopsis.map(tools.SynopsisMarkdown.collapseRepeats).map(StringPool.canonical).orElse(priorSlot.flatMap(_.synopsis)),
      // Detail fields (cast/director/runtime/originalTitle/countries/genres) are
      // filled by the deferred EnrichDetails merge; a listing-only cinema's re-scrape
      // carries none of them. Carry the prior slot's values forward when the fresh
      // listing lacks them — exactly as synopsis/poster/trailer above — so a listing
      // tick doesn't WIPE the enrichment (which EnrichDetails then re-adds, flapping
      // the row + doubling its change-stream writes). A listing that DOES carry the
      // field still wins, matching FilmDetail.mergeInto's "fill only if empty" rule.
      cast           = if (cm.cast.nonEmpty) displayNames(cm.cast)
                       else priorSlot.map(_.cast).getOrElse(Seq.empty),
      director       = if (cm.director.nonEmpty) displayNames(cm.director)
                       else priorSlot.map(_.director).getOrElse(Seq.empty),
      runtimeMinutes = cm.movie.runtimeMinutes.filter(_ > 0).orElse(priorSlot.flatMap(_.runtimeMinutes)),
      releaseYear    = effectiveYear,
      countries      = { val cs = StringPool.canonicalAll(cm.movie.countries.map(c => CountryNames.canonical(c, enrichmentLanguage)).distinct)
                         if (cs.nonEmpty) cs else priorSlot.map(_.countries).getOrElse(Seq.empty) },
      genres         = if (cm.movie.genres.nonEmpty) StringPool.canonicalAll(cm.movie.genres)
                       else priorSlot.map(_.genres).getOrElse(Seq.empty),
      // Interned like the fields above, and for the same reason: a film's poster,
      // film page and trailer are ONE url repeated across every cinema showing it.
      // Highest-yield strings in the corpus by some margin — the 2026-07-27 UK heap
      // dump held 136,064 poster-url instances for 1,896 distinct values (71.8x) and
      // 138,199 film-page instances for 2,004 (69.0x). `Showtime.bookingUrl` is
      // deliberately NOT interned: it is per-screening, only 1.6x repeated
      // (182,719 -> 116,571 distinct), so pooling it would evict this whole
      // low-cardinality vocabulary for almost no saving.
      posterUrl      = cm.posterUrl.map(StringPool.canonical).orElse(priorSlot.flatMap(_.posterUrl)),
      filmUrl        = cm.filmUrl.map(StringPool.canonical),
      trailerUrl     = cm.trailerUrl.map(StringPool.canonical).orElse(priorSlot.flatMap(_.trailerUrl)),
      // Canonical order so a reorder-only re-scrape stores a byte-identical slot and
      // the write-through guard skips it. Past showings the fresh scrape drops are NOT
      // retained: under the index-only cache the resident `priorSlot` is stripped (Nil
      // showtimes + a digest), so there's nothing to retain FROM, and re-stitching a
      // film's screenings from Mongo per scrape would cost far more read I/O than the
      // one deferred write it would save. Dropping a just-passed showtime is
      // display-neutral (the web filters past showtimes at render). See
      // MovieRecordMerge.sortShowtimes.
      showtimes      = MovieRecordMerge.sortShowtimes(cm.showtimes),
      // Carry the certificate forward on a listing-only re-scrape, like the detail
      // fields above, so a tick that lacks it doesn't wipe a value the detail merge added.
      ageRating      = cm.ageRating.map(StringPool.canonical).orElse(priorSlot.flatMap(_.ageRating))
    )

  /** Cast/crew names as the display layer needs them, for the two casings a
   *  cinema source invents: SHOUTED credits are title-cased
   *  ([[TextNormalization.titleCaseIfAllCaps]] — Cinema City's "KARL URBAN") and
   *  all-lowercase ones are capitalised ([[PersonName]] — Flicks' Anglophone
   *  venues emit `content_cast` as "christoph waltz"). The two rules are
   *  disjoint by construction — each returns its input untouched unless the
   *  string is entirely in the other's case — so a properly-cased name from
   *  TMDB, IMDb or any of the Polish scrapers passes through both unchanged, and
   *  the order they compose in doesn't matter.
   *
   *  Interned last, so the pool holds the canonical DISPLAY spelling rather than
   *  a separate instance per source casing. */
  private def displayNames(names: Seq[String]): Seq[String] =
    StringPool.canonicalAll(names.map(TextNormalization.titleCaseIfAllCaps).map(PersonName.capitalized))

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
