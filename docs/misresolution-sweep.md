# Mis-resolution sweep

Durable cross-run log for the sweep that finds films resolved to the **wrong
TMDB record** and re-resolves them. A **mis-resolution** is a `movies` row whose
own cinemas contradict its resolved film: the venues publish a director or a
runtime that belongs to a different picture. It is not the same failure as an
*unresolved* row (no `tmdbId` at all) — a mis-resolved row looks healthy on the
site and shows the wrong poster, cast and ratings.

Read this before re-running the sweep, so you don't re-diagnose rows already
settled, and — the load-bearing reason — so you check every outcome
**film by film**. The sweep is not self-checking; see below.

## The chain, and why each piece exists

Live in prod and working. Do not redesign it.

1. **Detect** — `CinemaCorroboration.contradiction`
   (`common/.../services/movies/CinemaCorroboration.scala`) flags a row whose
   venues disagree with its resolved film, and
   `resolvedOnWeakerEvidenceThanAvailable` flags one concluded from thinner
   evidence than the row now carries.
2. **Confirm** — `CrewConfirmation`
   (`worker/.../services/tasks/CrewConfirmation.scala`) asks TMDB who actually
   made the film before anything acts. A venue crediting the film's *other*
   director, or a pseudonym, is not a wrong film.
3. **Act** — `UnresolvedTmdbReaper`
   (`worker/.../services/tasks/UnresolvedTmdbReaper.scala`) spends the
   re-resolution, phase-spread over its 24h window. Wired in `WorkerWiring`.
   Its `confirmContradiction` **default is deliberately runtime-only**: a
   construction site that forgets the wiring degrades to doing *less*, never to
   force-re-resolving every unconfirmed name disagreement.

The **structural causes** were fixed, not just the rows that showed them:
`TmdbBasis` records what a conclusion was reached from; an uncorroborated key
year no longer counts as evidence; a year a venue wrote into its own title
outranks it; cinema years outrank the all-slot fallback (which includes the
Tmdb slot's own year, i.e. the row arguing for itself).

`ReadModelFilmPruneBurst` alerts on the mass version of the failure below —
more than 30% of a country's corpus pruned in an hour. The measured
worst-healthy baseline was 16.9%.

## The sweep is not self-checking — check every outcome film by film

This is the part that needs a human (or an agent that reads this file).

The sweep can correctly **reject** a wrong film and then **fail to find the
right one**. The row is left unresolved, so it falls below
`MovieRecord.readyToProject`, so `ReadModelProjector` prunes its card — the
film goes **invisible**. For a film with screenings that is *worse* than being
wrong: a wrong poster still lists the showtimes, a pruned card 404s.

**The gauge that finds them:**
`kinowo_worker_corpus_movies{subset="unresolved_with_showtimes"}` — rows that
fail `readyToProject` while their cinemas still list an upcoming showtime, i.e.
exactly the population the projector prunes. It rides the shared
`WorkerCorpusScan` pass, so it costs no reads of its own.

Note the pairing: `subset="misresolved"` counts the sweep's **input** (rows whose
venues contradict them), and `unresolved_with_showtimes` counts the **outcome**
where it rejected a wrong film and found no right one. Every other census gauge
gates on `readyToProject`, so before this series existed these rows dropped out
of all of them without being counted anywhere — the reaper logs a re-try, not an
outcome, one film is far below any prune-burst threshold, and only the
`kinowo.removal-audit` log named it (`reason=reconcile-prune`).

**Alerted by `ReadModelFilmsInvisibleWithScreenings`** (`> 0` for 2h, warning) in
`infra/nix/files/monitoring/rules/read-model-projection.rules`. A plain `> 0` is
right because the measured baseline IS zero — see the audit below — and newcomers
do not trip it either: an unresolved new film incubates in `pending_movies` and
reaches `movies` already concluded. The `for: 2h` rides out a re-key or
title-rule wave without letting a stuck row hide, since the reaper's own retry
period is 24h.

**Method, per flagged row:**

1. Compare the **cinema slots'** director and runtime against the resolved
   film's. The venues are the evidence; the resolved slot is the claim.
2. Search TMDB for **the title the venues publish**, not the title the row is
   keyed on.
3. Write the correct `tmdbId` and TMDB slot directly. **Never clear `tmdbId`
   to force a re-resolution** — see the traps below.

**Audit completed 2026-09-06 — the answer is ZERO.** Across all five prod
databases there is currently no row that fails `readyToProject` while carrying an
upcoming showtime. The corpus holds 504 unresolved rows (PL 245, UK 103, US 100,
DE 52, ES 4), and every one of them has concluded `tmdbNoMatch` with its cinema
detail done — so they all satisfy `readyToProject` and are visible, carrying
venue-supplied title, runtime and director instead of TMDB's.

That is the correction worth carrying forward: **unresolved does NOT mean
invisible.** `readyToProject` is `tmdbConcluded && (tmdbId.isDefined ||
detailDone)`, so a definitive no-match with detail done publishes fine. Only a row
that is unresolved AND un-concluded (or still detail-pending) disappears. Querying
`tmdbId: null` alone over-counts the invisible population by ~500 rows; the whole
of the "event cinema" catalogue — Met Opera, Royal Ballet & Opera, NT Live — lives
there legitimately and is on the site.

Earlier the same day, prod contradictions fell 56 → 29 over ~14h on the 24h phase
spread, and four rows the sweep had left unresolved were repaired by hand:
`birdman` (194662), Mission: Impossible — Dead Reckoning (575264), Goosebumps 2
(442062), G.B.H. (136775).

**How to re-run the audit.** Reach prod Mongo as `docs/white-cinema-investigations.md`
describes, then per country database intersect two sets: rows in `movies` matching
`tmdbId` absent AND (`tmdbNoMatch` not true OR `detailPending` true), against
`filmId`s in `screenings` having a `showtimes.dateTime` in the future. Slots live
in `movie_slots` (keyed by `filmId`) since the storage split — `movies` documents
carry no `data`/`sourceData` map any more, which is the trap that makes a naive
slot query return nothing.

## Rows deliberately left, and why each needs a different tool

- **`kungfupanda4|2008`** (`kinowo_us`, 112 upcoming screenings) is a genuine
  **mixed** row, and it is VISIBLE — showing the wrong film to most of its
  venues. It resolves to `tmdbId=9502`, Kung Fu Panda (2008, 90 min, Osborne &
  Stevenson). One venue (Bear Tooth Theatrepub) really is screening that film;
  **sixteen** Galaxy/Hangar venues are screening "Kung Fu Panda 4" (94 min, Joel
  Crawford). So 16 of 17 venues get the wrong poster, cast and ratings. Do not
  hand-pick — either single answer is wrong for one side.

  **Why `MixedFilmSplitter` never fires on it**, confirmed against the live row:
  `MixedFilmDetector.identityGroups` filters `cinemaSlots` down to those with a
  non-empty `originalTitle`, and **no US cinema slot carries one at all** (the
  Flicks-sourced slots have `title` only), so the row yields zero identity groups
  and `split` returns empty. Falling back to `title` is not enough on its own
  either: `titlesDiffer` demands DISJOINT word sets, and "Kung Fu Panda" is a
  subset of "Kung Fu Panda 4" — a base film and its numbered sequel never differ
  under that test. Two independent blockers, and loosening either one risks
  over-splitting a corpus where `splitsSoFar` is asserted to stay zero. Not
  attempted here for that reason.
- **`it|1990`** (`kinowo_uk`, 33 upcoming screenings) — **the earlier claim that
  this is invisible was WRONG.** The row carries `tmdbNoMatch=true` with detail
  done, so `readyToProject` holds and it IS in `web_movies` and on the site,
  listed from what the venues published (one of them supplies "It (1990)", 168
  min, Tommy Lee Wallace). TMDB carrying the 1990 miniseries as television costs
  it TMDB's poster/synopsis/ratings, not its visibility. There is no invisibility
  decision to make; giving TV a path is a quality improvement, not a fix.

## What provably cannot be closed at this layer

- **The Rotten Tomatoes modifier/year shape.** `"Top Gun"` against
  `"Top Gun - Re-Release" (2024)` must be ACCEPTED; `"Lalka"` against
  `"Lalka - Restored" (2026)` must be REJECTED. The two search results are
  identical in shape and only the *page* could tell them apart —
  `/m/lalka_1969` publishes no year either. Both cases are pinned by tests.
  Read the comment above the modifier fallback in
  `worker/.../services/enrichment/RottenTomatoesClient.scala` before touching it;
  anyone tightening the rule will see immediately which case they broke.
- **The end-of-tick prune still removes a slot whose write was skipped**
  (`touchedSlots` holds a record that was never written — `MovieCache`, the
  scrape-prune block). Pre-existing and older than the slot move; deliberately
  not fixed, because the opposite failure — stale slots never pruned — is worse.

## Traps this sweep has already paid for

- **Never clear `tmdbId` to force a re-resolution on a row with screenings.**
  The card is pruned within minutes and `UnresolvedTmdbReaper` will not revisit
  that row for up to **24h** (once per row per period, at a phase hashed from
  its resolve dedup key — there is no state to clear and no way to bring a key
  forward; a restart does not help). Write the correct resolution instead.
- **Verify a review finding before acting on it.** Acting on one unverified
  suggestion ("shorten Robert Downey Jr.") actively *regressed* behaviour: minus
  the suffix it is his father, whom TMDB ranks first because he is a director.
- **A guard you cannot reach may still be load-bearing.** One was deleted here
  on an unreachability argument that missed an arm, and had to come back.
- **Specs default to `ResolutionCache.passthrough`**, which never memoises, so
  no spec exercises a resolution-cache *hit* unless it wires a memoising cache.
  A whole bug class was invisible until one did.

## Test-layer notes for anyone working this area

- **`itAll` is flaky under parallel load on a laptop** — two different specs
  have been seen failing, each passing alone and on a quiet machine. Run a
  baseline at `HEAD` before believing your change broke it.
- **Do not add a `Thread.sleep` to wait for a change stream** in
  `worker/src/it/scala/MovieRepositoryIntegrationSpec.scala`. A fixed nap is a
  guess at the cursor-open window, and it makes a slow runner look exactly like
  the bug the spec exists to catch. That file ships `awaitStreamLive` and, as of
  `e0f6637b2` (2026-09-06), every stream-establish nap in it has been converted.
  The longer sleeps that remain are a **different, legitimate** pattern —
  proving that no *second* event arrives. Leave those.
