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

Nothing in the pipeline reports this. The reaper logs a re-try, not an outcome,
and one film is far below any prune-burst threshold — only the
`kinowo.removal-audit` log names it (`reason=reconcile-prune`).

**Method, per flagged row:**

1. Compare the **cinema slots'** director and runtime against the resolved
   film's. The venues are the evidence; the resolved slot is the claim.
2. Search TMDB for **the title the venues publish**, not the title the row is
   keyed on.
3. Write the correct `tmdbId` and TMDB slot directly. **Never clear `tmdbId`
   to force a re-resolution** — see the traps below.

**Measured 2026-09-06:** prod contradictions fell 56 → 29 over ~14h on the 24h
phase spread. ~13 of the 29 confirmed rows had not been acted on yet at the
time of writing. Four rows had been left unresolved-and-invisible by the sweep
and were repaired by hand: `birdman` (194662), Mission: Impossible — Dead
Reckoning (575264), Goosebumps 2 (442062), G.B.H. (136775).

## Rows deliberately left, and why each needs a different tool

- **`kungfupanda4|2008`** (`kinowo_us`, ~17 screenings) is a **mixed** row: its
  cinema slots carry both "Kung Fu Panda" and "Kung Fu Panda 4", two directors
  and two runtimes, and it has bounced between the two films. Do **not** hand-pick
  one — either answer is wrong for half the screenings. It needs
  `MixedFilmSplitter` (`worker/.../services/movies/MixedFilmSplitter.scala`).
  Note that most same-film splits in this corpus are intentional; this is the
  opposite case, one row that should be two.
- **`it|1990`** (`kinowo_uk`, ~17 screenings) is the 1990 miniseries, which TMDB
  carries as **television**. A movie-only resolver cannot resolve it, so it is
  correctly unresolved and consequently invisible. Open question: accept that,
  or give TV a path.

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
