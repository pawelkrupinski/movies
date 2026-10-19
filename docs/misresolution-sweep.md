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

## What is left of the chain

> **Historical.** The sweep's confirm and act steps (`CrewConfirmation`, `UnresolvedTmdbReaper`) were deleted with the
> old film pipeline on 2026-10-04 (43e8ed84c). The detect step survives only as a gauge
> (`CinemaCorroboration.contradicts` → `WorkerCorpusMetrics`' misresolved count); the identity
> resolver's `Acceptance` refuses a candidate a published fact contradicts, and a
> wrong film is fixed by a pin (`docs/design/identity-resolver.md` §13). What follows is what still
> applies: the gauges, the method and the traps.

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
exactly the population the projector prunes. `CorpusCensus` counts it from the
worker's cache, so it costs no reads of its own.

Note the pairing: `subset="misresolved"` counts the sweep's **input** (rows whose
venues contradict them), and `unresolved_with_showtimes` counts the **outcome**
where it rejected a wrong film and found no right one. Every other census gauge
gates on `readyToProject`, so before this series existed these rows dropped out
of all of them without being counted anywhere — one film is far below any prune-burst threshold, and only the
`kinowo.removal-audit` log named it (`reason=reconcile-prune`).

**Alerted by `ReadModelFilmsInvisibleWithScreenings`** (`> 0` for 2h, warning) in
`infra/nix/files/monitoring/rules/read-model-projection.rules`. A plain `> 0` is
right because the measured baseline (audit of 2026-09-06, all five countries) IS zero. The `for: 2h` rides out a re-key or
title-rule wave without letting a stuck row hide.

**Method, per flagged row:**

1. Compare the **cinema slots'** director and runtime against the resolved
   film's. The venues are the evidence; the resolved slot is the claim.
2. Search TMDB for **the title the venues publish**, not the title the row is
   keyed on.
3. Fix the film with a pin (`docs/design/identity-resolver.md` §13), not by
   clearing `tmdbId`: a cleared row fails `readyToProject` and its card is pruned.

## What provably cannot be closed at this layer

- **The Rotten Tomatoes modifier/year shape.** `"Top Gun"` against
  `"Top Gun - Re-Release" (2024)` must be ACCEPTED; `"Lalka"` against
  `"Lalka - Restored" (2026)` must be REJECTED. The two search results are
  identical in shape and only the *page* could tell them apart —
  `/m/lalka_1969` publishes no year either. Both cases are pinned by tests.
  Read the comment above the modifier fallback in
  `worker/.../services/enrichment/RottenTomatoesClient.scala` before touching it;
  anyone tightening the rule will see immediately which case they broke.
## Traps this sweep has already paid for

- **Verify a review finding before acting on it.** Acting on one unverified
  suggestion ("shorten Robert Downey Jr.") actively *regressed* behaviour: minus
  the suffix it is his father, whom TMDB ranks first because he is a director.
- **A guard you cannot reach may still be load-bearing.** One was deleted here
  on an unreachability argument that missed an arm, and had to come back.
- **Specs default to `ResolutionCache.passthrough`**, which never memoises, so
  no spec exercises a resolution-cache *hit* unless it wires a memoising cache.
  A whole bug class was invisible until one did.

## Test-layer notes for anyone working this area

- **`itAll` has failed under parallel load on a laptop** — two different specs
  have been seen failing, each passing alone and on a quiet machine. Run a
  baseline at `HEAD` to learn whether your change broke it; either way the
  failure is a flake to investigate and fix (`scripts/it-local.sh` gives a run
  its own mongod), never one to rerun to green.
- **Do not add a `Thread.sleep` to wait for a change stream** in
  `worker/src/it/scala/MovieRepositoryIntegrationSpec.scala`. A fixed nap is a
  guess at the cursor-open window, and it makes a slow runner look exactly like
  the bug the spec exists to catch. That file ships `awaitStreamLive` and, as of
  `e0f6637b2` (2026-09-06), every stream-establish nap in it has been converted.
  The longer sleeps that remain are a **different, legitimate** pattern —
  proving that no *second* event arrives. Leave those.
