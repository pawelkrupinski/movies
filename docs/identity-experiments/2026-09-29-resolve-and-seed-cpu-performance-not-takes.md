# 2026-09-29/30 — resolve and seed CPU (performance, not takes)

Measured through the CI measure (`scripts/identity-measure`) with JFR, each against a same-batch base (runner noise is
±25%); decisions had to stay byte-identical. The speedups that landed are in git history.

## Cross-resolve, content-keyed LRU of node scores ("score cache")
- **Tried:** an LRU of node scores kept across resolves, keyed by content.
- **Result:** UK heap 53 → 319 MB, US 106 → 559 MB at ~6–7k entries, ~25% hit rate; UK got slower.
- **Why stopped:** venue churn moves family pools, so the keys change between resolves. Caching Films across regions
  is the same trap: cheapen the derivation instead. (The Lalka family is one work by design; don't split its keys to
  shrink it.) Do not retry as is.
- **Code:** branch `wip/score-memo-rejected` (`a8fdf8dfd`) — neither the branch nor the commit is in this clone, and
  there is no `experiment/*` tag.

## Parallel region take-up ("parallel-regions")
- **Tried:** taking up regions on two threads — redone safely 2026-09-30 with its own `ownPrefetch()` and a
  `RegionPool` of at most 2 in flight, admitted by `tools.HeapHeadroom.tenuredBelow(0.6)`. An earlier form
  (`cc3c66dae`) had been reverted (`709c3bdcc`) after a `held.clear` NPE and US OOM kills.
- **Result:** US replay decisions byte-identical; seed 12.4 s vs 12.7 s — no gain. JFR: both region threads on-CPU
  throughout, 14 monitor events, so not a lock; each region costs ~2× CPU when paired on the 4-hardware-thread runner.
- **Why stopped:** prod take-up (~250 s) runs while `k3s-worker-1` is at 7.9/8 cores with five workers booting, so a
  second thread only competes. Don't re-push without a measured wall-clock gain.
- **Code:** branch `perf/parallel-regions-scoped`, never pushed and not in this clone; no `experiment/*` tag.
- **2026-11-05, landed for the take-up only:** the race the earlier forms hit was TrackedLookups' (two regions asking
  one question at once each asked it and dropped the other's reads; one region's prefetch released another's held
  documents). Fixed (one ask per question in flight, per-thread prefetches, the store handed one prefetch at a time),
  then `IncrementalResolver.restore` resolves its regions on `takeUpThreads` (worker: min(4, cores)); later drains stay
  serial. Measure run 41964603288: every take bucket 0 in all five countries, drift and convergence rows green; the
  take-up's non-context wall DE ~57 -> 29 s, US ~81 -> 46 s, US next-day ~66 -> 48 s (means of three main runs).
- **Next angle:** the levers are boot CPU contention (stagger rollouts) and GC (prod UK was GC-bound: SerialGC full
  GCs 28/min, 0.72 of wall time — the live set during a full take-up), not scoring CPU.

## Memoising `searchGroups` / `Qualifiers.split`
- **Tried:** a memo for `searchGroups` / `Qualifiers.split`, which showed 9.4% / 6.8% of JFR samples.
- **Result:** PL flat, DE +30%; the hash-map overhead ≈ the work saved.
- **Why stopped:** a JFR share that does not turn into a measured delta is noise.
- **Code:** not kept; no `experiment/*` tag.
