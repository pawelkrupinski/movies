# 2026-11-04 — FamilyReplay: a family's population lattice replayed from its slice — CLOSED 2026-11-05, superseded (head `d3a86b82abb2`)

- **What:** `worker/src/test/scala/scripts/FamilyReplay.scala` (branch `family-replay`, prototype, not wired): a
  family plus its read set replayed alone over its population lattice (subsets, levels, orders), model and final take.
- **Result:** reproduced 15 of the 16 known flips still on main (Titans x5, Buddies -- 3 listings, which a model-only
  walk misses --, Gala Durham, Oxide Ghosts x2, Hen Poole/Keswick, DCA); found the corpus-wide context reads
  (`CorpusContext` houses, segmentSpread, candidate venue titles) and the agreement's out-of-cluster reads the slice
  must carry. Report: scratchpad `FAMILY-REPLAY.md`.
- **Why closed (review 2026-11-05):** everything it prototyped landed in production shape: `FamilyIndex` (members,
  context reads by value, answers, rules; c5d658f63839), `FamilyFinalReads` (a1618510c44e), `FamilyConvergence`, the
  per-family lattice check with a closure-keyed cache (41689c87c4bf), and `MirrorRecompute` (519e53148c87); see
  `docs/design/identity-no-drift.md` "One family index". Nothing in the prototype is not covered there; the parking
  date (11-12) waited on nothing.
- **Code:** local tag `experiment/family-replay` (`d3a86b82abb2`); branch `family-replay` (1 commit ahead, unmerged).
