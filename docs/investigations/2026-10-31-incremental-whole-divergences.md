# Incremental vs whole: the eight divergence points (2026-10-31)

The 2026-10-31 structure review listed eight places where the incremental model (`IncrementalResolver`, `LiveCorpus`,
`LiveProjectionIndex`, the agreement stage's caches) and a whole resolve could disagree by mechanism alone. The owner
called them bugs under the no-stickiness rule: every take must be what a resolve with all the data from the start would
give. So each one is fixed so that the two sides agree, with no new resolver rule. Each fix is one change, carries a
property in `IncrementalWholeDivergenceSpec` that fails before and passes after, and comes with a CI convergence
measurement of how many takes move, per country.

| # | Point | Status | Property | Takes moved |
|---|---|---|---|---|
| 1 | One listing per key: whole keeps the smallest by the total order; a batch kept the last seen | **fixed** (`IncrementalResolver.onePerKey`, batch and restore) | twin-key batch, either order (failed 12/12 seeds before) | 0 (all five countries) |
| 2 | Corpus context built twice (`CorpusContext.of` / `LiveCorpus`) | no divergence found | random-sequence equivalence over 24 seeds holds; fallback records are read alike on both sides | — |
| 3 | Production's learned decorations never equivalence-checked | no divergence | decorated random sequence, 12 seeds (passes) | — |
| 4 | Rules version not covering StageWorks / FilmCuts | false positive: `identity-rules-sources.txt` lists every stage-works, dances and film-cuts table | — | — |
| 5 | Decision trace kept on one path only (`DecisionTrace.kept`) | no divergence | `IncrementalResolverSpec` "hand a stage after it what a resolve … hands it" | — |
| 6 | Projection index built twice (`IdentityProjectionPlan.index` / `LiveProjectionIndex`) | pending: files in flight (step6) | — | — |
| 7 | The agreement's per-cluster resolve (`resolveScored`) ignores pins | pending: AgreementStage in flight (settle-on-give-up) | — | — |
| 8 | Agreement memo validation budgeted (a stale verdict found eventually) | pending: AgreementStage in flight | — | — |

## 1. One listing per key

The worker gives the model one listing per key per venue (`ModelRead.corpus`). Its page-wait releases never share a key
with what the same drain admits: a waiting listing's venue scrape replaces its waiting set. So production should move
no take. Measured with the full measure (run 41241555033, base 73f1dcfcb, head ac921f2de):
0 takes lost, changed or gained, model and final stage, in PL (10,804 listings), DE (34,237), UK (36,649), US (97,570)
and ES (5,937). LANDABLE.
