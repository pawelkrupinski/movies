# 2026-11-05 — FamilyCounterfactual: predict identity flips ahead of time from the family dependency index — DROPPED, caught 0 of 6 reversals (head `41f7beb2c501`)

- **What:** `services.identity.FamilyCounterfactual`, an offline single-perturbation predictor over a mirror corpus:
  for each listing, perturb one population read the family dependency index says its take depends on and re-resolve
  its family; a take that would change (`canFlip`) is a predicted flip. Built on the dep-index branch (FamilyIndex /
  FamilyConvergence); single-threaded because `StoredTmdbLookups` races (NPE) under concurrent family resolves.
- **Measured:** offline only (mirror :28017, read-only; no CI, no resolver change). `identity_history` spans 7 days
  (2026-10-29..11-05), so fit on days 1-5, test on days 6-7. Counterfactual runs: DE 11-03 (228 of 29358 listings
  `canFlip`, 24 min), DE 11-04 (112/29917), UK 11-03 (89/31704, 44 min); UK 11-04 stopped. Week reversals (non-deploy):
  DE 116, UK 96, ES 1, US 2 = 215; 151 (70%) are a model-stage loss restored by a later stage 5-7 s later.
- **Result:** ahead of time 0 of 6 reversals caught, 218 listings flagged per day. At the change, 0 of 26: UK 11-03's
  21 reversals were BTS listings that arrived at 11:45, after the snapshot, so no snapshot-based predictor sees them.
  The listing-day baselines were no better: departures 0/24 (258 flags/day); a fitted risk score top 5% recall 0.62 at
  precision 0.001 (9.6k flags/day), unstable between fits (0.62 vs 0.17); "flipped in the last 7 days" recall 0.17,
  875 flags/day.
- **Why dropped:** reversals are mostly a transient inside one projection (a stage's loss restored seconds later),
  not a standing population-dependent take a perturbation exposes; the tool flags hundreds a day and hits none. Costly
  too (24-44 min per country-day, one thread).
- **Angle that worked instead:** event framing, "model stage takes none where a film stood" (R-lost): precision 0.57,
  recall 0.77 (151/195) at 37 flags/day. A per-listing display settle of ~1 min on R-lost changes hides 150/195
  reversals delaying 114 losses, vs a blanket 1 h settle hiding 159/195 and delaying 1018 takes. Structural form:
  do not publish a model loss while the projection's asked answers are unfiled (record `awaits` in take history).
  See `docs/design/identity-no-drift.md`, "Predicting flips (backtest, 2026-11-05)".
- **Code:** annotated tag `experiment/flip-counterfactual` (`41f7beb2c501`); the branch `flip-counterfactual` was
  removed.
