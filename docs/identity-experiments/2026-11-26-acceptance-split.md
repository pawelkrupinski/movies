# 2026-11-26 — `Acceptance` split into 13 pieces of at most 9 collaborators — PARKED (commit `c510b75c51`, tag `experiment/acceptance-split`)

- **What it tried:** consolidation A, phase 2. Split `Acceptance`'s 4 rule groups (bill, calibrated, director, fact,
  title, veto, fallbacks) into 13 pieces of at most 9 collaborators each, with a shared evidence seam
  (`AcceptanceFactRows`, `AcceptanceTitleRows`, `AcceptanceStageRows`, `AcceptanceRuleSteps`) and an `AcceptanceWiring`
  root composing the rule graph. Behaviour unchanged; 26 files, +1079 / -911.
- **The result:**
  - all but 2 pieces are at 9 or less; `AcceptanceStageRows` is at 10 and `AcceptanceWiring` at 14;
  - `services.identity` distance D rises 0.20 -> 0.22, past the 0.20 limit (92 collaborators, 11 abstract); the package
    has room for only about 5 more concrete collaborators;
  - the only ways to land it were a package exemption, or interfaces added only to move the number.
- **Why it was parked** (owner decision 2026-11-26): the coupling is real. The rule groups read the same scored
  candidates, listing, weights and calibration, so splitting them spreads one dependency set over many classes and adds
  a seam without removing a dependency. Phase 1's 4 dated fan-out exemptions on `Acceptance` stay instead.
- **Where the code is:** tag `experiment/acceptance-split` (local), commit `c510b75c51`, branch `acceptance-split`.
- **When to revisit:** if acceptance becomes its own measured package (`services.identity.acceptance`, with a row in
  `test/resources/coupling-layers.tsv`), its distance is then measured on its own and the pieces no longer count
  against `services.identity`.
