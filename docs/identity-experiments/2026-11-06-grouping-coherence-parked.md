# 2026-11-06 — grouping coherence, a report-only detector of wrong groupings — PARKED (tag `experiment/grouping-coherence`, head `32c143df5c`)

- **What:** `services.identity.GroupingCoherence` (built 2026-11-01..02 on branch `grouping-coherence`, 7 commits):
  a reader over the head resolve's decisions that flags clusters whose members (1) state conflicting facts (year more
  than 1 apart, a different director, a runtime gap of max(30 min, a quarter)), or (2) would resolve differently on
  their own (`DecisionTrace.Alone` / `Denied`, two read-only trace fields filled in `ResolverDecisions`). Wired as a
  ranked suspects list in the takes check (`scripts/ci/grouping_suspects.py`, step summary, a VictoriaLogs metrics
  line) and on `/debug/review` above the queue. Changes no take. The notebook with the signals, the calibration and
  both runs is `docs/investigations/2026-11-01-grouping-coherence.md` on the tag.
- **Measured:** calibrated run 41348097395 (prod-latest 41265286306): 41 clusters flagged in 5 countries, 11 judged,
  **1 really wrong** (DE tmdb:42236 "Fellinis Casanova", Filmmuseum München: a second programme crediting Jeunet grouped
  under Fellini's film by title). The false positives are years a venue states differently from the record
  (re-release years: all 5 UK flags; Kino Muzeum, kinoheld/Bambi and flicks.us feed years). Cost under 6 s a takes row.
- **Why parked:** 4 days stale against main; rebasing conflicts with the review page (`ReviewController`,
  `ReviewWiring`) that a running agent is landing, its trace fields change the decisions lines the takes skip gate's
  stored traces are keyed on, and `identity-takes-check.sh` is being reworked (the A/A row, the 20-runner packing). Its
  yield (1 confirmed wrong grouping of 11 judged flags) does not justify landing that churn now.
- **Angle worth trying next:** resume from the tag on a fresh branch off main, after re-calibrating the year signal
  (a venue year equal to the screening year, or a re-release of the record's year, weighs less; the notebook's
  follow-up 2). The Casanova split (same-venue programmes crediting different directors) is a grouping fix of its own,
  not this detector's; judge its take into `identity-judgements.tsv` when it is worked.
