# The footprint measure (`scripts/identity-measure --footprint`), validated 2026-10-22

**Why.** A full identity measure resolves every listing of the recording at base and head on ci-runner. On 2026-10-22
its compute was ~5-12 minutes, but runs took 21-102 minutes because they waited for runners. A footprint measure that
avoids the dispatch is only worth having if it is SOUND: it must never say "0 moved" when the full measure would move a
take.

**Ideas weighed.**

- *Re-resolve only the clusters a change touches* (billings whose TitleRulesGoldenSpec readings differ, plus their
  families). Rejected as a proof. The resolver also reads candidate titles, title segments and corpus-wide context
  through the normaliser, so a billing-level diff does not show every reading a change alters. And a selector slice has
  missed whole-corpus regressions twice before (26e48897d, a083).
- *A proxy for matcher/scoring changes* (the takes whose decision trace named the changed class). Rejected: a changed
  rule can fire where it never did before, and no trace of that exists. Such changes fall back to the full measure.
- *Built: replay the base's every title reading.* `TitleNormalizer` tells a process-wide reader every public reading it
  makes (country, method, arguments, result). `IdentityResolveDump --trace` and `UnmatchedClustersRatchet --trace`
  write the distinct readings (`services.movies.NormalizerTrace`). The takes check stores them per side as
  `takes-trace-<cc>-<identity code>-p<run>`. `NormalizerTraceCheck` reads every one of them again under the head's
  rules. If the change touches only the title rules and every reading comes out alike, the head's resolve would make
  the same readings, take the same branches and decide the same, so no take can move. A difference in `firedRules`
  (rule ids, read only by the decision trace) is listed apart and is not counted as a move.

**Validation.** Every case used recording 573. The base traces came from `identity-ci-run` runs 1378-1383: base
commits with the reader patched in, `--traces`. The full measures are the takes-only CI runs; the run number is given
in the table.

| change | kind | footprint (wall) | full (run, wall incl. queue) | agree |
|---|---|---|---|---|
| db8536f18 merge-key tier → table | rules | 0 can move (47 s) | — (no full on record) | — |
| b66180824 venue tier → table | rules | 0 can move (30 s) | 0 moved (1143, 54 min) | yes |
| 8728b27fb merge DKF/meeting tails | rules | 0 can move; 11 PL rule ids renamed (44 s) | 0 moved (1150, 27 min) | yes |
| f664df1ca Peppa "Cinema Experience" | rules | pl,de,uk,us read otherwise → full (55 s) | UK +350 gained-right (1387, 82 min) | yes (conservative) |
| 0321412ef BTS ARIRANG | rules | uk reads otherwise → full (45 s) | UK +136 gained-right (1388, 91 min) | yes |
| a13d52c9f PL strand, no separator | rules | pl reads otherwise → full (46 s) | PL +11 gained-right (1389, 102 min) | yes |
| 3ff3fe9b4 Dating Game | matcher | gated → full (1 s) | DE 1 cure (1261, 31 min) | yes (fallback) |
| 3eee813cb relay sole-result | matcher | gated → full (1 s) | UK 1 cure + 5, ES +7 (1271, 21 min) | yes (fallback) |

No case had a footprint of 0 where the full measure moved a take. When the footprint flagged a country, it was a
superset of the countries the full measure moved. A sanity run at a single commit read 871,872 readings and found
none different. Traces are 0.2-2.7 MB gzipped per country. The trace work adds about 1-3 s to a resolve.

**Limits.**

- The proof holds for the recording the trace was made over. Fills laid on later answer more questions, and the
  readings of those answers were never traced.
- A normaliser built other than through `TitleNormalizer.forCountry` cannot be read again. The check then counts the
  reading as unreplayable and exits 3, which is no proof.
- `TitleRulePlaceholders.scala`, `BillingParse` and `title-words.tsv` are read outside the normaliser, so a change to
  them is not footprint-measurable.
- A base whose identity code no takes check has traced yet falls back to the full measure.
- Landing always needs the full measure.
