# 2026-11-05 — a model loss keeps its previous film while the agreement's questions are unfiled — PARKED by the owner (head `b3f0e5051873`)

- **What:** `PendingLosses.hold`, applied in `IdentityProjection` after the agreement stage: a model no-film (no
  candidate, no evidence, below the cut; no fallback) whose decision the stage still waits on
  (`AgreementStage.waitingOn(d).questions`, asked and not attempted) and whose listings were all on one TMDB film keeps
  that film, basis `Held`, Pending (provisional card), until each question is attempted. Take-change lines gained
  `awaits` / `awaits_first`.
- **Why it was tried:** 151 of 215 prod reversals in the week to 11-05 were a model drop to none undone 5-7 s later.
  In 14 of the 16 dropping projections (143 of 166 DE/UK changes) the answers the agreement asked inline were filed
  DURING the projection, past `AskNowWithin` (5 s), and the next, answer-triggered projection took the film back
  (DE "Geister" x56, 11-02 17:10:55Z). Offline estimate: ~123 of the 151 removed, none added.
- **Measured:** local layers only (IdentityProjectionSpec Geister-shaped case, TakeChangeLinesSpec, RuleContract,
  FilmScheduleEndToEnd unshifted). No measure or drift dispatched (CI budget full); neither replays the projection.
- **Why parked:** it substitutes the previous take while Pending, against the owner's step-6 rule (2026-10-31: a Pending
  take is the decision on the inputs as they stand, never a previous take). The owner (2026-11-05) wants the
  projection to WAIT for its agreement answers instead, bounded by the answers' own timeouts and retries.
- **Code:** tag `experiment/pending-losses-held` (`b3f0e5051873`); branch `identity/await-answers-before-loss` (unmerged,
  so `git branch -d` refuses it; left for the owner to drop).
