# 21. Budget-spent correction fallback (stands as last decided), `CorrectionsPerApply`

- **Where:** `corrected`
- **Verdict:** interim only for a DECIDED correction; a take with none decided is read at once (2026-10-29)

The budget (200 corrections an apply), like the backfill's, only DELAYS a decision. The final result does not depend on
read order: once `backlog` is false it equals the whole resolve (harness property 2). A restart keeps the takes it had
(property 3). It is a deliberate memory bound on worker-us.

What waits for it is only a correction already DECIDED over the take's listings: one a deploy's new rules left stale, or
one an answer it read was filed again over. It stands as decided meanwhile, so the take served does not move until the
reading again does. A take with no decided correction (never read at its shape, or read waiting on an answer) spends
none of the budget. Left for a later apply's budget, it was served as the model took it, then corrected at an apply where
none of its inputs moved: an interim take its correction overrode, which the drift replay's flip check
(`services.identity.TakeFlips`) fails on, as prod's `kinowo_identity_listing_flips` counts it. Convergence run 3656
failed on such takes in three countries' rows: UK "The Ties That Bind Us" (corrected at the step-5 re-scrape), UK
"Cosi Fan Tutte" (a pooled take at step 9, corrected only at the settle), and US "NT Live: Hamlet" (withdrawn at a
deploy). The stage's backlog stayed set through every incremental step of the replay (projections 1-22 of 27).
