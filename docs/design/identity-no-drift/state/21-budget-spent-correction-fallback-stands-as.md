# 21. Budget-spent correction fallback (stands as last decided), `CorrectionsPerApply`

- **Where:** `corrected`
- **Verdict:** interim only, accepted

The budget (200 corrections an apply), like the backfill's, only DELAYS a decision. The interim takes depend on read order, but the final result does not: once `backlog` is false it equals the whole resolve (harness property 2). A restart keeps the takes it had (property 3). It is a deliberate memory bound on worker-us, named here as the one place the principle holds at quiescence rather than at every tick.
