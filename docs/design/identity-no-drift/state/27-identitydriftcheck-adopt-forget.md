# 27. `IdentityDriftCheck` (`Adopt` → `forget`)

- **Where:** worker adapters
- **Verdict:** **deleted**

Not wired since the OOM; deleted with `AgreementStage.forget`, `IncrementalResolver.reResolve`, `TakeChangeLines.drift` and the repair setting (ddd1c1951, −309 net).
