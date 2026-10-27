# 14. Cluster keys, counter, key store, restart re-attach

- **Where:** `StableNames`, `AgreementKeys`
- **Verdict:** (a) name

Nothing is found by a key (10). Harness property 3 restarts with keys kept, lost and renumbered. The harness's twin property runs a second stage whose keys count from elsewhere and are lost, renumbered or permuted on every restart: groupings, takes, waits (`waitingOn`) and the backlog stay identical.
