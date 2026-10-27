# 1. Stored families plus their slice digest

- **Where:** `IncrementalResolver.families`, `IdentityModelStore`
- **Verdict:** (a) cache

Reused only when the digest of every corpus fact the family read matches (`CorpusContext.slice`, a superset). On restore, nodes and claims are checked too. `IncrementalResolverSpec` checks every event against a whole resolve.
