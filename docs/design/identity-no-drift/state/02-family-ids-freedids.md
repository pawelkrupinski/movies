# 2. Family ids, `freedIds`

- **Where:** `IncrementalResolver`
- **Verdict:** (a) name

Labels only. Decisions are sorted by first member. Not `StableNames`: they are in-memory slot numbers, reused through the free list, never stored or shown, and overlap naming would cost a pass per update for nothing. The stored family's document name (`StoredFamily.idOf`, its smallest listing) is not either: a restore finds a family's document from its listings alone, with no previous names to overlap. The staging screen's rows are named by the cluster key, so they are `StableNames`' already.
