# 2026-11-12 — an ambiguity's bare nodes grouped in round A, voting on their pooled evidence — DROPPED (tag `experiment/ambiguous-bare-pooled-vote`, head `02ca784f2f4`)

- **What:** `ConstraintSolver` rule 4 (ambiguous nodes that are no side of their ambiguity join each other, never a
  side) applied to EVERY partition, round A's included, so the joined bare spellings voted as one cluster on their pooled
  evidence. Built for PL "André Rieu. Pada śnieg!": 27 model clusters (30 web films) for one concert, every bare
  spelling left alone by rule 3 because three venues held the concert's record (tmdb:1707785) and Cinema City's 30 denied it.
- **Measured:** `scripts/identity-what-if --base 34d7f11243 --head 02ca784f2f4` (snapshot 2026-11-12 ~21:00Z, all five
  countries): CLEAN everywhere (right-lost 0, right->wrong 0, new-wrong 0) but takes moved, all unjudged: PL model 23,
  UK model 19 / final 1, DE 4, US 0, ES 0. The Rieu slice (recording 43166417765): 64 bare listings in one cluster.
- **Why dropped:** `PopulationMonotonicityPart2Spec` (worker) found new A -> B -> A takes along arrival chains in the UK
  "Romeo and Juliet" relays (Theatr Gwaun Fishguard, Theatr Mwldan Cardigan, Old Market Hall Shrewsbury, Haverhill Arts
  Centre: tmdb:1702789 <-> none): the pooled vote took a film or none by how many bare spellings had arrived -- a
  population read.
- **What landed instead:** rule 4 in the FINAL partition only (`ambiguousTogether`): round A, which the votes read, keeps
  each ambiguous node alone, so each votes on its own evidence and the grouping moves no take; only the film-less bare
  nodes are drawn as one cluster (one web film).
