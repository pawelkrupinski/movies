# 2026-10-19 — take stability across scrapes: hysteresis and flip history (branch `experiment-take-stability`, base `b79c71a78`)

## Hold a long-held take against a switch (hysteresis), or flag a flipped take low-confidence — not landed
- **Tried:** (a) a listing that has held film X for ≥ H hours keeps it unless the switch comes with a resolver-closure
  change (reset), H ∈ {6, 24, 48} h, holding "no film" too or not; (b) a take whose listing flipped in the last 8 days
  (A→B→A reversal, ≥2 changes, or one change outside a deploy) as a low-confidence/suspect signal.
- **Measured:** not by `identity-measure`: its corpus is ONE recording resolved fresh, with no per-listing take
  history, so a prior on past takes has nothing to read there. Replayed instead over prod's own history: 8 days of
  `identity take change` lines (VictoriaLogs, 5,791 changes) and the mirror's current `identity_traces` (173,632
  listings), every take judged by `identity-judgements.tsv` (`label_of`). A change within 30 min of a build's first
  projection in its country counts as closure-driven ("deploy"), the rest steady-state. Scripts:
  `scripts/experiments/take-stability/` (`fetch.sh`, `analyse.py`, `signal.py`, `hyst.py`).
- **How often takes flip:** 4,383 listings changed in 8 days (DE 2.8 %, UK 4.0 %, US 1.8 %, PL 0.8 %, ES 0.5 %); 1,083
  changed ≥2 times, 971 reversed (A→B→A: DE 318, UK 621). 48 % of changes are steady-state (no deploy). Reversals are
  a few event billings at many venues (UK "CBeebies Panto 2026: Treasure Island" 211 changes, BTS "Arirang" live viewing
  166, Starlight Express 112, RBO Macbeth 52, LoL Worlds finals 63), mostly in changed-scope projections, median 10 h
  between flips; basis OwnMatch↔BelowThreshold/Vetoed.
- **Which side is right:** steady-state changes judged: right→wrong 209 vs wrong→right 181; gained wrong 288 vs gained
  right 59; lost right 88 vs lost wrong 86. Deploy changes are mostly cures (lost wrong 631, gained right 334,
  wrong→right 161, vs right lost 167, right→wrong 5). Steady churn is roughly a coin toss; closure changes cure.
- **(a) Hysteresis, billings whose switch it would block** (harm blocked = a right take kept or a wrong one kept out;
  cure blocked = a wrong take frozen or a right one kept out):

  | H | reset on closure change | hold "none" | harm blocked | cure blocked | unjudged |
  |---|---|---|---|---|---|
  | 6 h | no | no | 63 | 191 | 152 |
  | 24 h | no | no | 37 | 109 | 98 |
  | 24 h | yes | no | 17 | 5 | 84 |
  | 24 h | yes | yes | 21 | 15 | 158 |
  | 48 h | yes | no | 16 | 2 | 65 |

  Without the reset it freezes ~3 cures per harm prevented, every served known-wrong take too (9 billings served now,
  all held 8 days+). With the reset it still blocks data-driven cures (DE "The Killer" losing tmdb:970347 at 4 venues,
  "Das Boot - Director's Cut"), and the 84 unjudged billings it would freeze are unaudited.
- **(b) Flip history as confidence, current matched takes:** per LISTING it looks decisive — reversed 221 wrong / 169
  right (57 %) vs stable 46 / 2,992 (1.5 %) — but per BILLING (country, title, film) it is three billings (CBeebies
  Panto, DE "The Killer", PL "Zniknięcie" tmdb:1644): reversed 3 wrong / 18 right (14 %), any steady-state flip 7 / 30
  (19 %) vs stable 35 / 432 (7.5 %). ~2.5× lift on n=37, with the judgements themselves biased toward listings found by
  take-change hunts. It flags 0 of the 9 served known-wrong billings (all stable).
- **Why stopped:** the hysteresis prior trades cures for harm (no reset) or blocks little that the oscillation fixes
  per billing would not (with reset), adds per-listing state the hermetic gates cannot measure (the corpus has no take
  history, so CI would never see what it froze), and reverses the repo's stance that a resolve is a function of the
  current corpus. The flip flag is a weak, small-n signal.
- **Next angle:** treat a steady-state REVERSAL as a bug report, not a prior: alert on `TakeReversals` per billing
  (the gauge exists) and fix the two rules undoing each other (as with "The Ties That Bind Us"); 3 billings explain
  ~60 % of reversals. If a confidence feature is wanted, count reversals per billing across venues, not per listing.
- **Code:** `experiment/take-stability` (the replay scripts; no resolver change).
