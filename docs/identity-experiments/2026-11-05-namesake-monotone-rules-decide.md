# 2026-11-05 — approach A: a namesake-ambiguous listing decided by monotone (kind i/ii) rules only — NOT BUILT, the offline sort says empty

- **What:** take a namesake listing's film only through rules of kind (i) veto or (ii) pin, never population-dependent
  (iii) rules, so the take cannot flip with the population.
- **Measured:** offline sort only, `RuleContract` kinds (rule-contract `6060e6608cee`) against the decision traces of
  measure 41910637082's base: every base take behind the 50 (2f864590a) and 48 (b030bb899e2) served losses, the 9
  right->wrong and the new wrong.
- **Result:** every one is decided by a kind (iii) rule (final: `agreed:*` 33, `fill:*` 15-17; beneath: `accept:*`, `pooled:*`,
  below-the-rating-cut). The registry declares no accepting (i)/(ii) rule except `pin`. So A = "pinned, else no film" =
  the no-window column: 0 of 50 / 48 served losses recovered, 9 right->wrong become right-lost, every flip listing
  (Titans, Halstead/Fishguard R&J) held at none. A measure would have repeated 41795872892 (~4.5 h on starved CI).
- **Why parked:** nothing to recover with the existing rules. To keep e.g. Casablanca 1943 a NEW accepting rule must be
  declared and walk-proved (i)/(ii): accept a namesake's film only when every rival is vetoed by a (i) rule (own stated
  year/director/runtime against TMDB) and its own facts clear the cut with the population features
  (`venues.corroborating`, rivals, released-as-namesake) masked. A design choice, it fits held takes plus own facts.
- **Code:** none (sort only; table and script were scratch). Related: `2026-11-04-namesake-ambiguous-no-window.md`.
- **Closed 2026-11-05 (parked-items review)** with the no-window variant: see `2026-11-04-namesake-ambiguous-no-window.md`, "Re-checked 2026-11-05".
