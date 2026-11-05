# 2026-11-04 — the window only rules out rivals; the one work left is taken on the listing's own fact — CLOSED 2026-11-05 (head `2f864590ad37`)

- **What:** fixes the Titans regression of the release window: the window only rules rivals out; if it leaves one work
  the listing stays held apart and takes that work per listing only when its own facts corroborate it
  (`Corroboration.facts`), else no film. `NamesakeAmbiguity.read` returns `Reading(works, film)`; decisions per film group.
- **Measured:** measure 41910637082, drift 41910679949 (default seeds) and 41910575596 (de seeds 47, 64); local walks.
- **Result:** local walks 216/216 pass, Titans and Halstead R&J 0 flips; drift clean on both runs (the Dammtor and
  Raschplatz Titans baseline rows are FIXED, removable). But the measure is NOT LANDABLE: right->wrong 9 (de 8, uk 1),
  1 new-wrong (de), served right->none 50 (pl 11, de 32, uk 7), model right-lost pl 155, de 292, uk 103, us 20, es 4.
- **Why parked:** the 9 right->wrong are revival classics whose old film is taken in the base and the window plus
  "own fact" now takes a recent same-titled namesake instead: Die Reise ins Labyrinth -> Labyrinth (2025), Die Killerhand
  x2 -> Idle Hands, Girlfriends (1978) -> a 2025 Chinese film, Casablanca Kamp-Lintfort -> Casablanca (?), Metropolis
  (Lichtblick) -> a 2026 Metropolis, Zeugin der Anklage x2 -> Witness for the Prosecution (?), Aldeburgh Royal Ballet R&J.
  Each is decided by a population-dependent (kind iii) rule in the base (`accept:favoured-calibrated` 5, `accept:imdb-suggested` 2,
  `accept:season-record`, `pooled:unrivalled-calibrated`); a single window-surviving work with a thin own fact (an
  undated "?" film passes the window) is no evidence against the classic. Owner changed approach to held takes and own facts.
- **Code:** local tag `experiment/namesake-ambiguous-rework` (`2f864590ad37`); branch `identity/namesake-ambiguous`.
- **Closed 2026-11-05 (parked-items review)** with the no-window variant: see `2026-11-04-namesake-ambiguous-no-window.md`, "Re-checked 2026-11-05".
