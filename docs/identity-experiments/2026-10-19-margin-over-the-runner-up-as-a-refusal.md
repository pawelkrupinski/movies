# 2026-10-19 — margin over the runner-up as a REFUSAL (branch `experiment-margin-signal`, tag `experiment/margin-signal`, recording 38713274617, base `82de60cf0`)

The "next angle" of `experiment/margin-take` (its entry lives on that tag): refuse a take whose runner-up is close.

- **Tried:** `Acceptance.clearOfItsRunnerUp`, a filter in `takenBy`, so it covers the alone, pooled and season paths.
  It refuses a take when the best other eligible candidate is within R× its probability. In the exempt variants, a take
  `EvidenceWeights.favours` over that rival stands. The commits are points a–f: R 1.5/2/3, without and with the
  exemption.
- **Separation on current code first.** Source: the prod mirror's `identity_traces`, all five countries, 2026-10-19.
  The taken film's probability is not stored, so it was rebuilt as prior + evidence terms through the isotonic map. The
  traces were joined to the judgements with `label_of` (`target/margin/q1/sep.py`).
  - Unit: (country, title, film). 12,228 takes; 580 judged, 546 right and 34 wrong.
  - Ratio AUC 0.59, probability AUC 0.51. With 34 wrong, 5-fold cross-validation stays below 0.5 for both models. The
    training set's 0.69 → 0.75 does NOT hold.
  - Ratio < 2 covers 491 takes: 65 right, 4 wrong, 422 unjudged. By rule: favoured-calibrated 164, family 137,
    season-record 53, imdb-suggested 51.
- **Measured** with `scripts/identity-measure-local` (all five countries, base `82de60cf0`):

  | point | cured | right lost | unjudged lost | new wrong / right→wrong |
  |---|---|---|---|---|
  | e: R 2, exempt when favoured | 0 | 361 (model 354 + final 7) | 2,257 | 0 / 0 |
  | b: R 2, no exemption | 16 listings (ES "La maleta" ×14, UK "Notre-Dame de Paris" ×2) | 443 | 4,060 | 1 / 2 (DE "Live - Vortrag: Südafrika") |

  Both are NOT LANDABLE. Points a, c, d and f were not measured: R 2 already shows the shape of the result.
- **Where the refused takes came from** (e's lost listings, by prod's trace rule): favoured-calibrated 3,939,
  season-record 191, sole-result 105, family 32, imdb-suggested 27, pooled 12.
  - favoured-calibrated still loses under the exemption. The exemption weighs the rival with the highest probability,
    while the rule weighs the second-ranked one.
  - No weak rule concentrates the wrong takes. Scoping the refusal to the weak rules would cure nothing.
- **Examples** (verdicts by `scripts/label-lookup`). `identity-judge-cards` cannot card these: it cards only the
  capture's unmatched takes, and these are matched model takes.
  - UK "RBO Live - Tosca" → RBO 2026/27 Tosca: RIGHT, lost. The runner-up is another season's record of the same work.
  - UK "Dune: Part Three - See It First Preview": RIGHT, lost.
  - US/UK/ES "BTS WORLD TOUR 'ARIRANG' IN SÃO PAULO" ×2,089: unjudged and plainly right, lost. The runner-up is the
    Buenos Aires live viewing at the same probability. The city in the title decides, not the facts.
  - DE "Lalka (The Doll)" ×30 → 2026: unjudged and right (PL judges it so), lost.
- **Why it stopped:** a close runner-up is mostly a namesake from the same series (other cities, other seasons,
  remakes) that the title's words already tell apart. Probability does not see that, so the margin carries no new
  information on the takes the stack makes. Each wrong take cured costs ~30 right and ~250 unjudged.
- **Not tried:** margin as a weighted unified signal. On the 2026-10-08 training rows (`target/margin/q1/derive.py`),
  margin ≥ 2× fires on 26 wrong vs 12 right untaken contenders. The pinned
  `model.lean&count.leaning&title.namesIt&film.current` fires on 2 training clusters. Swapping `model.lean` for
  margin ≥ 2× or ≥ 5× fires on 2–3, all right: too thin to replace it or refit `LeanMargin`.
  `scripts/identity-refit` tunes neither the unified rules nor `LeanMargin`.
- **Code:** `experiment/margin-signal`, points a–f as commits; the tip is f, R 3 exempt.
