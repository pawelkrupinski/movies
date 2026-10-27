# 2026-10-19 — margin over the runner-up (recording 38713274617, base `a07939cf6`)

## Take a candidate below the rating cut that dominates its rivals ("dominant") — dropped

- **Tried:** a last alone rule, `dominant`: the best eligible candidate (priors lending) at probability ≥ floor F and ≥ R×
  its runner-up's (or with none), no closer record — below `ratingCut` (37.5 %). Constants, no flag.
- **Opportunity** (`scripts.MarginOpportunity`, the five captures, nodes still unmatched after the agreement stage):
  1,173 nodes have a best candidate under the cut. F ≥ 0.5 is above the cut, so the grid is F ∈ {0.2, 0.25, 0.3}:
  F 0.3 → 22 nodes at R 5 (16 at R 10) + 7 with no rival; F 0.25 → 75 + 15; F 0.2 → 80 (R 5) / 99 (R 2) + 16. The
  probabilities cluster on class plateaus (0.2966, 0.2942, 0.3674 …) and runner-ups sit at ~0.03, so R barely
  discriminates: almost every best candidate below the cut is ≥ 5× its runner-up (the `rivals` measure is already in
  the probability).
- **Measured** (`scripts/identity-measure-local`, all five countries; model + final stage; LOST = model lost + final
  right-lost):

  | point | gained | right | unjudged | NEW WRONG | right→wrong | lost |
  |---|---|---|---|---|---|---|
  | A F 0.30, R 5 | 183 model + 16 final | 29 | 45 | 125 (117 = UK "CBeebies Panto 2026: Treasure Island") | 3 | 6 |
  | B F 0.25, R 5 | 239 model + 17 final | 54 | 64 | 138 | 3 | 7 |
  | D = A + the billing is only the film's title | 31 model + 7 final | 18 | 17 | 3 | 2 | 1 |

  Every point NOT LANDABLE. The wrong ones are the predicted kinds: a broadcast panto taking the 1950 or 2027 namesake,
  MET relays taking a namesake film ("MET: Silent Night" → 2023), double bills ("The Tiger Who Came for Tea and …"),
  events ("Boarding on Insanity + Director's Q&A" → a 9-minute short), a stage work ("Manon" → 2007), and bare
  namesakes ("Unrest" → 2006, "Dernier été" → 2026, "Obsession (PG)"). Worse, the rule pre-empts better takes: an
  alone take keeps a node apart from its cluster's pooled or agreed film — DE "CONFIDENTE" 2025 → 2016 and "WHEN PIGS FLY"
  2026 → 1993 (both judged WRONG), UK "Andre Rieu: Let it Snow" → the 2019 film. The bare-billing guard (D) removes the
  event/relay/bill class but not the bare namesakes or the pre-emption.
- **Separation (does the margin help at all?)** On the 274 clusters of the unified training set
  (`identity-unified/training.tsv.gz`, 2026-10-08) whose top-logit candidate `identity-judgements.tsv` judges (168 right,
  106 wrong): AUC of the probability alone 0.72, of log(p1/p2) alone 0.76; 5-fold CV logistic AUC 0.69 → 0.75, log-loss
  0.624 → 0.582, Brier 0.214 → 0.196 with the ratio added. Ratio quantiles p10/p25/p50: right 1.6/8.3/24, wrong
  1.0/1.6/3.0. Below the cut the ratio does NOT separate (AUC 0.58, p1 0.48; 37 right vs 59 wrong), which is why the take
  rule fails. Among takes the stack makes (147 judged, 17 wrong) it does: ratio < 2 holds 5 of 17 wrong for 13 of 130
  right; over all 6,463 taken clusters ratio < 2 is 142 takes. The judged set is biased toward suspects; the training
  rows predate several resolver changes.
- **Why it stopped:** zero new wrong is unreachable — the wrong kinds below the cut look exactly like the right ones
  on probability and margin.
- **Next angle:** margin as a REFUSAL, not a take: refuse (or send to the agreement stage) a take whose runner-up is
  within 2× (`LeanMargin`-like) unless a fact of the listing's own favours it — 142 takes to weigh against ~5 wrong
  cured, within the trade rule if measured so on current code. It could also replace the pinned `leaning` conjunction
  in `identity-unified-rules.json` only after that measure.
- **Code:** `experiment/margin-take` (points A, B, C = F 0.2/R 2 unmeasured, D; `scripts.MarginOpportunity`).
