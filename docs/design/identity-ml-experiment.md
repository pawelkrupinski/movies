# Learning the matching rules: logistic regression and boosted trees on the pinned corpus (2026-10-06)

An OFFLINE experiment. No production code changed. The question: can matching rules be derived by linear regression or
machine learning from the whole pinned corpus, and how good, fast and accurate would that be at the bar that matters,
ZERO wrong matches?

Code: `scripts/ml-identity/identity_ml_experiment.py` (the fits and the measurements) and
`worker/src/test/scala/scripts/IdentityMlScorerBench.scala` (an in-JVM scorer for the boosted trees, with
`IdentityMlScorerBenchSpec`). To rerun:

    python3 -m venv <env> && <env>/bin/pip install scikit-learn lightgbm numpy
    timeout 1800 <env>/bin/python scripts/ml-identity/identity_ml_experiment.py        # ~6 min; target/identity-ml/
    sbt "worker/Test/runMain scripts.IdentityMlScorerBench target/identity-ml/gbm-d2.json target/identity-ml/scores-d2.txt"

## Dataset

The dataset is the unified evidence model's checked-in contender rows,
`test/resources/fixtures/identity-unified/training.tsv.gz`, written by `integration.IdentityUnifiedDataset`
(identity-resolver.md §20.2). They reuse the resolver's own feature extraction (`UnifiedEvidence.contenders`), so the
features match what the shipped fit reads:

- 25,301 (cluster, contender film) rows, 6,763 clusters across all five recorded full corpora (PL, UK, DE, ES, US), and 53 features.
- **Resolver features (14):** `model.logit` (the calibrated, monotone-refitted logistic over the `IdentityMeasures`
  tables: title, original title, year/title-year/season deltas, director, runtime, country, search rank, rivals,
  popularity, venues corroborating), `model.unscored`, `model.deniedBySome` (the learned cannot-links), the acceptance
  rules that fired (`rule.*`), the model's lean and best candidate, and the season/house productions.
- **Agreement signals (39):** family takes and leans (IMDb, Wikidata, Filmweb, Metacritic, RT), turned down, dissent, the
  listing's facts, runtime and contradictions, `venues.current`, the title guards, bills, stage work, the poster
  match/near/veto/vote, quorum, broadcast, catalogue, and the counts and conjunctions behind the quorum.
- **Labels:** 1,606 HAND rows (from `labels.tsv`, judged per listing; every other contender of a cluster whose right film
  a label names is wrong) and 22,643 WEAK rows (today's model take on a matched cluster that no label judges, with its
  other contenders as wrong). 1,052 rows are unlabelled and kept out of training. 6,540 clusters have a labelled-right
  contender, 126 of them hand-labelled.
- `expected-matches.tsv` is the ratchet's list of today's takes, not an independent label, and the 08-06-2026 e2e
  corpus's tmdbIds are the pipeline's own output. Neither adds ground truth, so neither is used as a label.

**The caveat that shapes every number below:** weak labels ARE today's takes. On 6,414 of the 6,540 positive clusters a
model can only reproduce today's stack, never beat it, and every feature that encodes the model's own rules
(`rule.calibrated`, `model.logit`) leaks the weak label. The 126 hand-labelled clusters are the only independent test.

## Method

- **Decision:** per cluster, take the best-scoring contender when its score clears a cut. A take is right or wrong by the
  contender's label.
- **Hold-outs:**
  - Grouped 5-fold cross-validation. Groups are the connected components of clusters joined by any film they hold a
    positive label for, so neither a cluster nor its right film sits on both sides (5,045 groups, the largest 398 rows).
  - Leave-one-country-out.
- **Cuts:**
  - *Oracle*: the cut is picked on the hold-out's own scores. This is an upper bound.
  - *Honest*: an inner grouped 4-fold cross-validation on the training part picks the cut (just above its highest
    counted-wrong take), and that cut is applied to the hold-out. Every take is counted in the hold-out fold only.
- **Zero-wrong bars:**
  - `strict`: any wrong take.
  - `new`: a wrong take that today's stack does not already make. This is the shipped fit's own bar.
  - `fill`: only clusters today's stack takes nothing in. Augmenting, never overriding: 260 clusters, 51 with a right film.
- **Training:** hand rows weighted ×5. Logistic: sklearn L2 (C=1, standardised). GBM: LightGBM, 300 rounds, 15
  leaves, learning rate 0.05. Monotone GBM: each signal held to `UnifiedEvidence.Signals`' direction.

## Results (grouped cross-validation, every take counted in the hold-out)

Today's stack on the same clusters: 6,489 / 6,540 right (99.2%) with **4 wrong**. These are hand-labelled model takes:
DE "Die Unbeugsamen", US "NT Live: All My Sons", PL "Ktoś całkiem obcy" and PL "Teksańska masakra piłą mechaniczną".
On the hand-labelled clusters it takes 75 of 126.

| model | strict recall@0 (oracle / honest) | new-wrong recall@0, honest (gained / lost vs today) | strict recall@1 / @3, honest | fill @0, honest (of 51) | AUC all / hand | train (fit) | score / cluster |
|---|---|---|---|---|---|---|---|
| (a) today's resolver + agreement | — (4 wrong) | 99.2% | — | — | — | — | — |
| (b) logistic, resolver features | 46.6% / 40.6% (1 wrong) | **98.2%**, 0 new wrong (+2 / −69) | 65.4% / 98.1% | 2 right, 0 wrong | 0.9988 / 0.909 | 0.06 s | 0.25 µs |
| (c) GBM, resolver features | 0.2% / 18.4% (1 wrong) | 96.7%, 0 new wrong (+0 / −164) | 79.0% / 95.9% | 0 | 0.9986 / 0.907 | 0.9 s | 7.8 µs |
| (d1) logistic, + agreement | 2.9% / 2.6% (1 wrong) | 3.5%, 0 new wrong | 7.2% / 29.9% | 1 right, 1 wrong | 0.9986 / 0.942 | 0.16 s | 0.6 µs |
| (d2) GBM, + agreement | 0.1% / 19.5% (2 wrong) | 95.9%, **2 new wrong** (+4 / −221) | 95.9% / 98.1% | 4 right, 2 wrong | **0.9996 / 0.978** | 2.2 s | 8.4 µs (JVM 18 µs) |
| (e) monotone GBM, + agreement | 1.0% / 18.7% (1 wrong) | 97.7%, 1 new wrong (+2 / −101) | 90.1% / 98.4% | 2 right, 1 wrong | 0.9996 / 0.982 | 0.5 s | 5.5 µs (JVM 20 µs) |

On the hand-labelled clusters at the honest new-wrong@0 cut, against today's 75 / 126 right:

| model | right / 126 | new wrong |
|---|---|---|
| (b) | 12 | 0 |
| (d1) | 28 | 0 |
| (d2) | 50 | 2 |
| (e) | 59 | 1 |

On every hold-out the learned models take fewer right films than today's stack and lose more than they gain.

**Leave-one-country-out** (honest cut from the other four countries):

- The strict zero-wrong cut does not transfer. Both GBMs take 0–37 of 873 in PL and 0–3 of 1,474 in UK; logistic (b)
  takes 19–61%.
- At the new-wrong bar (b) keeps 93–99% per country, but still adds 2 new wrong in PL and 2 in UK.

### Why strict zero-wrong collapses

Every model scores today's four wrong takes at ≥ 0.99, which is as high as clean right takes. Each of them is a take
the model's own rules fired on, with the same feature pattern as thousands of right ones. A cut above all four leaves
almost nothing. The features cannot separate those cases, so no learner can. That is a missing signal, not a
learnable rule. They were caught by a poster and by hand: `model-take-audit.tsv`.

### Calibration (reliability, held-out labelled rows)

| model | predicted | observed |
|---|---|---|
| (b) | 0.9–0.99 bucket, mean 0.985 | 0.995 |
| (b) | 0.5–0.7 bucket, mean 0.63 | 0.50 |
| (c) | 0.1–0.3 bucket, mean 0.17 | 0.07 |
| (d2) | ≥ 0.999 bucket, mean 1.000 (5,573 rows) | 1.000 |
| (e) | ≥ 0.999 bucket, mean 1.000 (3,354 rows) | 1.000 |
| (e) | 0.3–0.5 bucket, mean 0.36 | 0.62 (13 rows) |

- Logistic (b) is well calibrated at the top and over-confident in the middle.
- GBM (c) is over-confident at the low end.
- The middle buckets hold 10–60 rows each. With so few rows a probability there means little, and the zero-wrong cut
  never sits there.
- The full tables are in `results.json`.

### The wrong takes just below the zero-wrong cut, by name

- **(d2), (e):**
  - PL "Lalka" → Lalka (1968)
  - PL "Akademia Polskiego Filmu: W kręgu kina…" → Strachy (1938), a programme banner
  - PL "Obcy w domu" → 1989, another film's own title
  - PL "Filmowe popołudnie dla dzieci: Jesień" → Sonbahar (2008)
  - PL "Zamki na piasku" → Sandcastles (1972)
  - PL "LALKA / DOLLY" → Lalka (2026)
  - PL "Manon" → 1949, a stage work
  - DE "André Rieus Weihnachtskonzert 2026" → Tage wie diese
- **(b):**
  - UK "Royal Ballet and Opera: Tosca" → the 2025/26 record
  - PL "Silent Night" → Wśród nocnej ciszy
  - UK "Royal Opera House: Carmen", "The Royal Ballet: The Nutcracker"
  - UK "We're Going on a Bear Hunt + The Tiger Who Came to Tea", a double bill
- **(d1):** PL "Fallen Angels by Noël Coward" → Tchórz (2026).

Each of these is a case one of the hand-written guards or thresholds refuses today: `stage.work`, `bill.several`,
`title.anothersOwn`, the quorum and its rise per dissenting family, and the programme banners. They are the same takes
§20.4 to §20.6 found for the unified logistic model.

### What it would gain

- The right takes today misses, at a zero-new-wrong cut:
  - DE "Die Nibelungen - Teil 1: Siegfried" (b)
  - PL "HER Docs - To nie jest świat dla ludzi z…" → Mysteriet om Menopausen (d2)
  - PL "Baranek Shaun i kudłata bestia" rodzinne (d2)
- At most 2–4 of the 51 fillable clusters, and only (b) adds them with 0 wrong held out.
- The rest of the 51 are the classes §20.14 already measured as unfixable at zero wrong.

## Speed and memory

- **Training:** logistic fits in 0.06–0.16 s and the GBMs in 0.5–2.2 s (25k rows, 4 threads). The whole nested
  cross-validation for five models takes about 6 minutes.
- **Scoring is negligible next to computing the features.**
  - Python takes 0.25 µs per cluster for logistic and 5–8 µs for a GBM (2–57 ms for every contender of all five corpora).
  - In the JVM, `IdentityMlScorerBench` (plain Scala, no native library, no new dependency) reproduces LightGBM's
    probabilities to 5e-11 on every row. It scores the whole corpus in 122 ms (d2: 300 trees, 4,199 splits) and 137 ms
    (e), 18–20 µs per cluster, allocating **0 bytes** (`ThreadAllocation`, median of 20 passes).
  - The features need the full resolver and agreement stage: today's ~3.1 GB-allocation replay of the five corpora.
    A learned scorer would sit on top of that and replace none of it.
- **Model size:**
  - Logistic: 14–53 weights (under 1.3 KB). It ports trivially; `LogisticFit` and `UnifiedWeights` already do it.
  - GBM: 445–540 KB of LightGBM text (1.4–1.7 MB as a JSON dump). It runs in-JVM with the 60-line evaluator in
    `IdentityMlScorerBench`, which handles numeric `<=` splits and NaN direction. LightGBM's JNI build (lightgbm4j) or
    an exported PMML/ONNX evaluator are the alternatives.

## Interpretability

- **Logistic, resolver features (b):** standardised weights `rule.calibrated` 3.1, `model.logit` 1.6,
  `model.deniedBySome` −0.94, `rule.title` 0.73, `rule.imdb` 0.61, `production.house` 0.48, `production.season` 0.39,
  `rule.director` 0.27.
- **Logistic, + agreement (d1):** adds `poster.otherMatches` −1.17, `listing.facts` 0.71,
  `count.takersLessDissent` 0.68, `broadcast.take` 0.53, `agreement.quorum` 0.43. It also gives `and.takersNoDissent`
  −0.46 and `count.takers2` −0.43, which are the wrong sign: collinearity among the count features, not evidence.
- **GBM importance (gain share):** `model.logit` and `rule.calibrated` take 84–93% of it. Next come
  `count.takersLessDissent` (the quorum with its rise per dissenting family), `count.takers`, `rule.title`,
  `broadcast.take`, `rule.imdb`, `poster.match`, `rule.stage` and `poster.otherMatches`.

So the learners mostly reproduce today's rules: the model's own acceptance, then the agreement's quorum with its
dissent rise and the poster veto. They found no conjunction that the hand-written guards miss and that also holds up
held out. Where a learner departs from the guards, it makes the wrong takes listed above.

## Verdict

**Replacing the current fit:** no. On every hold-out each learned model is below today's stack:

- 95.9–98.2% of right clusters, against 99.2%.
- It loses 69–221 of today's right clusters and gains 0–4.
- Only the plain logistic over the resolver's own features stays at zero new wrong held out.

**Augmenting it:** at best 2 of 51 fillable clusters at zero wrong held out. That is noise-level and comes with the
risk below.

**The GBMs rank best on the independent labels** (hand AUC 0.98 against 0.91), but that does not turn into zero-wrong
recall:

- A strict zero-wrong cut sits above today's own four wrong takes, which no feature separates.
- At a cut tuned on held-out folds, the GBMs already make 1–2 new wrong takes.

**Risk.**

- Only 126 hand-labelled positive clusters, and the cut is set by the handful of high-scoring hand-wrong takes named above. One relabel moves it.
- Weak labels make every learner learn "do what today does".
- The leave-one-country-out cuts do not transfer.

**Recommendation.**

- Keep the calibrated logistic plus hand-written guards. ML adds nothing measurable at zero wrong.
- Spend the effort on new evidence for the classes that defeat every learner here: programme banners, stage works,
  another film's own title, and model takes with no counter-signal (posters, venue pages).
- Spend it on more hand labels too, and rerun this script when the labels grow, especially the
  hand-labelled-wrong takes.
