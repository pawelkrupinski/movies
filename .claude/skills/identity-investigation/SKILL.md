---
name: identity-investigation
description: Tools for identity-resolver investigations — why a listing took (or did not take) a film, base vs HEAD for a handful of listings, the ratchet's WRONG/UNJUDGED/LOST takes as a table, and evidence cards with suggested verdicts for judging UNJUDGED takes into labels.tsv. Use before writing an ad-hoc resolver test, downloading a recording, scraping a CI log for ratchet results, or researching takes by hand.
---

# Investigating an identity take

Four tools replace the mechanics that used to eat an investigation's hours. All of them read the
**unmatched-cluster capture** (`test/resources/fixtures/identity-unmatched/<cc>.json.gz`, a few MB per
country): a recording's clusters the model left unmatched, with every answer the resolver and the agreement
stage read for them. Replaying it is a pure function — no Mongo, no network, no 100–400 MB recording tree.

Downloads and per-ref compiles are cached in `$KINOWO_IDENTITY_CACHE` (default `~/.cache/kinowo-identity`),
shared by every worktree, so the second agent to ask pays nothing.

## Which tool

| Question | Command |
|---|---|
| Why did «title» at a venue take film X (or none)? | `scripts/identity-explain de --venue cinestar --title "rückkehr des königs"` |
| Did commit Y change that, and how? | `… --ref Y~1 --ref Y` (side by side, then the traces' diff) |
| The same on a newer capture (a capture PR, a capture run) | `… --fixture origin/chore/identity-capture` or `--fixture <identity-capture run id>` |
| What did a capture run's / PR's ratchet find? | `scripts/identity-ratchet-report 230` (PR) or `<run id>`; `--explain`, `--status ALL`, `--tsv` |
| …a local ratchet run | `sbt "worker/testOnly services.identity.UnmatchedClustersRatchetSpec"`, then `scripts/identity-ratchet-report --local` |
| Judge the UNJUDGED takes | `scripts/identity-judge-cards --fixtures origin/chore/identity-capture` → `target/identity-judge/` |
| Which listings did a change move, over a WHOLE corpus (matched ones too)? | the convergence row's identity takes check (`scripts/ci/identity-takes-check.sh`, `identity_takes_diff.py`) |

## identity-explain

Prints, per matching cluster: the billing (title, clean/search title, year, runtime, directors, catalogue ids)
and venue page facts, listings billed alike collapsed; the **captured** model decision; the cluster
**re-resolved by the code** (family keys and why, the chain into its family, every candidate with its
signals, each acceptance rule's refusal, the blocker); the **agreement stage** by the code (basis, fill rule,
guards passed, signals); the take(s) and their verdict in `labels.tsv`.

- Selectors are substrings, case and accents aside. At least one of `--venue`/`--title`.
- With `--ref`, each ref's tree is laid out once under the cache (sources by `git archive`, compiled classes
  seeded from this checkout by APFS clone) and compiled incrementally — ~1.5 min per new ref, then ~17 s a call.
  This checkout's `IdentityExplain.scala` is dropped into each ref, so a ref from before the tool works.
- Both refs replay the SAME capture: the comparison is the code's, never the input's.
- Model decisions are CAPTURED (whole-corpus resolve at capture time); a model-side change shows in the
  "re-resolved" section, which resolves the capture's listings alone (neighbours outside the capture missing).
- Not in the capture → the model matched it. Reproduce over the full corpus with `scripts/convergence-local.sh`
  (see the identity convergence local-repro memory).

## identity-ratchet-report

Reads the `identity-ratchet` artifact (`ratchet-takes.tsv`: status, country, venue, rawTitle, film, title,
basis, ids, was, explanation) that `UnmatchedClustersRatchetSpec` writes and the capture workflow uploads.
Statuses: WRONG, UNJUDGED, MOVED (an expected right take now takes another film; `was` names the old one),
LOST (takes none), NEW (right, not yet in expected-matches.tsv), RIGHT. A run from before the artifact
existed is parsed out of its pull-request job's log (no explanations then).

## identity-judge-cards

One card per (country, billed title, film taken): listing side vs record side (title, original title, year,
runtime, directors), the venue's catalogue ids, a synopsis snippet, why it was taken. Suggests a verdict ONLY
when decisive — RIGHT: venue catalogue id names it, or year ±1 and a shared director; WRONG: catalogue id
names another film of that database, runtime >30% off, a short billed as a feature, a record dated ≥2 years
after the year billed, or year AND directors both disagreeing. A card with reasons both ways stays open.

- Writes `cards.md`, `labels-draft.tsv` (decisive suggestions, each noted `— CONFIRM`) and `open.tsv`.
  It never touches `labels.tsv`: confirm a draft line, then copy it in.
- `--all` cards the judged takes too and prints how many decisive suggestions disagree with the labels —
  run it after changing the rules.
- Records the capture never read come from TMDB live when `TMDB_API_KEY` is set (`--live` reads it from the
  Touch ID vault); otherwise they stay unknown and the card stays open.

- It also shows each take's `SuspectTakes` reasons (fragment, era, runtime, sequel, feed id, weak fill route) —
  for the judge to weigh; they never decide a suggestion.
- Expect few decisive cards: most billings state no year or director (PR #230, 2026-10-13: 8 of 158 cards). The
  card's value is the evidence on one page; the suggestion is a bonus.

## What they replaced (measured 2026-10-13)

"Why did DE CineStar's «Herr der Ringe - Rückkehr des Königs (ext.)» take the 2025 short, and does 3e5a58e16 fix
it?" — without the tools: fetch recording 38363428661's DE tree (370 MB, 5 GB unpacked) 189 s, lay out and compile
the old commit fresh and run an ad-hoc spec 142 s — 331 s of mechanics for ONE ref, before writing the spec or
reading the answer. With them: `scripts/identity-explain de --venue cinestar --title "(ext.)" --fixture
origin/chore/identity-capture --ref 3e5a58e16~1 --ref 3e5a58e16` — 186 s cold for both refs, 37 s warm, and the
answer is on screen: `filled by venues.current … guards passed: …, title.anothersOwn …` before, no film after.

Judge cards for PR #230's 262 UNJUDGED takes (158 cards): ~40 s warm.
