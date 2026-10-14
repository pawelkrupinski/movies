---
name: identity-investigation
description: Tools for identity-resolver investigations — why a listing took (or did not take) a film, what prod's projection moved and why (LogsQL over the take-change log fields), base vs HEAD for a handful of listings, the ratchet's WRONG/UNJUDGED/LOST takes as a table, and evidence cards with suggested verdicts for judging UNJUDGED takes into labels.tsv. Use before writing an ad-hoc resolver test, downloading a recording, scraping a CI log for ratchet results, or researching takes by hand.
---

# Investigating an identity take

Four tools replace the mechanics that used to eat an investigation's hours. All of them read the
**unmatched-cluster capture** (`test/resources/fixtures/identity-unmatched/<cc>.json.gz`, a few MB per
country): a recording's clusters the model left unmatched, with every answer the resolver and the agreement
stage read for them. Replaying it is a pure function — no Mongo, no network, no 100–400 MB recording tree.

Downloads and per-ref compiles are cached in `$KINOWO_IDENTITY_CACHE` (default `~/.cache/kinowo-identity`),
shared by every worktree, so the second agent to ask pays nothing.

## Which tool

**Start with `scripts/identity-slice`** for any one listing — matched or not, model or agreement. It cuts the listing's
families and their corpus context out of a recording, with every answer they read, and decides them at each ref in
~1 s (the first call on a recording resolves its corpus whole once per ref, ~30 s; a new ref compiles once, ~2 min).
Design and limits: docs/design/identity-slice.md.

| Question | Command |
|---|---|
| Why does «title» at a venue take film X (or none) — matched listings too? | `scripts/identity-slice de --venue cinestar --title "rückkehr des königs"` (or `--key 'venue\|raw title'`) |
| Did commit Y change that? | `… --ref Y~1 --ref Y` — both refs on ONE recording (`--recording RUN`, default the newest green) |
| A prod take-change log line | `scripts/identity-slice --log '<the line's JSON>'` (its `repro` field is the same command) |
| Which commit of A..B moved it? | `… --bisect A..B` |
| Measure a rule change over many listings | `scripts/identity-slice de --batch FILE --ref BASE --ref HEAD [--labels TSV]` → cured / lost / new-wrong |
| Pin a repro as a failing test | `… --emit-case` → `test/resources/fixtures/identity-slices/`, `IdentitySliceCasesSpec` (expected film from labels.tsv only) |
| Does arrival order (the worker's incremental model) move it? | `… --orders 12` |
| Does the slice decide as the whole leg did? | `scripts/identity-slice de --faithfulness <convergence run id> --sample 60` |
| Why did «title» at a venue take film X (or none)? | `scripts/identity-explain de --venue cinestar --title "rückkehr des königs"` |
| Did commit Y change that, and how? | `… --ref Y~1 --ref Y` (side by side, then the traces' diff) |
| The same on a newer capture (the capture branch, a capture run) | `… --fixture origin/chore/identity-capture` or `--fixture <identity-capture run id>` |
| What did a capture run's ratchet find? | `scripts/identity-ratchet-report <run id>` (or a capture PR's number); `--explain`, `--status ALL`, `--tsv` |
| Land a judged capture | commit the verdicts to `chore/identity-capture`, push, then `gh workflow run identity-capture.yml -f open-pr=true` — re-runs the ratchet there and opens the PR only if it is green |
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
- Not in the capture → the model matched it: use `scripts/identity-slice` (any listing, model decisions re-resolved by
  the code against the whole corpus's context). `scripts/convergence-local.sh` only for what the pipeline itself does.

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

## Querying take-change logs (live, VictoriaLogs)

What prod's projection moved, and why, without a capture. The worker logs JSON, which Vector turns into
fields (`tools.JsonLineEncoder`, `services.identity.TakeChangeLines`), so filter and group on FIELDS, never a
regex over `_msg`. Every worker line carries `country`, `build` (the image's commit) and, inside a projection,
`projection_id` + `projection_mode` (whole / periodic / changed). Lines logged before 2026-10-14 are text
only and have none of these.

- **`identity take change: <kind> …`** — one per listing whose TMDB film moved (up to 2,000 a tick): `kind`
  (lost/changed/gained), `venue`, `listing`, `film_before`/`film_after`, `tmdb_before`/`tmdb_after`,
  `title_before`/`title_after`, `basis`, `confidence`, `reason_first`/`reason_last`, `rules` (accept:/pooled:/
  veto:/join:/apart:/refused: ids), `blocker`, `runner_up`, `fallback`, `unanswered`, `previous_basis`/
  `previous_confidence`/`previous_reason` (the take before; absent after an index rebuild), `listing_year`/
  `listing_runtime`/`listing_director`/`listing_original_title`, and `repro` — a ready
  `scripts/identity-slice <cc> --key 'venue|raw title' --ref <build>` line (any listing, matched or not; lines logged before
  2026-10-14 name `scripts/identity-explain`, which finds the listing only if the capture holds it unmatched).
- **`identity take change: summary …`** — one per tick that moved any: `take_changes`, `lost`, `changed`,
  `gained`, `logged`, `by_basis`.
- **`identity projection: …`** — every written tick: `listings`, `films`, `written`, `retired`, `declined`,
  `take_changes`, `seconds`, `scoped`, `retired_films`, `declined_films` (first 50 ids).
- **`identity suspect takes: counted …`** — `suspected` and `suspect_<signal>` per signal, under the
  projection's id.

Run from a shell (or paste the query into https://logs.kinowo.net):

```
q() { ssh monitoring-1 "curl -s http://10.20.0.11:9428/select/logsql/query --data-urlencode 'query=$1' --data-urlencode limit=${2:-50}"; }
```

| Question | LogsQL |
|---|---|
| What moved, by kind and basis, per country | `_time:24h kind:* \| stats by (country, kind, basis) count() n \| sort by (n desc)` |
| Lost takes in one country, by basis | `_time:24h country:uk kind:lost \| stats by (basis) count()` |
| …and what took them before | `_time:24h country:uk kind:lost \| stats by (previous_basis, basis) count()` |
| The last 20 changes at a venue | `_time:7d kind:* venue:="Cineworld Leigh" \| sort by (_time desc) \| limit 20 \| fields _time, kind, listing, tmdb_before, tmdb_after, basis, previous_basis, repro` |
| Every change of one film | `_time:7d kind:* (tmdb_before:=550 or tmdb_after:=550) \| fields _time, country, venue, listing, kind, basis` |
| Changes since deploy X | `_time:24h build:=<sha> kind:* \| stats by (country, kind, basis) count()` |
| Before vs after a deploy | `_time:3d country:pl kind:* \| stats by (build, kind) count()` |
| Which rule took (or refused) them | `_time:24h kind:gained \| stats by (rules) count() n \| sort by (n desc) \| limit 20` |
| One tick, whole | `projection_id:="2026-10-14T09:12:03.120Z" \| fields _time, _msg` (any line's `projection_id`) |
| Tick sizes over a day | `_time:24h "identity take change: summary" \| fields _time, country, take_changes, lost, changed, gained, logged, by_basis` |
| Retired / declined films | `_time:24h "identity projection:" retired:>0 \| fields _time, country, retired, retired_films, declined_films` |
| Suspect-take counts | `_time:24h "identity suspect takes: counted" \| fields _time, country, suspected, suspect_fragment, suspect_era, suspect_runtime` |

`\|` is the table's escape — type `|`. A field missing from a line (`tmdb_after` on a `lost`) is absent, not
empty; `kind:*` keeps only take-change lines (the summary has no `kind`).

## What they replaced (measured 2026-10-13)

"Why did DE CineStar's «Herr der Ringe - Rückkehr des Königs (ext.)» take the 2025 short, and does 3e5a58e16 fix
it?" — without the tools: fetch recording 38363428661's DE tree (370 MB, 5 GB unpacked) 189 s, lay out and compile
the old commit fresh and run an ad-hoc spec 142 s — 331 s of mechanics for ONE ref, before writing the spec or
reading the answer. With them: `scripts/identity-explain de --venue cinestar --title "(ext.)" --fixture
origin/chore/identity-capture --ref 3e5a58e16~1 --ref 3e5a58e16` — 186 s cold for both refs, 37 s warm, and the
answer is on screen: `filled by venues.current … guards passed: …, title.anothersOwn …` before, no film after.

Judge cards for PR #230's 262 UNJUDGED takes (158 cards): ~40 s warm.
