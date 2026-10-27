# Identity experiments: tried, measured, not landed

A record of resolver changes that were built and measured but did not land, so the next attempt starts from what was
learned instead of from scratch. Not landing is not a verdict on the idea: most of these failed on one cost a slightly
different approach could avoid, and each entry says what that cost was and which angle looked worth trying next.

Each entry: what it tried, how it was measured, the result, why it stopped, and where the code is. Code that left a
branch is kept as a local annotated tag `experiment/<name>` (`git show experiment/<name>`, `git log main..experiment/<name>`).
**One file per entry** in `docs/identity-experiments/`, named `<YYYY-MM-DD>-<slug>.md` (the day it was tried) and
starting with a `# ` title line; `ls docs/identity-experiments/` lists them by date. Add a file whenever an experiment
is dropped or parked; edit an entry in place when one is retried. Entries are files, not sections of this page, so
agents adding entries in parallel never touch the same file (`SharedFileLayoutSpec`).
Whenever a fix touches the same listings, class, rule or stage as an entry here, go back through that entry first and
consider a new angle: a narrower scope, another signal, another stage, or what has changed on main since that could
avoid the cost that sank it. Try it, or say why not.

The trade rule they were judged by: zero new wrong takes; a right take may be lost only as a deliberate trade, up to
~100 per wrong one cured, each listed with evidence in `test/resources/fixtures/identity-judgements.tsv`.

## Production-shaped recordings: `prod-latest` and `scripts/identity-repro` (2026-10-23)

Four "prod-only, not reproducible" cases entered as PARKED below (DE The Thing With Feathers, DE Metropol "Rose", UK
Notre-Dame de Paris, PL Cinema City's Polish Peppa re-title) were prod-only because 573/528 are the 10-20 corpus, not
because prod's incremental state cannot be replayed. Forgejo starts scheduled runs with ref `main`, so no nightly
recording had published since 10-20; fixed, a nightly now publishes its tree and marks it `prod-latest-<cc>.txt`
(the hermetic pair, 573, moves only on a `pin-hermetic` dispatch). On prod-latest = run 1622 (recorded 10-23 05:24),
`scripts/identity-repro` slices each and takes exactly what prod serves: Feathers 1278923 (wrong), Notre-Dame 11594
(wrong), Rose 1712584, Peppa none (NoCandidate). On 573 the same slices took 1127628 / none / 846864 / not-in-recording.
**So these cases ARE reproducible and measurable now:** retry them on `--recording prod-latest` (identity-slice,
identity-measure, identity-ci-run) before calling a case prod-state only.

## Measuring-tool caveat (read before trusting a "moved nothing")

`scripts/identity-measure-local` (and CI's measure) replays the final stage over the checked-in capture only, which
holds the clusters the model left unmatched when it was recorded. A take the head newly refuses at the model stage never
reaches that replay: a final-stage 0 there means not seen, not refilled. Variants B–D below were first misread as
"moving nothing served" for this reason. Count model-stage losses as candidate served losses.

## Remaining wrong takes when round 2 stopped (37 on `d0d3ffb1c`)

Copied feed facts 10 · title piece or banner taken as the title 7 · bare exact title, top hit 6 · pooled family vote 5 ·
fill on a current release 3 · database agreement on a bare title 3 · relay, double bill, other 3. A long tail: each
looked like it needed case-level evidence rather than a class rule.
