# identity-slice: one listing's identity, reproduced in seconds

`scripts/identity-slice` answers "why does this listing take (or not take) this film, and does commit X change
that?" for ANY listing — matched or not, model or agreement — from a recording, without booting the pipeline.
`--explain` (alias `scripts/identity-explain`) replays only the unmatched-cluster capture; `scripts/convergence-local.sh` replays a
whole country through the pipeline (10–90 min). The slice sits between them.

## What a slice is

A directory, cut from ONE "Record scrape fixtures" recording (`--recording RUN`, default the newest green one):

| file | holds |
|---|---|
| `rows.json.gz` | the recording's own scrape rows for the slice's listings (`CorpusFixture` format) — made listings again by each ref's own code, so a title-rule or normaliser change shows |
| `slice.json.gz` | the seeds (selected listings), the core and context keys, the recording and its `recordedAt` |
| `http.jsonl.gz` | every HTTP request the decision made of the recording, with the recording's outcome: body, bytes, remembered failure, or GAP |

- **Core**: the seeds' whole families as the whole-corpus resolve formed them (title, search-form, original-title
  and matched-film block keys: `Resolution.familyOf`). Resolved with `IdentityResolver.resolveRegion` — the
  incremental model's own A3 seam: a family decided against the corpus context decides as the whole resolve does.
- **Context**: every other listing whose node carries a `CorpusContext.Reads` key a core node reads (title key,
  title/search group, title shape). Never resolved; the core's `CorpusContext` is derived from core + context, so
  `recurringSegment`, banner spread, title groups and same-titled reach read as over the whole corpus.
- **Answers at the HTTP seam** (`tools.SliceHttp`): recorded below every client, so the slice holds whatever the
  code at ANY ref asks — TMDB search/movie/alt-titles, IMDb suggestions and GraphQL, Wikidata, Metacritic, RT,
  Filmweb, Cinemeta, Letterboxd, posters' bytes, venue detail pages — and never an answer the recording lacks.
  A recording GAP replays as the gap (the lookup reads `Unknown`, as the leg reads it) and is printed UNANSWERED.
  A request the slice does not hold FAILS LOUDLY (exit 3, named), unless `--recording` is given to widen from.
- **Filed answers** (`tools.FiledAnswers`, `filed.json.gz`): a recording holds NO family answers — prod files them in
  `identity_family_answers`, and no hermetic leg replays them (every leg's takes dump holds zero Agreed/Filled/Poster
  takes). Without them the agreement stage agrees on nothing. The slice reads prod's filed family answers, IMDb finds
  and the capture's live-filled TMDB answers from an unmatched-cluster capture (`--families`, default this checkout's
  `identity-unmatched/<cc>.json.gz`), only where the recording has no answer, keeps exactly those it read, and prints
  them as `FILED, NOT THE RECORDING'S`.
- **Clock**: the recording's `recordedAt` (`prod-coverage-<cc>.json`): season, anniversary and wait rules read it.

## How it decides

1. The recording opened Mongo-free (`tools.RecordedCorpus`: tree → remembered verdicts → gap leaf — the chain
   `IdentityShadow` and the capture use).
2. Once per (recording, country, CODE): the whole corpus resolved (`IdentitySlicing.index`, ~25 s for DE idle, ~50 s
   under load) and kept as `index-<cc>-<code key>.json.gz` — each listing's family and each node's context keys. The
   code key (`code_key` in the script) hashes what resolves it: the ref's main code, `worker/src/fixtures`, the build,
   and this checkout's tool files dropped in — never the commit, so a commit or an edit changing no code (a judgement,
   a spec, a doc) reads the index another made. Measured 2026-10-15: recording 38547714686 held 252 indexes made under
   252 commits and worktree edits, 12 of them distinct.
3. Per slice: model (`IdentitySlicing.resolve`), then the agreement stage with its family question rounds, posters
   and catalogue fills answered through the same fetch (`IdentitySlice.decide`). Extraction decides through a NOTING
   fetch; what it read is the slice. A slice extracted at a second ref is merged (union), so one slice serves both.
4. Replay: the same decision through `SliceHttp.Replaying` — ~1 s a slice.

Each ref runs in ONE JVM with this checkout's tool files dropped into its tree (`identity_tree`, shared with
`--explain`), so a ref from before the tool works; both refs replay the same recording. Those files must therefore
compile against OLD code too — they may read nothing a ref from before them lacks (83b869548's `Capture.live` broke
every older ref until FiledAnswers read it off the file). `scripts/identity-slice --compiles REF` checks one;
`scripts/identity-slice-old-ref-test.sh` checks c2974e3aa (2026-10-12) — run it after changing a tool file.

## Modes

- two `--ref`s: side by side, then every moved listing with its labels.tsv verdict.
- `--batch FILE`: many slices in one JVM per ref, `--threads` at a time in the batch's order, moves tallied CURED / LOST /
  NEW-WRONG / WON / unjudged. Each slice's result is kept per (recording, ref, tool files, filed answers, the labels about
  its listings) and read back by every checkout (`--fresh` decides again); `--serve` keeps the ref's JVM, recording and
  index between batches.
- `--bisect A..B`: binary search over `rev-list --first-parent`, one ref compiled at a time.
- `--emit-case`: copies the slice to `test/resources/fixtures/identity-slices/` with the film labels.tsv calls right
  (never the current take; refused when unjudged); `IdentitySliceCasesSpec` replays every pinned slice.
- `--orders N`: the slice also fed to the INCREMENTAL model (`IncrementalResolver`, the worker's path) in N
  shuffled arrival orders, a few batches at a time, model and agreement — every listing whose take an order moves
  from the whole resolve's is named. The nondeterminism check.
- `--modes N`: the periodic projection's agreement against N CHANGED projections' — the same stage applied again
  after scrapes of a random half of the venues, every listing still read and the re-scraped venues' read again, as
  `IdentityProjection.tickChanged` hands it (`IdentitySlicing.changedRead`: the intake's `Read.Changed` keeps every
  venue it did not re-read) — every listing a changed projection flips is named.
- `--faithfulness capture`: the capture's agreement-taken listings (Agreed, Filled, Poster, Broadcast, Catalogue),
  as this code's agreement takes them over the captured model decisions, each sliced alone and compared — end to end,
  and the agreement stage alone over the captured model decisions.
- `--log '<json>'`: a prod take-change line → country, venue, listing, build.
- `--faithfulness RUN`: a sample of a convergence leg's listings, each sliced alone at the run's commit and pinned
  recording, compared with the leg's own final takes (`convergence-diagnostics-<cc>-convergence/<cc>-takes`).

## Faithfulness, and what it does not cover

The slice is faithful when its take equals the whole leg's. Where it is not, the harness says which: the leg's
cluster holds listings outside the core (widen: `--widen 1`), or same members and a different take (context the
slice lacks — corpus-wide facts such as houses and venue titles learned across the corpus, agreement neighbours).
Measured 2026-10-14: model takes against the convergence legs (run 38529260149), 298 of 300 (PL, DE, UK, ES, US;
the 2 UK RBO misses agree one ring wider). Agreement takes against the unmatched captures' replays (recording
38363428661), the stage alone over the captured model decisions 180 of 180; end to end 170 of 180 — DE's 10 misses
are listings the CAPTURED model decided with an IMDb suggestion the capture asked live and kept nowhere (its
recording gaps it, the capture does not hold it), so this code's model leaves them unanswered and the agreement waits.
Captures from 2026-10-14 on keep those answers (`live`: every TMDB/IMDb response the whole resolve read live,
`LiveGapLeaf.answered`), and the slice answers its recording's gaps from them (`FiledAnswers.LiveLeaf`): the next
capture closes the gap; re-measure with `--faithfulness capture` on it.

Each DIFF whose model moved now says why: `[code]` (every question answered — the code decides otherwise), `[a: new
question]` (the capture keeps its live answers and still lacks it — recapture), `[a or b]` (a capture without live
answers cannot tell), and with `--prod-mirror` a prod DIFF of a slice at another build than the deployed worker's is
marked `[c]`. Measured 2026-10-14, DE, 60 listings, recording 38363428661: at main 51/60 end to end, the stage alone
60/60, prod 38/58; at the capture's own code (abea3a168) the same 9 misses — all unanswered `imdb`/`imdb-titled`
questions, so (b) unkept live answers, not new questions; at the deployed worker (afa874093) 43/60, 53/60, 33/56. An
earlier 43/60 read at main was a stale index: every checkout's own-code index was one shared
`index-<cc>-worktree` file (now keyed by checkout, commit and uncommitted edits).

UK CBeebies Panto (ca4059b55..9c8170f5e): without filed answers 0 moves (the recording's families answer nothing —
the CI view); with them ca4059b55 takes tmdb 1710881 by Agreed at 210 venues (WRONG) and both b5e156555 and
9c8170f5e drop all 210 — the forensics diff's 211 and prod's 213. At b5e156555 the first `--modes 4` flipped all 210
back to 1710881 in one changed projection — an artefact: it handed the changed stage only the re-scraped venues'
listings, so the cluster was digested and decided again on half of them, which no worker does (the intake's changed
read keeps every venue's listings; IdentityListingIntakeProjectedSpec). Fixed in `IdentitySlicing.changedRead`.

## Prod's state: `--prod-mirror`

`--prod-mirror` reads prod's own state from the LOCAL prod mirror (`scripts/local-mirror`, `<db>_prod_mirror` on
:28017; `tools.ProdMirror` refuses any other host — never prod): its filed family answers (`identity_family_answers`)
in preference to the capture's, the agreement stage's stored verdicts (`identity_agreements`, which the stage starts
from as prod's does: a verdict stands only while its listings, rules and every answer it read do) and the operator pins
(`identity_pins`, handed to the model). Beside each take it prints prod's stored take and why (`PROD …`: the model's
stored decision, the trace, the agreement verdict and correction, the stored titles) and `PROD TAKES OTHERWISE` where
they differ; with `--faithfulness` it also tallies the slice against prod's take now. `scripts/local-mirror/freshness`
runs first: a mirror synced before the recording, or more than `KINOWO_MIRROR_MAX_LAG_HOURS` (6) behind, is WARNed — the
slice mixes the recording's moment with the mirror's; no mirror at all refuses.

Measured 2026-10-14 (recording 38363428661, the capture of be6cc4142b59), `--faithfulness capture --sample 60`: DE 50 of 60
with and without it, UK 60 of 60. The DE misses are model-side — the captured model read an IMDb suggestion live that the
recording gaps — and family answers do not reach them (the next capture's live answers do, 83b869548). The tally against
prod's take (DE 12 of 57, UK 4 of 60) is not yet meaningful: the mirror held no `identity_agreements` (added in 692e96ee6,
synced once the daemon's checkout has it), so prod's take of every model-unmatched listing read as none.

Not covered: the projection itself (film ids, slots) — the slice decides takes, not films; the stored model a pod
restores, and its arrival order. A move prod logs that the slice does not reproduce at either ref, with `--prod-mirror`,
is that state's, not the code's.
