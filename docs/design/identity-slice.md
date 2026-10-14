# identity-slice: one listing's identity, reproduced in seconds

`scripts/identity-slice` answers "why does this listing take (or not take) this film, and does commit X change
that?" for ANY listing — matched or not, model or agreement — from a recording, without booting the pipeline.
`scripts/identity-explain` replays only the unmatched-cluster capture; `scripts/convergence-local.sh` replays a
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
2. Once per (recording, country, ref): the whole corpus resolved (`IdentitySlicing.index`, ~25 s for DE) and kept
   as `index-<cc>-<ref>.json.gz` — each listing's family and each node's context keys.
3. Per slice: model (`IdentitySlicing.resolve`), then the agreement stage with its family question rounds, posters
   and catalogue fills answered through the same fetch (`IdentitySlice.decide`). Extraction decides through a NOTING
   fetch; what it read is the slice. A slice extracted at a second ref is merged (union), so one slice serves both.
4. Replay: the same decision through `SliceHttp.Replaying` — ~1 s a slice.

Each ref runs in ONE JVM with this checkout's tool files dropped into its tree (`identity_tree`, shared with
identity-explain), so a ref from before the tool works; both refs replay the same recording.

## Modes

- two `--ref`s: side by side, then every moved listing with its labels.tsv verdict.
- `--batch FILE`: many slices in one JVM per ref, moves tallied CURED / LOST / NEW-WRONG / WON / unjudged.
- `--bisect A..B`: binary search over `rev-list --first-parent`, one ref compiled at a time.
- `--emit-case`: copies the slice to `test/resources/fixtures/identity-slices/` with the film labels.tsv calls right
  (never the current take; refused when unjudged); `IdentitySliceCasesSpec` replays every pinned slice.
- `--orders N`: the slice also fed to the INCREMENTAL model (`IncrementalResolver`, the worker's path) in N
  shuffled arrival orders, a few batches at a time, model and agreement — every listing whose take an order moves
  from the whole resolve's is named. The nondeterminism check.
- `--modes N`: the periodic projection's agreement (every listing read) against N CHANGED projections' — a fresh
  stage over the periodic one's stored verdicts, reading only the listings at a random half of the venues, as
  `IdentityProjection.tickChanged` hands it — every listing a changed projection flips is named.
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

UK CBeebies Panto (ca4059b55..9c8170f5e): without filed answers 0 moves (the recording's families answer nothing —
the CI view); with them ca4059b55 takes tmdb 1710881 by Agreed at 210 venues (WRONG) and both b5e156555 and
9c8170f5e drop all 210 — the forensics diff's 211 and prod's 213. At b5e156555, `--modes 4` flips all 210 back to
1710881 in one changed projection: a take that depends on which listings a projection reads.

Not covered: curation pins (prod Mongo, not in a recording); the projection itself (film ids, slots) — the slice
decides takes, not films; prod's own state between ticks (the stored model a pod restores, family answers filed after
the capture). A move prod logs that the slice does not reproduce at either ref, with the filed answers, is that
state's, not the code's.
