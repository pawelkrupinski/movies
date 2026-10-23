---
name: identity-investigation
description: Tools for identity-resolver investigations — why a listing took (or did not take) a film, what prod's projection moved and why (LogsQL over the take-change log fields), base vs HEAD for a handful of listings, the ratchet's WRONG/UNJUDGED/LOST takes as a table, and evidence cards with suggested verdicts for judging UNJUDGED takes into identity-judgements.tsv (the one file every identity judgement lives in). Use before writing an ad-hoc resolver test, downloading a recording, scraping a CI log for ratchet results, or researching takes by hand.
---

# Investigating an identity take

**Before building a resolver change, read `docs/identity-experiments.md`**: what was already tried, measured and not
landed, why, and where its code is kept (`experiment/<name>` tags). When one touches what you are fixing, revisit it
and consider a new angle on the cost that sank it. Add an entry whenever you drop or park one.

The tools below replace the mechanics that used to eat an investigation's hours. `identity-slice` (and
`venue-replay`, `recording-diff`, `listing-history --replay`) read a **recording**; `identity-slice --explain`,
`identity-judge-cards` and the ratchet report read the **unmatched-cluster capture** (`test/resources/fixtures/identity-unmatched/<cc>.json.gz`, a few MB per
country): a recording's clusters the model left unmatched, with every answer the resolver and the agreement
stage read for them. Replaying it is a pure function — no Mongo, no network, no 100–400 MB recording tree.

Downloads and per-ref compiles are cached in `$KINOWO_IDENTITY_CACHE` (default `~/.cache/kinowo-identity`),
shared by every worktree, so the second agent to ask pays nothing.

**Data sources.** The local prod mirror (`mongodb://127.0.0.1:28017`, databases `kinowo*_prod_mirror`) holds prod's
current state — `identity_traces`, `identity_model_families`, `movie_slots`, `screenings`, … — and may be read
READ-ONLY (never `:27017`, never prod itself); check it with `scripts/local-mirror/freshness <cc>` first. A tool's
default recording is the newest green "Record scrape fixtures" run that holds the country's corpus; a recording it
cannot fetch fails the tool, it never falls back to another. Name these tools in every subagent brief — they replace
general convergence runs.

## Which tool

**Start with `scripts/identity-slice`** for any one listing — matched or not, model or agreement. It cuts the listing's
families and their corpus context out of a recording, with every answer they read, and decides them at each ref in
~1 s (the first call on a recording resolves its corpus whole once per ref, ~30 s; a new ref compiles once, ~2 min).
Design and limits: docs/design/identity-slice.md.

| Question | Command |
|---|---|
| Why does «title» at a venue take film X (or none) — matched listings too? | `scripts/identity-slice de --venue cinestar --title "rückkehr des königs"` (or `--key 'venue\|raw title'`) |
| Did commit Y change that? | `… --ref Y~1 --ref Y` — both refs on ONE recording (`--recording RUN`, default the newest green recording that holds the country's corpus) |
| A prod take-change log line | `scripts/identity-slice --log '<the line's JSON>'` (its `repro` field is the same command) |
| Which commit of A..B moved it? | `… --bisect A..B` |
| Measure a rule change over many listings | `scripts/identity-slice de --batch FILE --ref BASE --ref HEAD [--labels TSV]` → cured / lost / new-wrong (parallel, results shared — see "Fast batches") |
| Pin a repro as a failing test | `… --emit-case` → `test/resources/fixtures/identity-slices/`, `IdentitySliceCasesSpec` (expected film from the judgements only) |
| A pinned case fails "lacks answers" after a resolver change (its listings now ask more) | `scripts/identity-slice-refresh [--recording RUN] <case dir>...` — re-emits each case with every listing its expected.tsv names, in place, its hand expected.tsv kept |
| Does arrival order (the worker's incremental model) move it? | `… --orders 12` |
| Does a CHANGED projection take it otherwise than the periodic one? | `… --modes 4` |
| Agreement takes (Agreed, Filled, Poster): read prod's filed family answers | default `--families` = this checkout's capture; without them the families agree on nothing (as in every hermetic leg) |
| What does PROD take for it now, and why — beside the slice's take? Prod's filed answers newer than the capture's, its stored agreement verdicts, its pins | `… --prod-mirror` — reads the LOCAL prod mirror (:28017, never prod), prints `PROD …` lines (model decision, trace, agreement verdict, correction, stored titles) and `PROD TAKES OTHERWISE`; checks `scripts/local-mirror/freshness` first and WARNs when the mirror predates the recording or lags >6 h. Use it when a take is an agreement one, or prod moved a listing the plain slice does not reproduce |
| Is the mirror current enough to trust? | `scripts/local-mirror/freshness [cc] [--not-before <recordedAt>]` — sync time and lag per country, per-collection counts; exit 3 = not fresh |
| Does the slice's agreement decide as the capture's replay? | `scripts/identity-slice uk --faithfulness capture --sample 60` |
| Does the slice decide as the whole leg did? | `scripts/identity-slice de --faithfulness <convergence run id> --sample 60` |
| Why did «title» at a venue take film X (or none) — the whole trace, from the capture? | `scripts/identity-slice de --explain --venue cinestar --title "rückkehr des königs"` |
| Did commit Y change that, and how? | `… --ref Y~1 --ref Y` (side by side, then the traces' diff) |
| The same on a newer capture (the capture branch, a capture run) | `… --fixture origin/chore/identity-capture` or `--fixture <identity-capture run id>` |
| What did a capture run's ratchet find? | `scripts/identity-ratchet-report <run id>` (or a capture PR's number); `--explain`, `--status ALL`, `--tsv` |
| Land a judged capture | commit the verdicts to `chore/identity-capture`, push, then `gh workflow run identity-capture.yml -f open-pr=true` — re-runs the ratchet there and opens the PR only if it is green |
| …a local ratchet run | `sbt "worker/Test/runMain scripts.UnmatchedClustersRatchet"` (exit 1 names each failed check; `--measure` reads a capture's gaps as unknown), then `scripts/identity-ratchet-report --local` |
| Judge the UNJUDGED takes | `scripts/identity-judge-cards --fixtures origin/chore/identity-capture` → `target/identity-judge/` |
| Which listings did a change move, over a WHOLE corpus (matched ones too)? Is it landable? **Run before landing any resolver change** — a `--batch` of chosen listings missed 26e48897d's lost PL "Lalka" and a083's UK "Fallen Angels" | **The default (CI, ci-runner):** `scripts/identity-measure [<commit> \| --head REF] [--base SHA] [--recording RUN] [--countries pl,uk] [--json] [--plan] [--all-recordings \| --one-recording] [--all-countries]` — measures only the countries the change's FOOTPRINT can move (without `--countries`: `services/cinemas/<cc>/`, a `<cc>` capture, a title-words row's language; shared code reaches all), over ONE recording, and the other recordings (`IDENTITY_MEASURE_RECORDINGS`) only when that one is LANDABLE and takes moved in 2+ countries; it collects as soon as the takes rows end (not the run's tail), so dispatch with `--no-wait`, keep working, and `--run <id>` later. Pushes a temporary `measure/<user>/<sha>-<rand>` branch (on Forgejo the run deletes it as its last job; a Ctrl-C cannot cancel the run there), dispatches only the convergence suite's `takes` rows of the countries named (`takes-only`, `takes-countries`; ~25-40 min wall, base vs head over the whole recording). `--recording RUN` replays that `Record scrape fixtures` run's bare pair (its own tree, no fills: what identity-measure-local lays) instead of the pinned one, and is never reused, then prints per country and stage (model / final takes) lost / changed / gained split by the judgements: cure, right-lost, right→wrong, wrong→wrong (target known-wrong / undecided / open), new-wrong, gained right / unjudged, accepted; 3 examples a bucket, EVERY unjudged move, each takes row's pass/fail, then on stderr where the time went (the lane wait, each takes job, each row's phases from the check's `timings.tsv`, `reused` when a stored base spared one -- a base any measurement resolved is stored for the next), and LANDABLE (no right-lost, right→wrong or new-wrong) or NOT LANDABLE with reasons. `--no-wait` then `--run <id>` to collect: it waits while the run is in progress (at most `--timeout`; `--no-wait` exits 2 instead), so no poll loop of your own, and re-classifies with your newer judgements too. **Reuses** an earlier run (no push, no dispatch; prints `REUSED measurement of run <id>`) when the change's `git patch-id --stable` over `.github/convergence-paths.txt` files is the same and main moved only outside those paths since its base (judgements aside: collecting re-classifies with the head's), within 24 h (`IDENTITY_MEASURE_REUSE_HOURS`); `--fresh` forces a dispatch. **Result cache:** every verdict (its table, exit, the countries that moved) is stored on the `convergence-fixtures` release as `measure-result-<key>.json`, key = (head sha, base sha, recording, countries, mode takes/full/footprint, hash of the measure tooling); the same request again, from any clone, prints `CACHED verdict of run <id>` in seconds with no push or dispatch (pinned-pair verdicts for `IDENTITY_MEASURE_CACHE_HOURS`, default 24); `--no-cache` forces a run; a `--no-wait` measurement is stored by its `--run`. Built on the takes check: `scripts/ci/identity-takes-check.sh`, `identity_takes_diff.py`, the rows' `identity-takes-<cc>` artifacts |
| Will this identity change LAND GREEN on Main? (**run before every identity push** — the measure alone called six of 2026-10-22's Main reds LANDABLE) | `scripts/identity-landing-check [<commit> \| --head REF] [--base SHA] [--full \| --footprint-only] [--local] [--no-measure \| --no-specs] [--no-cache]` — the measure (`identity-measure --footprint`, which falls back to the full measure; `--full` to land on) plus, in parallel, exactly the identity specs Main's rows run: `unit` (every `services.identity.*` in worker and identity — MatchedTakesLabelSpec, UnmatchedClustersRatchetSpec, IdentitySliceCasesSpec —, `services.titlerules.*`, worker `scripts.*`, DocCommentPlacementSpec, HardClusterRatchetSpec), `it` (HardClusterConvergenceIntegrationSpec), and when `scripts/ci/ci-row-scope.sh e2e` says Main runs the e2e rows, `e2e-corpus` (FilmScheduleEndToEndSpec) and `e2e-rest` (SearchQueryMarkersSpec, IdentityCutoverEndToEndSpec). Each group is one `scripts/identity-ci-run` at the commit, all at once (`--local`: sbt-warm / it-local.sh, one after another, under heavy-run). Prints `LANDABLE` or `BLOCKED` with each failing spec, its test and a hint (MatchedTakesLabelSpec "known-wrong in scope rows" → judgement rows stale → known-wrong scope rows; IdentitySliceCasesSpec → the change regresses a pinned listing, or the case asks unpinned questions → re-pin from the recording; the ratchet → copy expected-matches.tsv; DocCommentPlacement → move the Scaladoc). Logs under `target/identity-landing-check/<sha>/`. Exit 0 / 1 / 2 (could not check) |
| Did my TITLE-RULE change move any take at all? (while iterating — **never to land**: landing needs the full measure above) | `heavy-run scripts/identity-measure --footprint [--head REF] [--base SHA]` — for a change touching ONLY `title-rules.tsv`, `services/titlerules/` (not `TitleRulePlaceholders.scala`: FormatTags reads it directly) or `TitleNormalizer.scala`. Every takes check stores each side's NORMALIZER TRACE (every title reading its resolve and ratchet made: `takes-trace-<cc>-<identity code>-p<run>` on the release); this replays the base's under your rules (`services.movies.NormalizerTraceCheck` in your head's tree, ~1 min, no runner; `--footprint-only` never dispatches). All alike ⇒ the resolve reads, and so decides, exactly as the base did: `measure: footprint`, LANDABLE, no dispatch (sound for the trace's recording, its fills aside). Anything else falls back to the full measure by itself, saying why: a changed file outside the title rules (matcher/scoring code: no trace says what it decides, so no footprint), no stored trace of the base's identity code, or readings that read otherwise — listed (country, method, title, base, head): takes MAY move, and only the full measure says which. `--traces DIR` takes a base's traces from an `identity-ci-run` of `IdentityResolveDump --trace`/`UnmatchedClustersRatchet --trace` instead. Validated 2026-10-22 against the full measure: docs/investigations/2026-10-22-footprint-measure.md |
| Any other heavy sbt command over a cached recording, on CI (an `IntegrationTest/runMain`: a resolve dump, an ablation, a simulation, a replay sweep) | `scripts/identity-ci-run --recording RUN [--countries pl,de] [--head REF] [--heap 8g] -- '<sbt command>'` — `.github/workflows/identity-run.yml`: lays the recording as `identity-fetch-recording.sh` does, starts a throwaway MongoDB, runs the command per country when it names `{cc}` (placeholders `{cc}` `{fixtures}` `{corpus}` `{out}`), and downloads `{out}` (the `identity-run` artifact) into `target/identity-ci-run/<run>`; ~5 min of setup plus the command |
| Will the convergence `takes` rows skip my title-rule commit? Is the skip gate sound? | They do not yet: the gate (`scripts/ci/takes-skip-gate.sh`) runs in SHADOW on every takes row -- the takes still run, and the gate's SKIP/RUN is posted as the status `takes skip gate (<cc>)` (a SKIP where a take moved fails the row: the gate is unsound). SKIP needs every condition: only title-rule files (or docs/Markdown) changed, no identity/matcher/resolver/projection path, the trace tooling unchanged, the base's trace under its exact inputs (`takes-tracex-<cc>-<key>`), and the proof (`NormalizerTraceCheck` via `scripts/ci/takes-skip-proof.sh`) reading `0 read otherwise, 0 could not be made again`; any error or missing input is RUN. `GH_REPO=pawelkrupinski/movies scripts/ci/takes-skip-shadow-report.sh [main] [commits]` tallies the statuses: READY at >= 20 SKIPs and 0 disagreeing, the bar for `takes-skip: enforce`. Enforced, it still runs shape and drift, samples 1 in 10 SKIPs in full, runs a country in full when its last full takes row on main is over 24 h old, and `convergence-daily.yml` runs main in full daily. It never replaces the full measure an identity change LANDS on |
| FALLBACK ONLY (what the CI route cannot do yet), and only under the shared 3-slot heavy-run wrapper: the same whole-corpus verdict on the laptop | `scripts/identity-measure-local --base REF [--head REF] [--countries …] [--recording RUN]` — the takes rows' resolve (`IdentityResolveDump`), the ratchet's final takes and `identity_takes_diff.py`, each ref in a worktree of its own on a mongod of its own (`scripts/it-local.sh`), over a recording already laid in the cache; ~5 min a ref for all five countries (2026-10-17). No stored base, no corpus-shape specs: `scripts/identity-measure` stays the verdict of record. Local legs offline: `CONVERGENCE_LOCAL_PAIR=<run> CONVERGENCE_LOCAL_OWN_MONGOD=1 scripts/convergence-local.sh --i-need-a-local-leg <cc>` |
| Did a change make a NEW suspect take or corpus-shape finding? | the same check, `takes` row of a hermetic leg: base vs head on one recording; new = fails, standing = listed in the step summary, cured = counted (`identity_takes_diff.py` suspects, `corpus_shape_diff.py`) |
| What is the newest recording's own untriaged backlog (suspects, wrong takes, markers in queries, slot shapes)? | the **Identity findings** issue of the country (`gh issue list --search 'in:title "Identity findings"'`), updated by `.github/workflows/identity-findings-report.yml` after every recording, each entry with its commands; red after 3 days untriaged. Not a red-build gate: triage it, never block a push on it |
| Which titles does one country take as a film no other country takes, billed the same week (DE «Fallen Angels by Noël Coward» on the 1995 Wong Kar-wai film beside the UK's Coward relay)? | `python3 -I scripts/ci/countries_disagree.py --mirror mongodb://127.0.0.1:28017 --data <dir> [--country de]` (~30 s, after `scripts/local-mirror/freshness`) — the findings report's **Countries disagree** section: one line a title, `judge` (a take unjudged) or `check` (a take judged wrong), the lone take(s) first, the others after `\| vs`; titles all judged right, all relays/events (`cross_country.EVENT_RULES`) or whose billing states a year/director backing its take are left out. `identity_findings_report.py --countries-disagree <dir>` adds it to each country's issue (CI has no mirror: only a run given the export carries it) |
| Would re-tuning the rules' numbers (cuts, cannot-link bounds, signal weights) take more right with nothing wrong — the weekly refit, by hand? | `secrets run movies TMDB_API_KEY -- scripts/identity-refit [--skip-checks] [--relearn year.delta] [--threads N]` in your own worktree — the refit's spec, every bounded step measured on the five captures (kept ones written into `identity-weights.json`), and when one is kept the identity specs, the ratchet, the hard clusters and both snapshot layers regenerated; report `target/identity-refit/report.md`. CI's `identity-refit.yml` runs the same script |
| Why does a raw title clean / search / shape the way it does? Which title rule fired? | `scripts/title-trace "<raw title>" --country de [--venue X]` — every step, each rule's name, before and after (below) |
| Did commit Y change how a title cleans, searches or shapes? | `scripts/title-trace "<raw title>" --country de --ref Y~1 --ref Y` — both traces, then the steps that moved |
| Is a venue's client reading a field wrong (synopsis, director, runtime, year, title, format)? What did one venue scrape? | `scripts/venue-replay pl "ziemi rybnickiej" [--title trzy] [--fields]` — every listing, its clean title and search, fields, detail page re-read, shape checks (below) |
| Did commit Y change what a venue's client reads? | `scripts/venue-replay … --ref Y~1 --ref Y` (+ `--fixtures <client tree>` for pages the recording lacks, `--scrape` to re-run the listing parser) |
| …and what does prod hold for it right now? | `… --prod` — the local mirror's cinema_scrapes / venue_pages / identity_listings beside the replay |
| What do the judgements (`identity-judgements.tsv`: verdict, known-wrong / accepted state, scope, reason) and the expected matches say about a listing or film, who added each, and does prod still take it so? | `scripts/label-lookup uk --title "RBO: Romeo & Juliet" [--venue X]` or `scripts/label-lookup uk --film tmdb:N` (below) |
| Code or new data? What changed between two recordings (a red recording, a leg that moved)? | `scripts/recording-diff <runA> <runB> --country de [--venue X] [--title Y]` (below) |
| When and why did prod's take of a listing change, per deploy? Would CI have reproduced it? | `scripts/listing-history uk --title "ties that bind" [--venue X] [--since 7d] [--replay]` (below) |

## Fast batches, fast iteration

- **Batches run in parallel** in one JVM per ref: `--threads N` slices at once (default half the cores, at most 8);
  the output and `--out` keep the batch's order, the same takes as a serial run.
- **Results are shared.** Each slice's result is kept in
  `$KINOWO_IDENTITY_CACHE/slice/<run>/results/<code key>-<filed answers>/`, named by its selector, seeds,
  `--widen/--orders/--modes` and the labels about its listings (a label of another billing does not invalidate it).
  Any checkout replaying the same base (origin/main) reads it back. `--fresh` decides again; `--prod-mirror` and
  `--emit-case` never read or keep one.
- **`--serve`** keeps a JVM per (ref, recording, country) with the recording opened and the ref's index loaded, and
  sends each batch to it (no JVM start or corpus load per call). Each kept JVM holds a JVM slot for its life (a keeper
  beside it; a call reaching a live one takes none); it stops after `KINOWO_IDENTITY_SLICE_IDLE_MINUTES` (10) minutes
  without a request, `scripts/identity-slice --stop-served` stops them all now. Worth it for many small batches.
- Measured 2026-10-14, DE, recording 38547714686, refs c79eb901c → 4c0777340, load average ~150: the old serial
  tool, 500 listings at both refs 937 s (≈78 min for 2,500); 2,500 listings (916 slices) at both refs 691 s, the
  same takes; the same batch again (another checkout, results kept) 33 s. A 20-listing fresh batch: 25 s, 15 s
  through a kept `--serve` JVM.
- **A new ref is cheap.** The whole-corpus index and the results are keyed by the CODE a ref runs, not its commit
  (`code_key`): a commit changing no code reads what another made. A new ref's compile is seeded from the nearest
  compiled ref in the cache; one with no source changed keeps that ref's classpath, no sbt at all. Measured 2026-10-15,
  DE, recording 38666576506, a fresh worktree: a new ref's first slice 151 s (a full 1,766-file compile 125 s, index
  23 s) → 88 s with new code under load 105 (5 files recompiled, 30 s; index 47 s) → 20 s for a commit changing no code
  (index reused).
- **`--emit-case` goes through a kept JVM** (`--serve`, started when none is; `KINOWO_IDENTITY_SLICE_EMIT_SERVE=0`
  opts out): 8-11 s a call before, now ~11 s for the first and ~2 s for each after (measured 2026-10-15). An edit of
  this checkout's code stops the kept JVM of its older state.
- **Iterating on resolver code:** one spec through the kept sbt server (`scripts/sbt-warm "identity/testOnly X"`,
  test-layers skill) and a handful of listings (`--key`) while iterating; the whole identity suite
  (`scripts/sbt-warm identity/Test/test`) and one `--batch` at BASE and HEAD once, before pushing.

## title-trace

`scripts/title-trace "<raw title>" --country <cc> [--venue X] [--ref A [--ref B]]` (or `--batch FILE` of
`country<TAB>venue<TAB>raw title` lines, one JVM per ref for all of them) — no recording, no network: one
JVM over the code alone (a ref compiled once into the shared cache, as identity-slice does; ~5 s warm). Prints, in
production order, each step's own answer:

1. **ingest** — `TitleText.tidy`, the venue's own rules (`--venue`: a roster name or a unique part of it; an unknown
   name gets none), the format peel (`FormatTags.extractFormatTags`: tags, a filed article fronted) → the slot title;
2. **canonical tier** → the slot key (`sanitize`);
3. **search strip** — `recase`, each `TitleRules` / `ExtraTitleRules` rule that fired with its id and before → after,
   the format re-peel → the search query (`TitleNormalizer.searchQuery`);
4. **listing shape** — `NonMovieEventClassifier` (mystery incl. `isBareSneak`, non-screening, live, broadcast),
   `MultiFilmBill`, `ListingShape` (billsSeveral, quotes, stage work, house, relays, silent, programme slot), the
   pieces, and `billedBeside` against a film titled as each search candidate;
5. **searches** — the identity model's title and IMDb queries (`CandidateQueries`), `SearchTitles.candidates`;
6. **findings** — what `SearchQueryMarkersSpec` and `CinemaSlotInvariantsSpec` (title) would raise, before allowlists.

```
3. search strip — the lookup query (TitleNormalizer.searchQuery)
  recase (banner-aware): 'Pokaz przyjazny osobom z ADHD: Kandydaci śmierci' (unchanged)
    xtra-pp-przyjazny-audience [ExtraTitleRules, programmePrefix]: 'Pokaz przyjazny … ADHD: Kandydaci śmierci' → 'Kandydaci śmierci'
  = search query 'Kandydaci śmierci'
```

The listing's take needs its candidates and families: that is `scripts/identity-slice`, not this.

## venue-replay

`scripts/venue-replay <cc> <venue> [--recording RUN] [--ref A [--ref B]] [--title S]... [--fields] [--fixtures DIR]...
[--scrape [--date D]] [--prod]` — one venue's listings as each ref's code reads them, from a recording (default the newest
green; the cache identity-slice uses), never the network. Per listing: raw → clean title, search query, formats, year,
runtime, director, synopsis head, feed ids, detail URL, showtimes, where the detail fields came from (and `RE-READ … was`
for each field a re-read page moved), the CinemaSlotInvariantsSpec / SearchQueryMarkersSpec findings (their allowlists,
in e2e, may excuse one) and a `title-trace` line for a title a rule moved. Two refs: both, then every listing and field
that moved. One JVM per ref; ~8 s warm, ~14 s for two refs, ~2 min the first time a ref compiles.

```
- Trzy pożegnania   [search: Trzy pożegnania]                                     (d880f62cc~1)
    formats - · year - · runtime - · director Isabel Coixet · … · detail page re-read (client parseDetail)
    synopsis Scenariusz: Enrico Audenino, Isabel CoixetP
    CHECK CinemaSlotInvariants synopsis-is-credits: Scenariusz: Enrico Audenino, Isabel CoixetP
== d880f62cc~1 → d880f62cc
~ Trzy pożegnania
    runtime: '-' → '120'
    synopsis: 'Scenariusz: Enrico Audenino, Isabel CoixetP' → 'Po rozstaniu z ukochanym Marta, …'
```

What a recording can and cannot replay — it is prod's `cinema_scrapes` (the clients' PARSED output) plus the detail
pages the identity sweep read, never a scraper's listing pages:

- **Every venue**: its recorded row, read by the ref's code — slots, title rules, format peel, search query, checks.
  A title-rule or slot change shows; a listing-parser change does NOT (the row is what prod's parser made of it).
- **Detail pages, re-read by the ref's parser**: every `DetailEnricher` whose pages the recording holds (PL ~30
  clients, UK Cineworld + Flicks, US Alamo + Flicks; DE and ES record none), and a client with its own
  `parseDetail(html, url)` (Teatr Ziemi Rybnickiej) when `--fixtures` holds the page.
- **`--scrape`** re-runs `fetch()` itself, clock on `--date` — only over a tree holding the venue's listing pages: a
  client's checked-in fixture tree (`test/resources/fixtures/kinoheld` for Filmwerk Gütersloh, `--date 2026-10-11`).
- A request no tree holds FAILS (exit 3), named, whatever the client did with it — `--title` narrows to what you hold.

Code or data: both refs read ONE recording, so what moves is the code's; whether the venue's data moved between two
recordings is `scripts/recording-diff`. Cases pinned in `scripts.VenueReplaySpec` (`test/resources/fixtures/venue-replay/`).

## label-lookup

`scripts/label-lookup <cc> --title T [--venue V]` or `--film tmdb:N` (`--no-prod` skips the mirror, `--no-history`
each row's last-changing commit) — Python, no JVM, ~1 s. Reads the one judgements
file, `test/resources/fixtures/identity-judgements.tsv` (each row: verdict, state, scope, reason, judged, evidence),
and `expected-matches.tsv`, and prints:

- with `--venue`, the **effective verdict** per film, read as every gate reads it (`identity_judgements.label_of`: a
  `*` row covers the venue, the venue's own row overrides it), and a NOTE when two TMDB films come out
  right at one venue (Lalka at Kino Iluzjon: the 1968 film by its own label, the 2026 one by `*`'s);
- every **entry** with its verdict, note and the commit + date that last changed its line (`git blame`);
- **prod's take now** from the local prod mirror's `identity_traces` (`--no-prod` skips it), its freshness by
  `scripts/local-mirror/freshness`, each take judged by the labels — flagged when prod takes a film labelled WRONG, or
  misses one labelled RIGHT (UK "RBO: Romeo & Juliet" at Filmhouse Newlyn: labelled and expected tmdb:1702789, prod
  `none (Vetoed)`);
- the **identity judgements lint**'s findings that touch it (`scripts/ci/identity_judgements_lint.py`'s own rules),
  and the policy's named exceptions (`accepted-right-loss`) among its rows.

### The judgements file

Every gate reads it — the ratchet, the suspect gate, `MatchedTakesLabelSpec`, the takes check, the lint,
`label-lookup`, `identity-judge-cards`, `identity-slice --emit-case`. `expected-matches.tsv` beside it alone is
regenerated by the ratchet, never edited by hand. A new WRONG judgement is fixed or made known-wrong (with why) in the
same commit. The pre-push hook runs the lint (`judgements`), and the lint's own test runs it in CI.

ONE row per (country, venue or `*`, raw title normalised, film); columns `country venue rawTitle film verdict state
scope filmTitle reason judged evidence` (documented in `scripts/ci/identity_judgements.py`; the Scala reader is
`tools.IdentityJudgements`, both held to `identity-judgements-normalise.jsonl`). To record a judgement:

- right / wrong: a row, `state` `open`. A venue's row overrides the `*` row.
- a WRONG take that is served and has no safe fix: `state` `known-wrong`, `scope` the gates it stands in (`capture`:
  the unmatched-cluster ratchet; `served`: a leg's suspect gate; `rows`: MatchedTakesLabelSpec; `hard-clusters`: an
  expected-hard-clusters file), `reason` why. Covers every venue billing the title. A scope no take needs any more fails
  as stale (capture, rows, hard-clusters) or is named GONE (served): strike it.
- a loss the takes check should accept: `state` `accepted-loss` with `reason`; of a take judged RIGHT only as
  `accepted-right-loss` (the lint names each on every run). A move to another film is accepted by judging that film
  right, or the old one wrong.
- `undecided` rows are the suspects standing since the suspect gate was armed (2026-10-13): never add one.

The lint (`python3 scripts/ci/identity_judgements_lint.py`, a second) fails a second row for one key — a listing judged
both ways, or a `merge=union` merge (`.gitattributes`) that kept both sides of one edited row — keys not normalised, a
word outside the vocabulary, and everything the old lint did.

A title is matched exactly (single-spaced) when any file holds it so, else as a part (case and accents aside:
`--title Emil` finds "Emil und die Detektive").

## recording-diff

`scripts/recording-diff <runA> <runB> [--country cc]... [--venue S] [--title S] [--limit N] [--corpus-only]` — two
"Record scrape fixtures" recordings (identity-slice's cache), per country, older first. ~3 s a country on the corpus;
the first comparison over a tree reads it once (~15 s UK) and keeps its manifest. Prints:

- listings added / lost, **RE-SPELLED** (a lost and an added title at one venue differing only by whitespace, case,
  accents, punctuation or truncation — each kind named) and **RE-TITLED** (the very same showtimes under another
  title: a feed's film name replacing the venue's);
- **FIELDS** lost / gained / changed on listings in both (synopsis, runtime, year, director, cast, originalTitle,
  `feed:<id>`), the venues moving most; **DETAIL PAGES** vanished or appeared per venue;
- **ORIGINS** whose refusal share moved (`scripts/ci/recording_health.py` over both trees);
- **MARKER SPELLINGS** new and gone (sneak, preview, pokaz, double bill, certificates);
- `--title`: each matching listing in both runs — "in both, spelled alike, fields alike" means the input did not
  move, so a take that moved between the runs moved by the code.

```
== DE  38498856157 → 38542338732  (venues matching «filmwerk»)
NEW DATA: 2 re-titled listings; 1 new marker spellings
RE-TITLED (the same showtimes under another title):
  Filmwerk Gutersloh: «Sneak_nov + Filmquiz» → «Sneak Over»
MARKER SPELLINGS NEW in 38542338732:
  sneak        «sneak over»  e.g. Filmwerk Gutersloh: «Sneak Over»
```

Measured 2026-10-14: 38363428661 → 38498856157 names «Pokaz przyjazny osobom z ADHD» a new `pokaz przyjazny` spelling
(PL), shows Cineworld's detail unchanged (UK, 94 % → 94 %, no origin moved) and Lieblingsmenschen's double space at
Astor billed alike in both; 38498856157 → 38542338732 shows that double space collapsed (RE-SPELLED, whitespace) and
the sneak re-titled to kinoheld's «Sneak Over».

## listing-history

`scripts/listing-history <cc> [--venue S] --title S [--since 7d] [--replay [--recording RUN] [--replay-limit N]]` —
one listing's take over time, ~15 s without `--replay`:

- **TAKES** per listing: the run of films with the moment and deploy of each change (listings of one billing moved
  alike collapse into one line); **FLIPS within deploy X** when a deploy changed it more than once (a flip per pod
  start: "The Ties That Bind Us" goes OwnMatch 1460982 → Corrected 1096642 within 30 s of every UK pod start);
- **DEPLOY** blocks: the worker image tags Prometheus saw (`kube_pod_container_info`) → commit and subject, each with
  its take-change lines (VictoriaLogs JSON fields; pre-10-14 text lines parsed from the message): kind, before → after,
  basis, confidence, projection mode and id, pod, reason;
- **LABELS** of every spelling the listing went by (`scripts/label-lookup --no-prod`);
- **PROD NOW** from the local mirror (read-only, after `scripts/local-mirror/freshness` and its lag): the
  identity_traces trace, the model family's decision, identity_agreements' verdict, identity_pins, the scraped listing;
- `--replay`: `scripts/identity-slice --key … --ref <deploy commit>` at every deploy that logged a change and at the
  one before it, all on ONE recording, in two views — **CI's** (`--families none`: no filed answers, as every hermetic
  leg) and **prod's** (its filed family answers) — each **REPRODUCED** (the take moves between the two deploys as prod's
  did) or **NOT REPRODUCED**. Reproduced only with filed answers = the CI-vs-prod gap; reproduced in neither = prod's
  own state (restored model, arrival order). One ref compiles at a time (~2 min a new one). Measured: CBeebies Panto
  at Cineworld Ashford on 9c8170f, recording 38363428661 — CI: NOT REPRODUCED (ca4059b takes none too); filed answers:
  REPRODUCED (ca4059b tmdb 1710881 → none), prod's 213 lost takes CI never saw.
  `LISTING_HISTORY_SLICE=<other checkout>/scripts/identity-slice` replays with another identity-slice — needed while
  this checkout's tool files no longer compile at the deployed refs (from 83b869548, `FiledAnswers` reads
  `Capture.live`, which refs before it lack: every ref before it fails to compile).
  Where the local prod mirror is readable a third view replays with **prod's state now** (`--prod-mirror`).
  `--replay-limit N` (default 6) bounds how many deploys are replayed.

## identity-slice --explain

`scripts/identity-explain <cc> …` is its alias (prod's take-change `repro` lines printed it until a355c464e).

Prints, per matching cluster: the billing (title, clean/search title, year, runtime, directors, catalogue ids)
and venue page facts, listings billed alike collapsed; the **captured** model decision; the cluster
**re-resolved by the code** (family keys and why, the chain into its family, every candidate with its
signals, each acceptance rule's refusal, the blocker); the **agreement stage** by the code (basis, fill rule,
guards passed, signals); the take(s) and their verdict in the judgements.

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
basis, ids, was, explanation) that the ratchet (`scripts.UnmatchedClustersRatchet`, and its spec) writes and the capture workflow uploads.
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
  The drafts are `identity-judgements.tsv` rows; it never touches the file itself: confirm a draft row, then copy it in.
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
`projection_id` + `projection_mode` (whole / periodic / changed) + `projection_trigger` (`deploy`: a pod's first
projection, `periodic`: the hourly reconcile or the period's, `scrape` / `answer` / `film-change`: what asked for one of
what moved, joined by `+` when several did, `retry`: an unsettled one run again). Lines logged before 2026-10-14 are text
only and have none of these; `projection_trigger` and the take lines' ids below from the 2026-10-22 builds on.

- **`identity take change: <kind> …`** — one per listing whose TMDB film moved (up to 2,000 a tick): `kind`
  (lost/changed/gained), `venue`, `listing`, `film_before`/`film_after`, `tmdb_before`/`tmdb_after`,
  `title_before`/`title_after`, `basis`, `confidence`, `reason_first`/`reason_last`, `rules` (accept:/pooled:/
  veto:/join:/apart:/refused: ids), `blocker`, `runner_up`, `fallback`, `unanswered`, `previous_basis`/
  `previous_confidence`/`previous_reason` (the take before; absent after an index rebuild), and — ids and numbers, so
  "why did this listing change" is one query — `stage` (`model`, `relay`, `pin`, `agreement`, `poster`, `broadcast`,
  `fill`, `catalogue`, `correction`, `event`), `decided_by` (the one rule: `accept:sole-work`, `pooled:<rule>`,
  `veto:<id>`, a blocker such as `search:found-nothing` / `rule:<why>`, `agreed:imdb+wiki`, `fill:<rule>`,
  `correction:families+poster`, `withdrawal:filmweb`, `poster:vote`, `pin`), `title_rules` (`title:<title-rules.tsv
  id>` and `format:<tag>` the title took), `score` (the node's own probability of the film it weighed), `runner_up_tmdb`
  / `runner_up_score` / `margin` (the best other film it weighed, and score minus its score), `trigger` (the
  projection's, or `review` when a pin decided either side), `previous_decided_by` / `previous_runner_up_tmdb` and
  `runner_up_won` (`true`: the film taken now is the one the take before beat), `listing_year`/
  `listing_runtime`/`listing_director`/`listing_original_title`, and `repro` — a ready
  `scripts/identity-slice <cc> --key 'venue|raw title' --ref <build>` line (any listing, matched or not; lines logged before
  2026-10-14 name `scripts/identity-explain`, now an alias of `identity-slice --explain`, which finds the listing only if
  the capture holds it unmatched).
- **`identity take change: summary …`** — one per tick that moved any: `take_changes`, `lost`, `changed`,
  `gained`, `logged`, `by_basis`.
- **`identity projection: …`** — every written tick: `listings`, `films`, `written`, `retired`, `declined`,
  `take_changes`, `seconds`, `scoped`, `retired_films`, `declined_films` (first 50 ids).
- **`identity suspect takes: counted …`** — `suspected` and `suspect_<signal>` per signal, under the
  projection's id.
- **`identity rule firings: counted …`** — per written whole-corpus projection: `listings_counted`, `rules_fired`,
  `rules_<kind>` (title, accept, veto, …). The per-rule numbers are Prometheus counters, not log fields:
  `kinowo_worker_identity_rule_fires_total{country, rule}` (live listings each rule id fired on, added up per counted
  projection) over `kinowo_worker_identity_rule_counted_listings_total{country}` — Grafana "Identity rules" panels
  (worker-diagnostics). A rule's share: `sum by (rule) (increase(kinowo_worker_identity_rule_fires_total[1d])) /
  scalar(sum(increase(kinowo_worker_identity_rule_counted_listings_total[1d])))`. Dead in prod vs dead in the
  recordings, per title-rules.tsv row: `scripts/rule-firing-prod [--window 7d] [--country cc] [--all-kinds]`.

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
| Why did this listing change — one line | `_time:7d kind:* venue:="Kino Muza" listing:="Lalka" \| fields _time, kind, tmdb_before, tmdb_after, stage, decided_by, previous_decided_by, score, runner_up_tmdb, runner_up_score, margin, trigger, title_rules, build, repro` |
| All changes caused by rule X (as the deciding rule) | `_time:7d kind:* decided_by:="accept:sole-work" \| stats by (country, kind) count()` |
| …by rule X anywhere in the trace (vetoes, cannot-links, refusals) | `_time:7d kind:* rules:"apart:learned-listing-listing-probability-below-the-cannot-link-cut" \| stats by (country, decided_by) count()` |
| …by a title rule | `_time:7d kind:* title_rules:"title:search-programme-prefix" \| fields _time, country, venue, listing, kind, decided_by` |
| Listings where the runner-up won after deploy Y | `_time:3d build:=<sha> kind:changed runner_up_won:true \| fields _time, country, venue, listing, tmdb_before, tmdb_after, previous_decided_by, decided_by, margin` |
| Which stages moved takes after a deploy, and by what | `_time:24h build:=<sha> trigger:deploy kind:* \| stats by (country, stage, decided_by) count() n \| sort by (n desc)` |
| Near-misses: changes decided by a thin margin | `_time:24h kind:* margin:<0.05 \| fields _time, country, venue, listing, decided_by, score, runner_up_tmdb, runner_up_score` |
| What a scrape vs a filed answer moved | `_time:24h kind:* \| stats by (country, trigger, stage) count()` |
| Tick sizes over a day | `_time:24h "identity take change: summary" \| fields _time, country, take_changes, lost, changed, gained, logged, by_basis` |
| Retired / declined films | `_time:24h "identity projection:" retired:>0 \| fields _time, country, retired, retired_films, declined_films` |
| Suspect-take counts | `_time:24h "identity suspect takes: counted" \| fields _time, country, suspected, suspect_fragment, suspect_era, suspect_runtime` |

`\|` is the table's escape — type `|`. A field missing from a line (`tmdb_after` on a `lost`) is absent, not
empty; `kind:*` keeps only take-change lines (the summary has no `kind`).

## What they replaced (measured 2026-10-13)

"Why did DE CineStar's «Herr der Ringe - Rückkehr des Königs (ext.)» take the 2025 short, and does 3e5a58e16 fix
it?" — without the tools: fetch recording 38363428661's DE tree (370 MB, 5 GB unpacked) 189 s, lay out and compile
the old commit fresh and run an ad-hoc spec 142 s — 331 s of mechanics for ONE ref, before writing the spec or
reading the answer. With them: `scripts/identity-slice de --explain --venue cinestar --title "(ext.)" --fixture
origin/chore/identity-capture --ref 3e5a58e16~1 --ref 3e5a58e16` — 186 s cold for both refs, 37 s warm, and the
answer is on screen: `filled by venues.current … guards passed: …, title.anothersOwn …` before, no film after.

Judge cards for PR #230's 262 UNJUDGED takes (158 cards): ~40 s warm.
