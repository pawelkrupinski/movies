---
name: test-layers
description: Exact commands for every local test layer (sbt testUnit, itAll, PageTest, Playwright, iOS LocalServer, swift test), their module-scoping variants, and the testOnly/-z narrowing that cuts a run from minutes to seconds. Use when running tests locally before reporting work done, or when a run is slower than it needs to be.
---

# The test layers, in full

Run **all** the layers that match the change. Run them **in parallel**
when there are no dependencies (separate `Bash` tool calls in the same
message), but do **not** split the runs into new subagents — keep
everything in the main session so the test output and any follow-up
edits stay in one context. The only acceptable reason to skip a layer
is that the local toolchain is missing (e.g. no full Xcode for
`xcodebuild test`); say so explicitly when reporting the work.

- **`sbt testUnit`** — every module's unit specs in one run; or
  `sbt web/Test/test` / `worker/Test/test` / `scrapers/Test/test` (the cinema scrapers) / `common/Test/test` / `identity/Test/test` (the resolver) /
  `testkit/Test/test` / `repochecks/Test/test` (the repository-text checks: workflows, scripts, nix and monitoring
  config; package `deploy`) / `quality/Test/test` (the code-quality measures) / `e2e/Test/test` to scope to one module.
  Unit specs for controllers, services, enrichment, clients, models.
  Each unit module runs in its own forked, heap-capped test JVM (`project/ForkedTests.scala`; web's
  fork is Play's), so nothing a run leaves behind stays in the kept sbt server; only `e2eShared`
  runs inside the server (the write-failure tripwire needs it). `testUnit` used to die with "sbt
  server disconnected" on a warm server's second or third run — the unforked specs' leftovers,
  not a flake: investigate, don't rerun.
- **`scripts/unit-changed`** (`--plan` to see it) — between edits, only the unit tests of the modules the change
  touches (committed since origin/main, uncommitted and untracked), in the warm server: the hook's own `--unit-plan`
  selection, minutes where `testUnit` is ~16 warm. The loop, not the gate: the layers above still run before "done".
- **`sbt itAll`** (or `sbt web/IntegrationTest/test` /
  `worker/IntegrationTest/test`) — `*/src/it/scala/` specs that wire
  fakes + the real cache/repository. Locally run it as
  **`scripts/it-local.sh`** (any sbt command as the argument, default
  `itAll`): it starts a mongod of the run's own on a free port and
  removes it afterwards. Its sbt never builds while the worktree's
  kept sbt-warm server does (`sbt-warm --exclusive`): it waits for the
  server's running command, and server calls wait for it — "waits for it
  to end" in either log is that, not a hang. On the shared `:28017` server other agents'
  heavy runs (identity shadows, convergence legs) saturate its write
  tickets and journal, and light suites fail on 10 s Mongo round trips —
  seven did on 2026-10-12, all green on a private server under the same
  machine load. If you do use `:28017`, give the run a `MONGODB_DB` no
  other live run holds: `IntegrationRunClaim` refuses a second live run
  on one prefix by name.
- **Live-site canaries are NOT in `itAll`.** An `it` spec that talks to a real third-party site (Metacritic, Rotten
  Tomatoes, IMDb, TMDB, Filmweb) carries `@LiveSites` (`worker/src/it/java/integration/LiveSites.java`); build.sbt's
  `it` config excludes that tag, so `itAll`, Main's integration rows, the pre-push `itrest` and the landing check never
  select it, and a site changing its answer blocks no landing. `LIVE_SITES=1 sbt itLive` runs only those specs (the
  nightly `live-sites.yml`, which opens an issue on a red run and gates nothing; TMDB_API_KEY for the enrichment spec).
  `LiveSitesTaggedSpec` (unit) fails a spec naming `LiveUpstream.Probes` without the tag.
- **`sbt web/PageTest/test`** — `web/src/page/scala/views/PageJsBehaviourSpec`
  drives real Chrome over CDP against Twirl-rendered fixtures. Covers
  `web/src/main/assets/js/`, the inline `<script>` blocks in
  `web/src/main/twirl/views/repertoire.scala.html` /
  `_repertoireView.scala.html`, and the rendered HTML shape those JS
  blocks read.
- **`cd page-tests-playwright && npx playwright test [--project …]`**
  — mobile + desktop × Chromium / Chrome / WebKit (CI runs these; the
  Firefox projects stay in `playwright.config.ts` for local runs only).
  `--project` narrows to one; `--list` shows which exist.
- **Neither page layer gates landing** (the owner, 2026-11-17, "B1"): the pre-push hook and the landing check no
  longer run them, nor `e2e (rest)`; Main runs all three after landing, as gate rows. For a UI change run them locally
  anyway (`scripts/page-tests-local [chromium|webkit|all]` boots the fixture server and runs CI's Playwright rows).
- **iOS LocalServer** — `scripts/sbt-warm --serve 'web/PageTest/runMain
  tools.FixtureServerMain <port-file>'` in the background (`--serve`: keeps
  the checkout's sbt server, and so its one JVM slot, in use until stopped —
  stop it when done), `KINOWO_LOCAL_URL=http://127.0.0.1:$(cat
  <port-file>) swift test --package-path ios --filter LocalServer`
  in another. Decodes the live `/{city}/api/repertoire`, `/api/details`
  and `/api/cinemas` JSON through the app's own `Codable` models with the
  stores' bare `JSONDecoder()` — the same line `RepertoireStore` /
  `DetailsStore` run. (The app has no HTML path any more; a Twirl
  template change cannot break it, a `MovieApiController` JSON change can.)
- **Any `xcodebuild test` / `simctl boot`** boots a Simulator and leaves it booted (three, for days, put the laptop in
  swap on 2026-11-07): run it as `scripts/ios-simulators run -- xcodebuild test ...`, which shuts down what the command
  booted on any exit. `scripts/ios-simulators sweep` shuts down agents' Simulators (name `agent-`/`red<N>-`/`fixer<N>-`,
  or booted through `run`) up over 2 h; the owner's hand-booted one is never touched.
- **`swift test --package-path ios`** — iOS unit / integration suites
  without the live server. Quicker; required for any change to iOS
  model / parser logic regardless of whether you also need LocalServer.
- **`scripts/run-script-tests`** — every script self-test (`<tool>-test.sh`,
  `<tool>_test.py` under scripts/, .github/, android/scripts, ios/scripts,
  infra/bin), found by name and run in parallel; ci.yml runs it whole.
  `scripts/run-script-tests scripts/ci identity-slice` narrows by path or
  name, `--changed origin/main` runs only the tests whose test, tool, a
  file they name, or a library they source/import (transitively) changed,
  or a path matching a glob the test declares it reads (a repo-wide lint:
  `# script-test-reads: .github/workflows/*` in its first 40 lines)
  (the pre-push hook's `scripttests`), `--list --all` shows the
  `# script-test: <tag>` ones kept out of the default run. `--bash /bin/bash`
  runs them under macOS's bash 3.2, the floor the tools promise: with
  `--changed` it is the floor's own scope (the bash the change touches parsed
  by it, its added lines held to 3.2, those files' own self-tests re-run), the
  pre-push hook's floor run; the nightly Order independence `swift` job runs
  all of them under it.
  `--linux` runs them in CI's ubuntu-26.04 container (Docker / Rancher
  Desktop; the pre-push hook runs that leg beside the native one when Docker
  is up). A new self-test needs no wiring: name it by the scheme and CI runs it.

- **`scripts/quality-report`** — the code-quality ratchets (CLAUDE.md, "Code quality ratchets") over the
  production Scala, in the warm server (~3 s): the methods over each limit and the worst ones, then each ratchet's
  verdict against its baseline; the pre-push hook runs the same on any production Scala change. A method you made more
  complex than its row (or a new one over a limit) fails it: split it, or `--update` and commit the baseline with its
  trailer. `--update` after a cleanup records the falls (no trailer). A hand-written literal table is no row: declare
  it in `test/resources/quality-data-tables.tsv` and the measure checks it is data. A measure change runs `quality/Test/test`
  (Sonar's published examples) and `scripts/run-script-tests quality` (the ratchet and its mutation test).
  `scripts/hotspots` ranks where to clean first (churn × complexity, change-coupled pairs): a report, not a layer.
  `scripts/js-quality-report` takes the same measures (and fan-out, cycles, distance) over the JS/TS, per area
  (`scripts/js-quality/areas.tsv`; `--root DIR --area NAME=PATH` for another checkout): a report, no baseline, no gate.

## Refactoring safety tools (on demand; reports, never gates)

None runs in `testUnit` or the pre-push hook, and none instruments, mutates or recompiles the caller's checkout: the
touched-code pair measures in a throwaway worktree at the range's head (`scripts/lib/touched_tree.py`), the scalafix pair in
a kept worktree with SemanticDB on (`scripts/lib/scalafix_tree.py`). Each prints its own `--help`.

- **`scripts/coverage-touched <base>..<head>`** (scoverage, ~1.5 min on a three-file scraper lane): each production
  method the range touches, with its statement coverage by the specs that reach it (`touchedSpecs`: the method's own
  source's specs and its callers' specs), the ones under 80% first. Before a refactor: write the pinning tests for
  every method it lists.
- **`scripts/mutation-check <base>..<head>`** (Stryker4s, ~3 min on the same lane): mutates only the touched methods'
  sources, runs only the reaching specs, and lists each touched method's killed and SURVIVING mutants -- a survivor is
  a behaviour change no test would catch (2026-11-20: a "behaviour-preserving" scraper lane widened two venues' genre
  parsing, tests green; the check on that fix found 10 survivors in KinoGramClient.movieOf at 89.7% coverage).
  `--max-specs` (default 60) refuses a range whose helper every spec reaches: scope it tighter.
- **`scripts/identity-boot-cost [--update]`** (~2 min): bytes and time a family for a cold resolve, a take-up
  re-resolving every family, and one with nothing moved, over the pinned identity slices in a JVM without escape
  analysis; fails a phase whose bytes a family rose over the baseline's tolerance (2%:
  `test/resources/identity-boot-cost-baseline.tsv`). Run it on any change to the resolver's hot paths or the take-up.
- **`scripts/scalafix-move <from.pkg.Name> <to.pkg.Name>`**: moves a class (and companion) to another package,
  every reference re-pointed from SemanticDB (`scripts/scalafix-rules/MoveSymbol.scala`), every configuration
  compiled before the result is staged in the checkout. Use it instead of hand-editing imports; then
  `scripts/pure-move-check`, as any move.
- **`scripts/scalafix-organize [--check] [--main]`**: OrganizeImports (and RemoveUnused) over the sources
  (`.scalafix.conf`): java/javax, scala, everything else, each import's own selectors kept. After a move or merge
  that left imports out of order.
- **`scripts/dead-code`** (~2 min warm): production classes and methods nothing calls, from Zinc's dependencies and
  used names (entry points -- routes, configuration, the AOT list, a `main`, library overrides -- excluded), ranked:
  unreferenced first (delete, in a cleanup commit), then test-only (a seam on purpose, or a displaced caller).

## Per-country convergence suites (e2e, `@CountryScoped`)

`GermanyConvergenceSpec` and its PL/UK/ES/US siblings (plus the `*SampleConvergenceSpec`
fast legs) run in CI's convergence workflows, never in `testUnit`. Their inputs are not
in git: each replays the PAIR `Record scrape fixtures` pinned — the country's scrape
corpus (artifact `scrape-fixtures-<code>` of the pinned recording run, 14-day retention)
and its enrichment tree plus fills (release `convergence-fixtures`, pinned by
`hermetic-<code>.txt`). One command restores the pair and runs the full leg hermetically,
exactly as CI does. It refuses without `--i-need-a-local-leg`: for which takes a change moves use
`scripts/identity-slice` / `scripts/identity-measure` instead (seconds / ~8 min, not tens of minutes):

```
scripts/convergence-local.sh --i-need-a-local-leg de      # or pl / uk / es / us
scripts/convergence-local.sh --i-need-a-local-leg de convergenceGermanySample   # any alias, same pair
```

It reads `TMDB_API_KEY` (required) and `OMDB_API_KEY` (optional) from the environment or,
inline, from the Touch ID vault (`secrets get movies TMDB_API_KEY`) — and never the
vault's `MONGODB_URI`, which is production's. Mongo: `MONGODB_URI` if set, else the
itAll mongod on `:28017`, else a throwaway `mongo:<production release>` on `:27117`; every run's
databases sit under a unique `MONGODB_DB` prefix and the suite drops them.

By hand, the same thing:

```
scripts/restore-country-corpus.sh de          # or all; fails loudly if no pair is pinned/left
TMDB_API_KEY="$(secrets get movies TMDB_API_KEY)" \
  MONGODB_URI="mongodb://127.0.0.1:28017/?directConnection=true" MONGODB_DB=kinowo_it_conv_de1 \
  KINOWO_COUNTRY=de KINOWO_COUNTRIES=de \
  KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES=enrichment-de KINOWO_CONVERGENCE_HERMETIC=true \
  sbt convergenceGermany
```

The restore overwrites the still-tracked July `cinema-scrapes-{pl,de}.json.gz` /
`prod-coverage-{pl,de}.json`; `git checkout -- test/resources/fixtures/corpus` before
committing. Everything else it writes is gitignored. A pair expired from the release
or its artifact gone: `gh workflow run "Record scrape fixtures" --ref main` (~10 min).

## A refactor's equivalence phase: `tools.Equivalence`

A rewrite that must change no output (a rule table, a parser split) keeps a frozen copy of the old code in the test tree
and compares the two over recorded inputs -- captures, drift slices, fixtures -- with testkit's
`tools.Equivalence.assertEquivalent(inputs, frozen, current, minimum, show)`: each side's reading rendered as lines (its
outputs and trace strings), every input's lines byte-identical. Before comparing, it breaks each side's reading of the
first input and must catch it, and it refuses one function handed twice, a corpus below `minimum` and a reading that is
the same for every input. It is compiled into common's and identity's tests too (build.sbt's `sharedTestDoubles`);
`ProdTitlesNormalizationSpec` uses it. The frozen copy and its spec go once the rewrite has landed (99723fb1b).

## Run the narrowest scope you can

`sbt web/PageTest/test` runs the page specs in ~30 s. `sbt 'web/PageTest/testOnly
views.PageJsBehaviourSpec -- -z "card poster link"'` runs 4 in ~6 s.
When iterating on one test, use `testOnly` + the `-z` substring
filter. Same for the main `sbt testUnit` — there are hundreds of unit
specs; `testOnly` to the spec under change cuts a full run from
minutes to seconds.

Note the `web/` project prefix is required — a bare
`PageTest/testOnly …` fails to parse.

## Keep sbt warm: `scripts/sbt-warm`

A plain `sbt -batch` pays a JVM start and, in a fresh worktree, a cold compile on every call.
`scripts/sbt-warm "<command>" ["<command>"...]` runs the commands in ONE sbt server kept per
checkout (`sbt --client`), and seeds a fresh worktree's `project/target` and `*/target/scala-*`
from the newest local build of a main commit (another worktree's, the root checkout's, or an
identity tool's ref tree; APFS clones), so the first compile is incremental. The server holds one
of the machine's JVM slots for its life (its keeper's; commands in it take none), and its keeper
stops it when the worktree is removed or after 10 idle minutes (`KINOWO_SBT_WARM_IDLE_MINUTES`),
never under a running command;
`--status`, `--stop`, `--reap`, `--seed`. The pre-push hook runs its sbt check through the kept
server too (3–5 s instead of a ~55 s sbt JVM). Don't mix it with a plain `sbt` in the same
checkout while the server lives: two compilers over one target/ delete each other's classes.

Measured 2026-10-14, `worker/testOnly services.identity.UnmatchedClustersRatchetSpec` in a fresh
tree: plain `sbt -batch` 232 s (cold compile of four modules); `scripts/sbt-warm` seeded from a main
build ~50 s on the first call (7 s server start, one changed file compiled, the spec's own ~38 s),
41 s after.

**Fast iteration:** while iterating, one spec — `scripts/sbt-warm "worker/testOnly X -- -z 'case'"`;
once before pushing, the whole suite the change touches (for the identity resolver:
`scripts/sbt-warm identity/Test/test`), then the layers above.
