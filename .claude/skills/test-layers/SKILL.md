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
  `sbt web/Test/test` / `worker/Test/test` / `common/Test/test` / `identity/Test/test` (the resolver) /
  `testkit/Test/test` / `e2e/Test/test` to scope to one module.
  Unit specs for controllers, services, enrichment, clients, models.
- **`sbt itAll`** (or `sbt web/IntegrationTest/test` /
  `worker/IntegrationTest/test`) — `*/src/it/scala/` specs that wire
  fakes + the real cache/repository. Locally run it as
  **`scripts/it-local.sh`** (any sbt command as the argument, default
  `itAll`): it starts a mongod of the run's own on a free port and
  removes it afterwards. On the shared `:28017` server other agents'
  heavy runs (identity shadows, convergence legs) saturate its write
  tickets and journal, and light suites fail on 10 s Mongo round trips —
  seven did on 2026-10-12, all green on a private server under the same
  machine load. If you do use `:28017`, give the run a `MONGODB_DB` no
  other live run holds: `IntegrationRunClaim` refuses a second live run
  on one prefix by name.
- **`sbt web/PageTest/test`** — `web/src/page/scala/views/PageJsBehaviourSpec`
  drives real Chrome over CDP against Twirl-rendered fixtures. Covers
  `web/src/main/assets/js/`, the inline `<script>` blocks in
  `web/src/main/twirl/views/repertoire.scala.html` /
  `_repertoireView.scala.html`, and the rendered HTML shape those JS
  blocks read.
- **`cd page-tests-playwright && npx playwright test [--project …]`**
  — mobile + desktop × Chromium / WebKit / Firefox / Edge. `--project`
  narrows to one engine; the default `--list` shows which exist.
- **iOS LocalServer** — `sbt 'web/PageTest/runMain tools.FixtureServerMain
  <port-file>'` in one shell, `KINOWO_LOCAL_URL=http://127.0.0.1:$(cat
  <port-file>) swift test --package-path ios --filter LocalServer`
  in another. Decodes the live `/{city}/api/repertoire`, `/api/details`
  and `/api/cinemas` JSON through the app's own `Codable` models with the
  stores' bare `JSONDecoder()` — the same line `RepertoireStore` /
  `DetailsStore` run. (The app has no HTML path any more; a Twirl
  template change cannot break it, a `MovieController` JSON change can.)
- **`swift test --package-path ios`** — iOS unit / integration suites
  without the live server. Quicker; required for any change to iOS
  model / parser logic regardless of whether you also need LocalServer.
- **`scripts/run-script-tests`** — every script self-test (`<tool>-test.sh`,
  `<tool>_test.py` under scripts/, .github/, android/scripts, ios/scripts,
  infra/bin), found by name and run in parallel; ci.yml runs it whole.
  `scripts/run-script-tests scripts/ci identity-slice` narrows by path or
  name, `--changed origin/main` runs only the tests whose test, tool, a
  file they name, or a library they source/import (transitively) changed
  (the pre-push hook's `scripttests`), `--list --all` shows the
  `# script-test: <tag>` ones kept out of the default run. `--bash /bin/bash`
  runs them under macOS's bash 3.2, the floor the tools promise: the pre-push
  hook does so on a Mac for the tests a push selects, and the nightly Order
  independence `swift` job for all of them.

## Per-country convergence suites (e2e, `@CountryScoped`)

`GermanyConvergenceSpec` and its PL/UK/ES/US siblings (plus the `*SampleConvergenceSpec`
fast legs) run in CI's convergence workflows, never in `testUnit`. Their inputs are not
in git: each replays the PAIR `Record scrape fixtures` pinned — the country's scrape
corpus (artifact `scrape-fixtures-<code>` of the pinned recording run, 14-day retention)
and its enrichment tree plus fills (release `convergence-fixtures`, pinned by
`hermetic-<code>.txt`). One command restores the pair and runs the full leg hermetically,
exactly as CI does:

```
scripts/convergence-local.sh de               # or pl / uk / es / us
scripts/convergence-local.sh de convergenceGermanySample   # any alias, same pair
```

It reads `TMDB_API_KEY` (required) and `OMDB_API_KEY` (optional) from the environment or,
inline, from the Touch ID vault (`secrets get movies TMDB_API_KEY`) — and never the
vault's `MONGODB_URI`, which is production's. Mongo: `MONGODB_URI` if set, else the
itAll mongod on `:28017`, else a throwaway `mongo:8.3.11` on `:27117`; every run's
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
identity tool's ref tree; APFS clones), so the first compile is incremental. A watchdog stops the
server when the worktree is removed or after 30 idle minutes (`KINOWO_SBT_WARM_IDLE_MINUTES`);
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
