# Project conventions for Claude

Skills (`.claude/skills/`) hold the mechanics and load on demand: `test-layers`, `regenerate-snapshots`,
`agent-ops` (worktrees, landing, waiting, sbt-warm, downtime), `identity-investigation`, `record-client-fixtures`,
`writing-fakes`, `external-api-rate-limits`. Every other document is indexed in `docs/README.md`; the incidents
behind the rules below are in `docs/agent-workflow-history.md`.

## Run the relevant test layers locally before claiming work done

When a change touches what a layer exercises, run it in the same task, before reporting the work done. Run **all**
matching layers, **in parallel** when independent (separate `Bash` calls in one message), in the main session — do
**not** split test runs into subagents, so the output and follow-up edits stay in one context. The only acceptable
reason to skip a layer is a missing local toolchain (e.g. no full Xcode for `xcodebuild test`); say so explicitly.
CI is the safety net, not the test plan: land once the layers are green locally, never push to let CI find out.

| Layer | Required for |
|---|---|
| `sbt testUnit` | any change under `web/`, `worker/`, `scrapers/`, `common/`, `geodata/` or `identity/` `src/main/` that isn't pure view markup (controllers, services, enrichment, the cinema scrapers, models, the generated rosters, the identity resolver) |
| `sbt itAll` (`*/src/it/scala/`, fakes + real cache/repository) | enrichment pipelines, cache layering, the read-model projection, anything crossing the `MovieService` ↔ `MovieRepository` ↔ `MovieCache` seam |
| `sbt web/PageTest/test` (real Chrome over CDP, Twirl fixtures) | *recommended, not required before landing* (below): `web/src/main/assets/js/`, the inline `<script>` blocks in the repertoire templates, the rendered HTML shape those blocks read |
| Playwright (`page-tests-playwright`; mobile + desktop × Chromium / Chrome / WebKit in CI, Firefox local-only) | *recommended, not required before landing* (below): visible UX changes — card-tap, pill rows, gestures, the empty / loading states |
| iOS LocalServer (iOS decoders `[Film]`, `[FilmDetails]`, `CinemaCatalog` against a live fixture-server `/{city}/api/*`) | either side of that contract: the `MovieApiController` JSON shape or the iOS `Codable` models |
| `swift test --package-path ios` | any iOS model / parser logic, whether or not LocalServer is also needed |
| `scripts/run-script-tests` (every `<tool>-test.sh` / `<tool>_test.py`, found by name — a new self-test needs no wiring) | any change under `scripts/`, `.github/scripts/`, `.github/actions/` or the mobile `*/scripts/`; `--changed <base>` narrows, `--linux` runs CI's ubuntu container |

**The page layers and e2e run after landing, on Main** (the owner, 2026-11-17, "B1": in a week of landing checks the
page area failed 0 of 14, ~23 min each, and `e2e (rest)` 0 of 95). Neither the pre-push hook nor the landing check runs
them, and landing does not require them; Main's `page-tests-*` and `e2e (*)` rows are gate rows, so a failure there is
a normal gate red with a fixer. When you change UI, still run the page layer you touched locally as cheap insurance
(`scripts/page-tests-local`, `web/PageTest/test`) -- and the commit gate's fail-before/pass-after test for a visible
change is still a PageTest or Playwright test you run yourself.

Exact commands, module scoping and `testOnly` / `-z` narrowing: `test-layers` skill. Run sbt through
`scripts/sbt-warm` (`agent-ops` skill).

**The pre-push hook** (`scripts/hooks/pre-push`; install once per clone with `scripts/install-hooks.sh`) runs, in
seconds, the checks that otherwise fail CI, selected by the paths the push changes; its header lists every check.
It complements the layers above and replaces none of them. Dry-run it with `scripts/hooks/pre-push --range A..B`.
Step by step, with every refusal and what to do: `docs/landing-process.md`.
Bypasses — `PREPUSH_SKIP="<checks>"`, `git push --no-verify` — must be named in your report.
A check's local tool missing (sbt or a working java, shellcheck, actionlint, npm, python3 / timezonefinder, gh, curl,
jq) BLOCKS the push with the fix, never a skip: for sbt, `. "$HOME/.sdkman/bin/sdkman-init.sh"` (or put
`~/.sdkman/candidates/{sbt,java}/current/bin` on PATH) before `git push`. Only what lies outside the machine's install
warns and passes: Docker not running. The GitOps manifests
(`infra/bin/fetch-gitops`, from GitHub) failing to fetch BLOCKS too: the checkout left behind is stale.

**Red builds gate every push to main.** While the newest COMPLETED run on main of Main, Identity model convergence or
Identity takes -- exactly those three, the owner's gate (2026-10-27; the takes rows split into Identity takes,
2026-11-08, which every convergence run's preflight dispatches) -- is red (running, queued and cancelled runs say nothing, nor an incomplete run red only in aggregates like
Identity takes' `verdict complete`: a run whose takes rows did not all decide, agentctl's NO VERDICT; the drift explorer's replay runs under its own workflow, never a gate run), a push lands only as a fix for it: every pushed
commit touching a production path (`*/src/main/`, `conf/`, `build.sbt`, `project/`, `infra/nix/`, `Dockerfile`)
carries a `Fixes-Red: <run>` trailer naming any red run of the current episode (failed since the last green, failing the same way). Tools-only commits (scripts, docs, `.claude/` skills,
`*/src/test|it|fixtures/`, `test/resources/`, `.github/`) land while red without one. The hook enforces it;
`PREPUSH_SKIP=redgate` bypasses — say so. A gate gh cannot read refuses a production push.
**Every trailer** (`Fixes-Red:`, `Coupling:`, `Complexity:`, `Length:`, `Pure-Move:`, ...) **counts only in the message's final block, beside `Co-Authored-By:` with no blank line between**; anywhere else git ignores it (pre-push's `scripts/ci/trailers.py lint` warns).
The gates judge only what a commit's code changed; a recording's own data
findings go to the non-gating Identity findings report (history: `docs/agent-workflow-history.md`). The nightly
recording's capture (Record scrape fixtures) and its hermetic replay (Verify scrape recording) do not gate either;
`agentctl ci-why` lists their reds as information (`docs/design/convergence-fixture-fill.md`, "Capture, then verify").
**A new check gates only once it has been green**: every job, runner label and gate workflow THE GATE reads has a row in
`.github/gating-checks.tsv` with the green run that proved it; a new one lands as `unproven` (owner and why) and only
reports until then -- a run red only in unproven jobs holds no push. After its first green run on main:
`scripts/ci/gating_checks.py prove --workflow-file <f> --run <run id> --number <#n>`, and commit the registry.

## Regenerate the snapshots your change shifts

Three checked-in snapshot layers; a change that shifts one is not committable until it is regenerated and committed
alongside (commands: `regenerate-snapshots` skill):

- **`expected-*.html`** (`PageSnapshotSpec`, `/` per city) — shifts on any Twirl template, markup,
  `data-*`/CSS-class, `PosterProxy` or inline-JS change (comments inside inline `<style>`/`<script>` are stripped by
  `tools.Minify` and don't shift it; HTML `<!-- -->` comments do).
- **`expected-schedules.txt`** (`FilmScheduleEndToEndSpec`) — the whole-corpus assertion, every film's fields; the
  layer that names WHICH films moved, usually the first to fail.
- **`read-model-snapshot.json`** (same spec) — a cache the page-test servers boot from; stale is slow, never wrong.

The last two shift on any scraper, enrichment, `TitleNormalizer`, identity-resolver, `ReadModelProjector`,
model-field or raw-fixture change — which usually shifts the HTML too, so regenerate all three.

## Tests: the commit gate

**Do not commit a change unless a test confirms the behaviour was not there before and is there now** — it fails
before the change and passes after. No such test, not committable. `scripts/fail-before -- <test command>` proves it
(the commit's test-side files against its parent must fail, then at the commit must pass).

- **"Test" means any layer that reaches the behaviour:** unit, `IntegrationTest`, PageTest, Playwright, iOS
  LocalServer / emulator. "It's UI, I can't unit-test it" means drop to PageTest / Playwright / the emulator and
  assert there (e.g. pixel-sample the rendered result). A visible UX change without a browser-or-emulator test has
  not met the gate.
- Every new or modified behaviour gets its test — non-negotiable for bug fixes, new methods or branches,
  parsing/normalisation, and any logic deciding what to persist or display. "I ran it once and it looked right" is
  not a substitute.
- **Write the failing test first** whenever feasible (fixes, features, parser changes, "why is this value wrong"
  investigations) and watch it fail for the right reason: it proves the test exercises the path (a green-from-the-start
  test often asserts nothing), the bug is real, and the fix hits the root cause. For a suspected parser/enrichment/data
  bug, a test feeding the suspect input through the real code is both the diagnosis and the regression test.
- **The only exception** is behaviour no harness can reach (e.g. sub-16px input focus-zoom, unreproducible in
  Playwright WebKit and the Simulator): test the closest reachable mechanism (the maximum-scale toggle) and say in the
  commit message / report that the behaviour itself is unreachable and why. "Observable only via a running
  server/browser" is not that exception.
- Exploratory spikes may skip test-first but owe the test before commit. Extend a neighbouring test rather than
  inventing a new style. Pure logic: in-memory unit tests. Composed services: the existing
  `*/src/test/scala/services/...` patterns wiring fakes / in-memory implementations.
- **External-service clients** (TMDB, IMDb, Cinemeta, OMDb, Filmweb, Metacritic, RT, scraped sites) are tested
  against a real captured response replayed from disk — never live HTTP in tests, never hand-written mock JSON
  (`record-client-fixtures` skill).

## Investigate every red; never rerun to green

A red test or tool error is investigated to its root cause; a flaky test is fixed, never rerun until it passes — a
test that passes on a second run is not healthy, it is flaky, and a rerun is never a fix.

- **Read a run's exit status, not a filtered tail of its output.** A test runner piped into `grep`/`tail`/`head` exits
  with the filter's status, and `head` cuts the failure summary off: write the run to a log and read the log
  (`scripts/claude-hooks/masked-exit-guard` denies the pipe).
- First classify it. A real assertion failure names an expectation, value or location ("expected X, got Y at line
  N"). An environment-shaped error says the runner could not start, the loader tripped on something fine a minute
  ago, or the filesystem disagrees with what was just written (`No tests found`, `EBUSY`, `ENOENT` on a file you just
  wrote, a module-resolution error in a known-working setup, Playwright's `did not expect test.describe() to be called
  here`).
- For an environment-shaped error, don't iterate on variations of the command ("maybe file paths", "maybe `--list`")
  — each is a new shape of the same noise. Find the suspect state (`test-results/`, `node_modules/.cache/`, stale
  `target/`, a stray process of yours — `scripts/agent-procs kill --mine`, never `pkill`), clear it, and run the
  original command once to learn which it is. Reproduces: a real bug to chase. Doesn't: find and fix the race or
  shared state that made it intermittent, then commit that fix.

## Worktrees: don't work in the root checkout, never strand work, no PRs

Many agents work this repository at once (mechanics and tools: `agent-ops` skill).

- **Work in your own worktree, never the root checkout** (`/Users/pawel/projects/movies`, `main`), which is for
  orchestration/inspection only and must end every session clean. Worktrees may be sibling dirs or
  `.claude/worktrees/`. Enforced by `scripts/claude-hooks/root-checkout-guard` and `scripts/hooks/pre-commit`.
- **No PRs, and never push a feature branch** — the only ref that reaches origin is `main`. The sole exceptions
  are eight bot PRs: decoration-discovery, identity-refit, identity-capture (a PR only when its ratchet is
  clean), discover-cinemas, retire-venues, Renovate's Android Gradle bumps (weekly, `renovate.yml`), nix-update
  (weekly, one PR on `nix-update/all`) and, from the same workflow, movies-gitops' image-pin bumps (weekly, one PR
  there on `image-pins/all`); an agent may push judged verdicts to that bot's `chore/identity-capture`
  branch (`identity-investigation` skill); and `scripts/mobile-ship.sh` pushes only the release commit it cut in its
  own detached worktree.
- **Landing:** all relevant layers green → `~/bin/agentctl land` from your own worktree (it rebases; the hook runs
  the basic tests and the touched area's tests locally; no pre-merge CI) → remove the worktree and delete your own
  merged branch with `git branch -d` (refuses an unmerged branch; never `-D`).
- **Subagents** land with `agentctl land` in the background and act on its watch verdict (`WATCH RED` → fix forward);
  they never wait on a branch CI run.
- **Never finish or yield with a dirty tree you own:** commit, or `git stash push -u -m "<what + why>"`. An
  interrupted task still owes a checkpoint (`wip: <state>` commit on your own branch, or a labelled stash).
- **Don't strand work on a stale base:** behind main → rebase before piling on.
- **Touch only what's yours:** dirty files you didn't create are a co-agent's WIP — surface them; never
  `git add -A`, commit, stash or discard them.
- **Never `pkill -f` / `killall` by pattern** — use `scripts/agent-procs kill --mine`.
- Wait on long runs with `scripts/wait-for` and on CI with `scripts/ci-wait`, in the background, acting on the
  notification — never a foreground sleep/poll loop (`scripts/claude-hooks/foreground-wait-guard` denies a foreground
  wait of over a minute), never an unbounded background one (`unbounded-poll-guard` denies it and prints the
  `scripts/wait-for <file> --contains <text>` fix). `scripts/agent-procs` audits worktrees and orphaned watchers; `scripts/agent-time`
  shows where agents' time went.

## Agent tooling: `~/bin/agentctl` (laptop only; reference `~/bin/AGENTCTL.md`)

The whole path from commit to main — `land`, every pre-push step, the area tests, the gate, each refusal and what
to do — is in `docs/landing-process.md`.
**A refactoring lane follows ONE checklist**, "Landing a refactor" at the top of that doc (pure moves, what-if,
narrowed layers, keep working while queued) instead of re-deriving it from the sections below.

**No pre-merge CI** (the owner, 2026-10-26): "only run the basic tests before merging and merge on green (main not
green branch); if some specific area (browser / page, identity) is touched, run tests specific to that locally (unless
it takes longer than 15 mins), then merge to main and test everything there". So every change — identity included —
lands with `agentctl land`: the pre-push hook runs the basic tests (path-selected lints, generated artefacts, every
test config compiled, the snapshot and deploy specs) and, when the push touches an AREA, that area's local tests
(`scripts/hooks/local-tests.txt`: `identity`, under 15 min warm -- the `page` area moved to Main on 2026-11-17, above; the identity set narrowed to the specs and
countries the change can reach, `scripts/ci/identity_impact.py`, proved by `scripts/identity-impact-check`). Merge on green means MAIN green: the
red gate below; while main is red, a production change waits on the one local `waiting` branch (`land` prints
`WAITING <sha>`, keeps running, and lands it with the others in one push once main is green). Nothing waits for a branch CI run; `scripts/identity-landing-check` and `identity-measure` are optional
tools for your own confidence, not merge requirements. The merge queue (`agentctl queue`) is retired.


1. `eval "$(~/bin/agentctl env --print)"`: puts sbt and java on PATH. A locked vault prints the `! secrets unlock` line for the user.
2. Work and commit in your own worktree.
3. `~/bin/agentctl land` (Bash `run_in_background=true` when an area's tests will run): waits for the repository's
   landing lock, fetches, rebases, runs the pre-push hook as a dry run (basic + area tests), pushes, verifies. Ends
   `LANDED <sha>` or `NOT LANDED: <why>`.
4. `~/bin/agentctl wait run <id> [--until landable]`: run it in the background, then end your turn. It prints only the verdict.
5. `~/bin/agentctl ci-why`: when main is red or the gate refused. It shows the failed jobs, infra vs test, and how to clear it.

## Commit and push policy

- **Commit at every stable state**, before moving on; each commit a checkpoint you'd happily revert to. Stable: a fix
  with its passing regression test; a feature with golden-path and edge-case tests; a refactor or mechanical sweep
  with build and tests green. Not stable: a production change without its test (the commit gate), half-done work, a
  green compile with skipped or commented-out tests.
- Don't pile unrelated changes into one commit; a phase with internal milestones gets a commit per milestone; a
  genuinely atomic phase (a multi-file rename) may be one big commit.
- Messages give the *why* in one or two sentences — a one-line subject naming the change, an optional short
  paragraph — via a HEREDOC. Never amend a published commit.
- **Auto-land once stable** at the natural end of the asked change — don't ask "want me to commit?"; doubly so for
  a diagnose-and-fix flow, which ends with the fix committed and landed. If you commit but defer landing, say so.
- **Stop and ask only for what can't be undone cheaply:** force pushes / rewriting published history
  (`push --force`, `reset --hard` against upstream, amending anything on origin); destructive ops (dropping a Mongo
  collection, truncating a table, deleting a branch you didn't create or one not merged, `rm -rf` outside
  `target/`); staging anything that might carry a
  secret (credentials, API keys — stage by explicit path); a diff so large a reviewer would balk. Everything else —
  green code, CSS, docs, refactors, pinning a k3s deployment to a known-good image during an incident — just do it.
  If a push's CI fails, fix forward; don't undo the push.
- **Batch pushes to main:** Main's run in flight always finishes and publishes; a newer push queues, and an even newer
  push replaces the queued run (it shows as cancelled). Every run that starts still costs ~20 min of runners:
  push when a coherent change is locally green, not per commit; if your previous push's run is under ~10 minutes
  old, keep committing and push once; judge `main` by the newest completed run.
- **Downtime:** workers may go fully down (≲15 min, just do it); the web tier must not go fully down if there is any
  way not to — and if it was, say so. The one accepted exception (the owner, 2026-11-18): rebooting k3s-worker-1,
  where every web pod runs, is a ~2 min full web outage, taken with `infra/bin/rolling-reboot --accept-outage --only
  k3s-worker-1` (a second node was declined). Policy: `agent-ops` skill.

## Design rules

**Extract repeated patterns at two uses, not three.** A multi-line shape written a second time — a fake repository
inside every spec, a regex + `replaceAll` chain in two parsers, a "load fixture and feed through this client" helper,
a worker-pool scaffold, a `NFD → strip diacritics → ł→l → lowercase` chain, a JSON-LD-block idiom — gets extracted
(a `private[services]` helper, a shared `*/src/test/scala/...` base, a method on the most relevant class). Trivial
inline statements are exempt ("three similar lines beat a premature abstraction"). Put the shared piece where most
callers already see it without new cross-package imports; delete the inline copies in the same commit; name it
after the concept (`InMemoryMovieRepository`, `ProductionLineRegex` — not `FakeRepository`, `parseHelper`).

**SOLID, with Dependency Inversion as the load-bearing one.**

- Every non-trivial collaboration is wired in `AppLoader` (the composition root); everything else receives
  abstractions through constructor parameters — `class FilmwebRatings(tmdb: TmdbClient, filmweb: FilmwebClient)`,
  never constructing collaborators inside. Constructors and parameters take the abstraction, never a concrete class;
  production code never references a test subclass.
- Reaching for a feature flag to toggle behaviour usually means a missing abstraction: a trait with two
  implementations, picked at the composition root.
- Single responsibility: one reason to change per class (`ImdbIdResolver` vs `ImdbRatings`); treat
  `Manager`/`Handler`/`Util`/`Helper`/`Processor` names as a smell.
- Open/closed: a new cinema is a new `CinemaXClient` on the scrape contract (`ScrapeReaper` unchanged); a new rating
  site is a `*Ratings` class on the rating-site port (`SiteRatings.Site`: its find and read; `SiteRatings` unchanged),
  its row columns in `RatingColumns`, plus one `RatingSource` entry in `RatingSources`.
- Liskov: an `InMemoryMovieRepository` honours `MovieRepository`'s write-through contract; callers never need
  `isInstanceOf`.
- Interface segregation: `MovieRepository` stays the persistence contract (`findAll`, `upsert`, `updateIfPresent`,
  `delete`, `enabled`, `close`); a read-only caller gets a `MovieRepositoryReader` sub-trait.
- **Never suffix a class with `Impl`.** Name what makes the implementation distinct (`MongoMovieRepository`,
  `InMemoryMovieRepository`, `HttpFilmwebClient`). With only one implementation and no good name, you don't need the
  trait yet — collapse it until a second appears. Test: would a reader guess from the name what it does?

**Coupling is reviewed on every change, and only falls unless a commit says why.** `scripts/coupling-report` measures
the production code from Zinc's own class-dependency record and, between the classes of one source file (which Zinc
does not record), their class files -- which also take off the file's imports Zinc charges to its last class (`sbt
couplingReport`, `project/Coupling.scala`): the
module matrix, each package's Ca/Ce/instability/abstractness/distance, the package cycles with the class references
behind every edge (`--out DIR` keeps every reference, `edges.tsv`), the top fan-in/fan-out classes and god hubs, split
packages. **The package graph has no cycle** (0 since 2026-11-18, the owner's decision): the pre-push `sbt` check
(`scripts/ci/coupling_ratchet.py`) refuses any package cycle edge, naming the class references behind it, and no
`Coupling:` trailer or baseline row excuses one. `test/resources/coupling-layers.tsv` gives every package its layer
(tools < models < settings < clients/services < controllers < modules/diagnostics) and, inside the services and
composition-root layers, its RANK: a package uses only its own layer and rank or lower ones, so an edge that would close
a cycle is refused first as an upward edge, saying which direction is allowed; a new package takes its row (and rank) in
the commit adding it, and an `allow` row, a moved layer or a changed rank needs the same trailer as a baseline rise. The
ratchet also holds the code to `test/resources/coupling-baseline.tsv`: no new cross-module edge kind, no package's Ce
past its row. **Two limits** (the owner, 2026-11-18): a class's fan-out ≤ 9 and, for a package of 8+ types, its
distance D ≤ 0.20. What was past them then is frozen (`fanout` / `distance` rows, which only fall); a class or package
NEWLY past a limit fails, and neither `--update` nor a trailer admits it -- cut it (a trait at the seam, a leaf value type
down to `models`, a generic helper to `tools`), or add an exemption row to `test/resources/coupling-exemptions.tsv` with a
`Coupling: <why>` trailer. Its categories: the composition roots (`modules.*`, `diagnostics.*`), registries (venue
rosters, scraper catalogues), the typed-config root (`settings.ProcessConfiguration`), the foundation packages (`models`,
`tools`, `settings`, `services.titles`: stable and concrete on purpose), and the declared scrape contract, whose twelve
member types a scraper's fan-out counts as ONE dependency. Prefer cutting the dependency (a trait at the seam, the class
moved to the package that owns it or one rank down); when a Ce rise is right, run `scripts/coupling-report --update` and
commit the baseline with a `Coupling: <why>` trailer. A fall needs no trailer: `--update` records it, so the ratchet
tightens. **A frozen class may be split stepwise** (the owner, 2026-11-18): a commit carrying `Coupling: split of <X>`
(`... into <A>, <B>` when the pieces are ambiguous) with the `--update`d baseline freezes each piece at its own value
when every piece takes over X's code (git sees its file as a rename or copy of X's, or 2+ of X's members moved into it:
an unrelated class claims nothing), each is strictly below X's row, and the UNION of what the pieces and X's remainder
reference (their references to each other aside) lies within X's frozen reference set at the base
(`test/resources/coupling-fanout-refs.tsv`, written by `--update`): the split adds no dependency, though pieces sharing
X's types may sum past its row. The pieces' rows then only fall, and a piece at 9 or less needs none. In that commit
the frozen distance row of a package holding X or a piece, or one their references reach, may rise by 0.02 at most,
re-frozen there; elsewhere D never rises. **That 0.02 is cumulative, not per split** (the owner, 2026-11-19): each
`distance` row carries a fixed ceiling, its `distance-cap` row (its 2026-11-19 value + 0.02); a split stops there, a fall
lowers the ceiling with the row (min(ceiling, row + 0.02)), and nothing raises it -- `--update` keeps it, and over a
conflicted baseline (agentctl's regen) takes the least of every side's. A package at its ceiling frees D before the next
split (a leaf value type down to `models` / `tools`, as 84a5db6d1 did for `services.movies`); `scripts/coupling-report
--check-split` says the headroom left.

**Code quality ratchets**, the same model over the production Scala's text (the `quality` module, scalameta;
`scripts/quality-report`, held by `scripts/ci/quality_ratchet.py` in the pre-push `sbt` check on a production Scala
change): every method's cognitive complexity (Sonar's) ≤ 15, cyclomatic ≤ 10, inner defs ≤ 2
(`test/resources/complexity-baseline.tsv`, trailer `Complexity: <why>`); method ≤ 50 lines, file ≤ 800
(`length-baseline.tsv`, `Length: <why>`); no new clone group of 100+ tokens (PMD CPD) and no group gaining a copy
(`duplication-baseline.tsv`, `Duplication: <why>`). Each baseline freezes what is over a limit today; a new method or file over
one, or a frozen row rising, fails until split or recorded (`scripts/quality-report --update`, committed with the
trailer); a fall needs none. A moved source keeps its rows only when git sees the rename (`git diff -M`) and no value
rises in the move. `scripts/hotspots` (a report, not a gate; monthly as the `quality-hotspots` release's
assets) ranks sources by 90-day relative churn × cognitive complexity and names change-coupled pairs across packages.
A lane's report pastes `scripts/ratchet-diff <base>..<head>` (every changed row, fell / rose / new, `--measure` for
unrecorded falls), never hand-written numbers; a split is checked with `scripts/coupling-report --check-split <X> <piece>...`.
**The naming lint** (`scripts/ci/naming_lint.py`, the same step; the owner's "names must match behaviour", 2026-11-19):
two top-level types of one package named with the same words in another order (`PruneSweep` / `SweepPrune`) refuse the
push -- rename one after what makes it distinct (a pair mid-rename sits in its `ALLOWED_PERMUTATIONS` with an owner); a
NEW type named for an action (`...Apart`, `...Stages`, `...Read(s)`, `...Reading(s)`, `...Builds`, `...Rereads`,
`...Drafts`, `...Traces`, `...Denials`, `...Plans`) with at most one public def is warned about: fold it into the class
whose data it reads, or name the one thing it is.

**Share business logic between real and fake implementations.** They differ only at the infrastructure boundary
(storage, HTTP backend, clock), never in the rules; a fake that re-implements logic lets tests pass while real code
is broken. Read the `writing-fakes` skill before writing any `Fake*` / `Stub*` / `InMemory*`.

## Cleanup and displaced-code audit: a phase of every task

The work is done when the area is at least as clean as you found it and nothing the change made obsolete is left.

- **At the end of every task**, remove: duplication you introduced or noticed (extract it); dead code the change
  made obsolete (fields no longer written, methods whose only caller went, one-shot scripts whose purpose is
  served, unused imports); comments and docs that now lie; intermediate variables / helpers collapsed to one line;
  stale tests and fixtures; naming drift (`EnrichmentCache` after the concept became `MovieCache`).
- **Audit what the change displaced** — walk the call graph: the code path it replaced (a fallback branch, a
  workaround resolver, an `else` now unreachable, a helper whose only caller inlined it); a class now redundant
  (pass-through methods, its only caller or trait gone); a parameter, event type, config flag or default with no
  purpose left; a test for behaviour that no longer exists. Obvious → delete in the same commit. Debatable (would
  ripple beyond scope) → name it out loud: "Heads-up, X is now redundant. Want me to remove it in a follow-up?"
  Silently leaving the dead path is the failure mode.
- **Open with cleanup** when the area demands it: three near-identical branches you'd add a fourth to (refactor
  first), an already-dead field/param/method (remove first), a test that would copy setup from two others (extract
  the helper first).
- The output is the diff: half real change, half "also deleted these dead methods" is the right shape.
- Cleanup reaches beyond the file you'd otherwise touch — every file you read is fair game. Give substantial cleanup
  its own commit (a one-line dead import rides along; a 200-line rename does not). Skip only when it would balloon a
  commit beyond review with no clean split — then say what you saw and didn't do.

## Deploying a NixOS host: `infra/bin/deploy <host>`

Never hand-craft `nix-env --set` / `switch-to-configuration` lines. `infra/bin/deploy <host>`
previews (refuses a checkout behind origin main, uses the `monitoring-1`-style ssh alias, shows
units started/stopped/restarted and whether a reboot is owed); `--apply` switches and verifies.
Agents run the preview only and hand the user the `--apply` command. Details, and the auto-apply
new/routine-unit rules: docs/deploying-nixos-hosts.md.

## Scripts that hit external services

Run per-row work in parallel — a fixed pool of 5–10 workers, halve concurrency on any 429/503, print throughput at
the end. Per-service limits: `external-api-rate-limits` skill.

## Investigating identity, scraper or CI failures

CI runs on GitHub's hosted runners.

**Writing a workflow (WorkflowPlanSpec):** inputs, refs, run ids and skip decisions are evaluated in ONE plan job (a
root: `preflight`/`plan`) and every other job reads `needs.<plan>.outputs.<name>`. A step with its own `timeout-minutes` starts `. scripts/ci/bounded-step.sh`
and an `if: always()` `scripts/ci/bounded-step.sh --reap` step precedes whatever runs after it fails: the runner
abandons a timed-out step's processes, it does not end them.

Before an ad-hoc test or a full convergence leg (10–90 min), use the local tools that answer in seconds —
`scripts/identity-slice` (`--explain`, `--ref A --ref B`, `--bisect`, `--batch`, `--prod-mirror`),
`scripts/title-trace`, `scripts/venue-replay`, `scripts/recording-diff`, `scripts/listing-history`,
`scripts/label-lookup`, `scripts/identity-ratchet-report`, `scripts/identity-judge-cards`,
`scripts/identity-measure` (whole-corpus; optional before landing, main's convergence runs it after),
`scripts/identity-what-if` (a change's model takes over prod's current corpus on the local mirror, base vs head, in minutes;
`scripts/pure-move-check <base>..<head>` first: a pure code move -- files, packages, classes renamed -- owes no what-if,
and its commit says `Pure-Move: verified`),
`scripts/identity-refit`. Which tool
answers which question, flags, speed and data sources: `identity-investigation` skill. **Name these tools in every
subagent brief.**

**Heavy identity work runs on CI, not the laptop** (the user, 2026-10-21): measure with
`scripts/identity-measure --base X --head Y` (pushes a `measure/<user>/…` branch the run deletes itself); any other
heavy sbt command over a cached recording (a resolve dump, an ablation, a simulation, a replay sweep) with
`scripts/identity-ci-run --recording RUN [--countries …] -- '<sbt command>'`. `scripts/identity-measure-local` and other
local heavy runs are a fallback only, for what the CI route cannot do yet, and they take the machine's shared JVM slots
themselves (`scripts/jvm-gate.sh`, `KINOWO_JVM_SLOTS`, behind `agentctl gov`; there is no `heavy-run` wrapper to call --
the name is the old one); never lower the slot count or bypass the gate, and say so in the report if you use a gate escape hatch.
The governor admits by CPU as well as memory (2026-11-18): every sbt-warm command, what-if and slice request is charged
its job's cores (`agentctl gov status` shows them), so a full laptop queues work instead of running it 3-5x slower. A
whole-module `testUnit` / `<module>/test` and `scripts/fail-before` it would hold go to ci-runner's measure lane by
themselves when that is free (`scripts/ci-sbt`; `--ci` / `--local` decide instead; the proof's verdict is each CI run's
own sbt step, a run with none is no verdict). Narrow `testOnly` runs and compiles stay local.

**Optional: `scripts/identity-landing-check`** — no longer a merge requirement (2026-10-26: the hook's `identity` area
tests run before the merge, main's CI runs the rest after). For your own confidence on a risky identity change: ONE
verdict, `LANDABLE` or `BLOCKED` with the failing spec, its test and a hint. It runs the measure (`identity-measure --footprint`, falling back to the full measure; `--full` for the landing gate,
`--footprint-only` while iterating) AND, in parallel on CI (one `identity-ci-run` per group; `--local` = sbt-warm, which takes
a JVM slot), exactly the identity specs Main's rows run and the measure never does: every `services.identity.*`
(MatchedTakesLabel, UnmatchedClustersRatchet, IdentitySliceCases), the title-rule golden/examples/mutation specs, the
worker `scripts.*` re-resolves, DocCommentPlacement, the hard-cluster ratchet and convergence, and -- when Main's scope
runs the e2e rows -- FilmScheduleEndToEnd, SearchQueryMarkers, IdentityCutoverEndToEnd. On 2026-10-22 at least six Main
reds were changes `identity-measure` alone had called LANDABLE. Nothing requires its LANDABLE; the hook never runs it.

**Measuring cheaply** (2026-10-22: measurements averaged 37 min queued + 66 min wall for ~12 min of compute):
- **Let the footprint pick:** without `--countries`, identity-measure measures only the countries the change can move
  (a `services/cinemas/<cc>/` file, a `<cc>` capture, a title-words row's language; shared resolver code reaches all),
  over ONE recording (the pinned pair). It measures the other recordings itself only when that one is LANDABLE and
  takes moved in 2+ countries (cross-country); `--all-recordings` when you need them all at once, `--plan` to see what
  it would run. Don't hand-run three recordings by default.
- **Dispatch and keep working:** `--no-wait`, then `--run <id>` later; it collects as soon as the takes rows end, not
  the run's tail. Never sit idle on a measurement.
- **A repeat request answers from the result cache:** every verdict is stored on the `convergence-fixtures` release
  (`measure-result-<key>.json`, key = head, base, recording, countries, measure mode and the measure tooling's hash),
  so asking the same measurement again -- from any agent or clone -- prints `CACHED verdict of run <id>` in seconds,
  no dispatch (a pinned-pair verdict for 24 h). `--no-cache` forces a run. A `--no-wait` measurement is stored when
  its `--run` collects it.
- **Don't re-measure after an unrelated rebase:** a rebase onto a main that moved only outside the take paths
  (`--take-paths`) is answered by the earlier run (REUSED, no dispatch). Never `--fresh` for that.
- **Batch pushes** (below): each push to main is a ~20-minute run on the same runners the measurements wait for.
- **A title-rule change: `--footprint` first** (it takes a JVM slot itself: it compiles the head's title rules). When the
  change touches only `title-rules.tsv`, `services/titles/rules/` (not `TitleRulePlaceholders.scala`) or
  `services/titles/TitleNormalizer.scala`, it replays every title reading the base's stored resolve made under your rules, in ~1
  minute and no dispatch: all alike is `measure: footprint` LANDABLE, no take can move (a rule refactor, a merge of
  rows that fold alike). Rely on it while iterating; for a whole-corpus verdict use the full measure (no
  `--footprint`) -- optional before landing (2026-10-26), main's convergence runs it after. Anything else -- a matcher, scoring or BillingParse change, no stored trace, a reading that reads
  otherwise (listed: takes may move) -- falls back to the full measure by itself, saying why.
- **The convergence `takes` rows' skip gate** (`scripts/ci/takes-skip-gate.sh`, CI's side of the same proof): a takes row
  may leave out its takes diff only when ALL hold -- every changed file a title-rule file (as above) or inert (docs/,
  .claude/, Markdown); no identity/matcher/resolver/projection path; the trace tooling unchanged; the base's trace stored
  under its EXACT inputs (code, recording, tree, fills, capture: `takes-tracex-<cc>-<key>`); and the proof ending
  `0 read otherwise, 0 could not be made again`. Anything else -- an error, a timeout, a missing trace, an unknown file
  -- is RUN. It is in **SHADOW** (`takes-skip: shadow`, the default): every row still runs in full, and the gate's answer
  is posted as the status `takes skip gate (<cc>)`; a SKIP where a take moved fails the row as unsound. Enforce
  (`takes-skip: enforce`) only once `scripts/ci/takes-skip-shadow-report.sh` says READY (>= 20 SKIPs, 0 disagreeing);
  even then the shape and drift checks run, 1 in 10 SKIPs is sampled in full, a row whose country had no full takes row
  on main for 24 h runs in full, and `convergence-daily.yml` dispatches a full run every day. Changing the gate:
  `takes-skip-gate-test.sh` and its mutation test (`takes-skip-gate-mutation-test.sh`: every check's removal must fail it).

- **The drift gate fails only on what the change adds** (the owner, 2026-11-04). Each `takes-drift-1|2` row replays
  the fixed seeds of `test/resources/fixtures/drift-gate-seeds.tsv` at the head and at its base; a flip only the head
  shows fails, one both show is `pre-existing (backlog)`. Every known flip is a row of
  `test/resources/fixtures/identity-flip-backlog.tsv` (the work list, with owners); the nightly explorer
  (`identity-drift-explorer.yml`) finds old flips on other seeds and agentctl files each as a backlog row plus a gate
  seed. Divergences always fail. Head and base must replay the same SCENARIO per seed (the arrangement, and the fill
  draw of every question both ask: `scripts/ci/drift_scenario.py`); a mismatch is a HARNESS ERROR (exit 2), never flips.
  Details: `docs/design/identity-no-drift.md`, "The differential gate".
- **The per-family convergence check is a nightly explorer, not a gate** (the owner, 2026-11-06, option A):
  `identity-family-explorer.yml` walks every family's population lattice over the pinned recording (`FamilyConvergence`, one
  row a country shard), outside THE GATE, never red for a commit. As a per-commit gate it cost +36% runner minutes (37 of
  40 identity commits re-walk every family) and prod rarely shows these flips. Its output is release assets, not backlog
  rows: `family-flips-<cc>.tsv`, `family-flips-ranked.tsv` (by deciding rule: structural fix 2's list) and
  `family-flips-new.tsv` (new since last night); the seed drift rows above keep gating. Design: `docs/design/identity-no-drift.md`,
  "The family explorer".
- **A drift or flip red** (the convergence `takes-drift-1|2` rows, `scripts/ci/identity-drift-check.sh`; each row
  replays its fixed seeds, `scripts/ci/drift_seeds.py`, and a red names them with its reproducer
  `IdentityDrift <cc> --seed N`; a red seed is cut into a drift slice: `docs/design/identity-slice.md`) is fixed in
  the incremental STATE that diverges (a lag, a budget, queue order, a cache, a restart key), never by a new resolver
  rule: a rule change is identity design, measured on the full corpus in every country (the owner, 2026-10-29); a
  `Fixes-Red:` commit touching rule code needs `Identity-Design: <doc>`. Known flips: the backlog above. The red is
  not closed until its listing's drift slice is committed (`scripts/identity-slice <cc> --venue V --title T
  --emit-drift-slice`; `IdentityDriftSlicesSpec` replays them all in the unit suite). A change to
  the drift HARNESS (IdentityDrift, IdentityLiveDrift, TakeFlips, identity-drift-check.sh) carries, in the same commit,
  a capture-replay case proving the replay behaves as the worker does (IdentityDriftSpec, CorrectionFlipReplaySpec,
  TakeFlipsSpec, the check's own test), or `Drift-Harness-Refactor: <why>` for a pure refactor. Both enforced by
  pre-push's `driftguards` (`scripts/ci/commit_rules.py`); details: `docs/design/identity-no-drift.md`, "Fixing a drift
  or flip red".
- Read `docs/identity-experiments.md` and the entries in `docs/identity-experiments/` before a resolver change; add an
  entry FILE (`<YYYY-MM-DD>-<slug>.md`) for every experiment you drop or park, its code kept as an `experiment/<name>`
  tag, not a stray branch.
- **Every identity judgement lives in ONE file**, `test/resources/fixtures/identity-judgements.tsv` — one row per
  (country, venue or `*`, raw title, film), edited in place, never a second row for one key; a venue's row overrides
  `*`. Every gate reads it; `expected-matches.tsv` beside it is regenerated by the ratchet, never edited by hand. A new
  WRONG judgement is fixed or made known-wrong (with why) in the same commit; a cured known-wrong scope fails as stale
  until struck; a new suspect is judged, never added undecided. `python3 scripts/ci/identity_judgements_lint.py`
  checks it in a second (columns and states: `scripts/ci/identity_judgements.py`, the skill).
- The local prod mirror (`mongodb://127.0.0.1:28017`, `kinowo*_prod_mirror`) may be read, READ-ONLY, after
  `scripts/local-mirror/freshness <cc>`; never `:27017`, never prod itself.
- Secrets come from the Touch ID vault: `secrets run movies <KEY> -- <command>`.
- **"value X is not a member of object Y" that a clean build doesn't reproduce is stale zinc, not your code.** Zinc
  misses a member added to an object another source wildcard-`export`s (`IdentityMeasures`' `export
  TitleShapeMeasures.*`), in a warm local sbt as much as in a CI build cache restored from an older commit (Main #226).
  `scripts/ci/sbt-invalidate-exporters.sh` deletes the exporters' classes so zinc recompiles them; `sbt-target-cache`
  runs it after every restore. Locally, run it (or `clean`) before rebuilding.
- **"wrong number of arguments at pickleQuotes" with "Ignoring analyses of X due to error in reading TASTy" is stale
  TASTy, not your code**: `-Wsafe-init` read an unchanged dependent's class file before zinc's next cycle recompiled it
  (Main #1397). A batch sbt drops that configuration's classes and runs again by itself (project/StaleTastyRetry.scala);
  in sbt-warm's server run `staleTastyDrop`, then the command again.
