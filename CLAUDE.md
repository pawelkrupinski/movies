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
| `sbt web/PageTest/test` (real Chrome over CDP, Twirl fixtures) | `web/src/main/assets/js/`, the inline `<script>` blocks in the repertoire templates, the rendered HTML shape those blocks read |
| Playwright (`page-tests-playwright`; mobile + desktop × Chromium / Chrome / WebKit in CI, Firefox local-only) | visible UX changes — card-tap, pill rows, gestures, the empty / loading states |
| iOS LocalServer (iOS decoders `[Film]`, `[FilmDetails]`, `CinemaCatalog` against a live fixture-server `/{city}/api/*`) | either side of that contract: the `MovieController` JSON shape or the iOS `Codable` models |
| `swift test --package-path ios` | any iOS model / parser logic, whether or not LocalServer is also needed |
| `scripts/run-script-tests` (every `<tool>-test.sh` / `<tool>_test.py`, found by name — a new self-test needs no wiring) | any change under `scripts/`, `.github/scripts/`, `.github/actions/` or the mobile `*/scripts/`; `--changed <base>` narrows, `--linux` runs CI's ubuntu container |

Exact commands, module scoping and `testOnly` / `-z` narrowing: `test-layers` skill. Run sbt through
`scripts/sbt-warm` (`agent-ops` skill).

**The pre-push hook** (`scripts/hooks/pre-push`; install once per clone with `scripts/install-hooks.sh`) runs, in
seconds, the checks that otherwise fail CI, selected by the paths the push changes; its header lists every check.
It complements the layers above and replaces none of them. Dry-run it with `scripts/hooks/pre-push --range A..B`.
Bypasses — `PREPUSH_SKIP="<checks>"`, `git push --no-verify` — must be named in your report.
A check's local tool missing (sbt or a working java, shellcheck, actionlint, npm, python3 / timezonefinder, gh, curl,
jq) BLOCKS the push with the fix, never a skip: for sbt, `. "$HOME/.sdkman/bin/sdkman-init.sh"` (or put
`~/.sdkman/candidates/{sbt,java}/current/bin` on PATH) before `git push`. Only what lies outside the machine's install
warns and passes: the red gate's API unreachable or no Forgejo token, the GitOps clone, Docker not running.

**Red builds gate every push to main.** While the newest COMPLETED run on main of Main, Identity model convergence
or Record scrape fixtures is red (a cancelled run says nothing), a push lands only as a fix for it: every pushed
commit touching a production path (`*/src/main/`, `conf/`, `build.sbt`, `project/`, `infra/nix/`, `Dockerfile`)
carries a `Fixes-Red: <run id>` trailer naming a red run. Tools-only commits (scripts, docs, `.claude/` skills,
`*/src/test|it|fixtures/`, `test/resources/`, `.github/`) land while red without one. The hook enforces it;
`PREPUSH_SKIP=redgate` bypasses — say so. A push to the Forgejo remote reads Forgejo's runs (`Fixes-Red:` takes the run number in its
URL or the API run id), with `$FORGEJO_API_TOKEN` or the `movies` vault's token while unlocked; otherwise skipped. The gates judge only what a commit's code changed; a recording's own data
findings go to the non-gating Identity findings report (history: `docs/agent-workflow-history.md`).

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
  are five bot workflows: decoration-discovery, identity-refit, identity-capture (a PR only when its ratchet is
  clean), discover-cinemas and retire-venues; an agent may push judged verdicts to that bot's `chore/identity-capture`
  branch (`identity-investigation` skill); and `scripts/mobile-ship.sh` pushes only the release commit it cut in its
  own detached worktree.
- **Landing:** rebase onto main → all relevant layers green → land → remove the worktree and delete your own
  merged branch with `git branch -d` (refuses an unmerged branch; never `-D`). While GitHub is
  suspended, land with `git merge --ff-only <branch>` in the clean root checkout on `main`; when it is back, fetch,
  rebase, and push `<sha>:main` from your own worktree — never `HEAD` mid-rebase.
- **Subagents** commit, rebase and hand back; they land only if every gate is already green, and never wait on CI.
- **Never finish or yield with a dirty tree you own:** commit, or `git stash push -u -m "<what + why>"`. An
  interrupted task still owes a checkpoint (`wip: <state>` commit on your own branch, or a labelled stash).
- **Don't strand work on a stale base:** behind main → rebase before piling on.
- **Touch only what's yours:** dirty files you didn't create are a co-agent's WIP — surface them; never
  `git add -A`, commit, stash or discard them.
- **Never `pkill -f` / `killall` by pattern** — use `scripts/agent-procs kill --mine`.
- Wait on long runs with `scripts/wait-for` and on CI with `scripts/ci-wait`, in the background, acting on the
  notification — never a foreground sleep/poll loop. `scripts/agent-procs` audits worktrees; `scripts/agent-time`
  shows where agents' time went.

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
- **Batch pushes to main:** a newer push cancels Main's run in flight, and a cancelled run says nothing. Push when a
  coherent change is locally green, not per commit; if your previous push's run is under ~10 minutes old, keep
  committing and push once; judge `main` by the newest completed run.
- **Downtime:** workers may go fully down (≲15 min, just do it); the web tier must not go fully down if there is any
  way not to — and if it was, say so. Policy: `agent-ops` skill.

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
  abstractions through constructor parameters — `class FilmwebRatings(cache: MovieCache, client: FilmwebClient)`,
  never constructing collaborators inside. Constructors and parameters take the abstraction, never a concrete class;
  production code never references a test subclass.
- Reaching for a feature flag to toggle behaviour usually means a missing abstraction: a trait with two
  implementations, picked at the composition root.
- Single responsibility: one reason to change per class (`ImdbIdResolver` vs `ImdbRatings`); treat
  `Manager`/`Handler`/`Util`/`Helper`/`Processor` names as a smell.
- Open/closed: a new cinema is a new `CinemaXClient` on the scrape contract (`ScrapeReaper` unchanged); a new rating
  source is a `*Ratings` class plus one `RatingSource` entry in `RatingSources`.
- Liskov: an `InMemoryMovieRepository` honours `MovieRepository`'s write-through contract; callers never need
  `isInstanceOf`.
- Interface segregation: `MovieRepository` stays the persistence contract (`findAll`, `upsert`, `updateIfPresent`,
  `delete`, `enabled`, `close`); a read-only caller gets a `MovieRepositoryReader` sub-trait.
- **Never suffix a class with `Impl`.** Name what makes the implementation distinct (`MongoMovieRepository`,
  `InMemoryMovieRepository`, `HttpFilmwebClient`). With only one implementation and no good name, you don't need the
  trait yet — collapse it until a second appears. Test: would a reader guess from the name what it does?

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

## Scripts that hit external services

Run per-row work in parallel — a fixed pool of 5–10 workers, halve concurrency on any 429/503, print throughput at
the end. Per-service limits: `external-api-rate-limits` skill.

## Investigating identity, scraper or CI failures

CI runs on the self-hosted Forgejo since the GitHub suspension (2026-10-19), and workflows must stay valid on
github.com. Every Forgejo difference these workflows hit and how they were adapted: infra
`docs/forgejo-actions.md` (`~/bitcashier/infra/docs/forgejo-actions.md`).

Before an ad-hoc test or a full convergence leg (10–90 min), use the local tools that answer in seconds —
`scripts/identity-slice` (`--explain`, `--ref A --ref B`, `--bisect`, `--batch`, `--prod-mirror`),
`scripts/title-trace`, `scripts/venue-replay`, `scripts/recording-diff`, `scripts/listing-history`,
`scripts/label-lookup`, `scripts/identity-ratchet-report`, `scripts/identity-judge-cards`,
`scripts/identity-measure` (whole-corpus, before landing any resolver change), `scripts/identity-refit`. Which tool
answers which question, flags, speed and data sources: `identity-investigation` skill. **Name these tools in every
subagent brief.**

**Heavy identity work runs on CI (ci-runner, Forgejo), not the laptop** (the user, 2026-10-21): measure with
`scripts/identity-measure --base X --head Y [--recording RUN] [--countries pl,de]` (pushes a `measure/<user>/…`
branch the run deletes itself; ~25-40 min); any other heavy sbt command over a cached recording (a resolve dump, an
ablation, a simulation, a replay sweep) with `scripts/identity-ci-run --recording RUN [--countries …] -- '<sbt command>'`.
`scripts/identity-measure-local` and other local heavy runs are a fallback only, for what the CI route cannot do yet,
and only under the shared 3-slot heavy-run wrapper; say so in the report.

- Read `docs/identity-experiments.md` before a resolver change; add an entry for every experiment you drop or park,
  its code kept as an `experiment/<name>` tag, not a stray branch.
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
